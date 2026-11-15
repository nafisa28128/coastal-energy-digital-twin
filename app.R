# ============================================================
# CLIMATE-ADAPTIVE HYBRID RENEWABLE ENERGY DIGITAL TWIN
# Coastal Bangladesh | Interactive R Shiny application
# ============================================================

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)

# Climate multipliers are exploratory stress-test assumptions,
# not calibrated climate projections.
scenario_table <- data.frame(
  Scenario = c("Baseline", "Heat stress", "Extreme weather",
               "Prolonged cloud cover", "Combined climate stress"),
  solar = c(1.00, 0.96, 0.85, 0.60, 0.70),
  wind = c(1.00, 1.00, 0.75, 1.00, 0.70),
  tidal = c(1.00, 1.00, 0.80, 1.00, 0.80),
  demand = c(1.00, 1.10, 1.00, 1.00, 1.10),
  stringsAsFactors = FALSE
)

required_columns <- c(
  "timestamp", "pv_power_kw", "wind_power_kw", "tidal_power_kw",
  "total_load_demand_kw", "medical_cold_chain_load_kw",
  "grid_availability", "ambient_temperature_c", "heat_index_c",
  "extreme_weather_flag"
)

parse_timestamp <- function(x) {
  z <- suppressWarnings(ymd_hms(x, tz = "Asia/Dhaka", quiet = TRUE))
  if (all(is.na(z))) z <- suppressWarnings(ymd_hm(x, tz = "Asia/Dhaka", quiet = TRUE))
  if (all(is.na(z))) z <- suppressWarnings(as.POSIXct(x, tz = "Asia/Dhaka"))
  z
}

read_energy_data <- function(file) {
  validate(need(!is.null(file), "Upload your hourly energy CSV to begin."))
  d <- read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  names(d) <- tolower(trimws(names(d)))
  missing <- setdiff(required_columns, names(d))
  validate(need(length(missing) == 0,
                paste("Missing required columns:", paste(missing, collapse = ", "))))
  d$timestamp <- parse_timestamp(d$timestamp)
  validate(need(!anyNA(d$timestamp), "The timestamp column could not be parsed."))
  numeric_cols <- setdiff(required_columns, "timestamp")
  for (nm in numeric_cols) d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
  validate(need(all(vapply(d[numeric_cols], function(x) all(is.finite(x)), logical(1))),
                "Numeric columns contain missing or non-finite values."))
  d <- d %>% arrange(timestamp)
  validate(need(nrow(d) >= 2 && !anyDuplicated(d$timestamp),
                "The dataset needs at least two unique hourly records."))
  validate(need(all(diff(d$timestamp) > 0), "Timestamps must be in ascending order."))
  validate(need(all(d$grid_availability %in% c(0, 1)),
                "grid_availability must contain only 0 and 1."))
  validate(need(all(d$extreme_weather_flag %in% c(0, 1)),
                "extreme_weather_flag must contain only 0 and 1."))
  validate(need(all(d$total_load_demand_kw >= 0) &&
                  all(d$medical_cold_chain_load_kw >= 0),
                "Demand values cannot be negative."))
  validate(need(all(d$medical_cold_chain_load_kw <= d$total_load_demand_kw + 1e-6),
                "Medical cold-chain demand exceeds total demand in at least one row."))
  d
}

simulate_twin <- function(d, scenario, battery_capacity, initial_soc,
                          max_battery_kw, grid_limit_kw, min_soc,
                          efficiency, solar_multiplier, wind_multiplier,
                          tidal_multiplier, demand_multiplier,
                          healthcare_priority) {
  s <- scenario_table[scenario_table$Scenario == scenario, , drop = FALSE]
  n <- nrow(d)
  intervals <- as.numeric(diff(d$timestamp), units = "hours")
  dt <- c(intervals, median(intervals))
  validate(need(all(dt > 0) && all(is.finite(dt)), "Time intervals must be positive."))

  heat_scenario <- scenario %in% c("Heat stress", "Combined climate stress")
  extreme_scenario <- scenario %in% c("Extreme weather", "Combined climate stress")
  cloud_scenario <- scenario == "Prolonged cloud cover"

  pv_factor <- rep(s$solar, n)
  if (heat_scenario) {
    pv_factor <- pv_factor * pmax(0, 1 - 0.004 * pmax(d$ambient_temperature_c - 25, 0))
  }
  wind_factor <- rep(s$wind, n)
  tidal_factor <- rep(s$tidal, n)
  if (extreme_scenario) {
    event <- d$extreme_weather_flag == 1
    pv_factor[event] <- pv_factor[event] * 0.85
    wind_factor[event] <- wind_factor[event] * 0.75
    tidal_factor[event] <- tidal_factor[event] * 0.80
  }
  if (cloud_scenario) pv_factor <- pv_factor * 0.60

  pv <- d$pv_power_kw * pv_factor * solar_multiplier
  wind <- d$wind_power_kw * wind_factor * wind_multiplier
  tidal <- d$tidal_power_kw * tidal_factor * tidal_multiplier
  generation <- pv + wind + tidal

  heat_demand_factor <- rep(1, n)
  if (heat_scenario) {
    heat_demand_factor <- 1 + 0.005 * pmax(d$heat_index_c - 30, 0)
  }
  demand <- d$total_load_demand_kw * heat_demand_factor * demand_multiplier * s$demand
  critical <- pmin(d$medical_cold_chain_load_kw * heat_demand_factor *
                     demand_multiplier * s$demand, demand)
  noncritical <- pmax(demand - critical, 0)

  # Observed grid availability is retained. During flagged extreme-weather
  # hours, the stress scenarios assume a grid outage as a deterministic test.
  grid_available <- d$grid_availability == 1
  if (scenario %in% c("Extreme weather", "Combined climate stress")) {
    grid_available[d$extreme_weather_flag == 1] <- FALSE
  }

  validate(need(initial_soc >= min_soc,
                "Initial battery SOC must be at least the minimum reserve SOC."))
  energy <- battery_capacity * initial_soc / 100
  reserve <- battery_capacity * min_soc / 100
  charge_eff <- sqrt(efficiency)
  discharge_eff <- sqrt(efficiency)
  out <- vector("list", n)

  for (i in seq_len(n)) {
    # Renewable allocation: healthcare first when priority is enabled.
    if (healthcare_priority) {
      ren_critical <- min(generation[i], critical[i])
      ren_noncritical <- min(max(generation[i] - ren_critical, 0), noncritical[i])
    } else {
      ren_noncritical <- min(generation[i], noncritical[i])
      ren_critical <- min(max(generation[i] - ren_noncritical, 0), critical[i])
    }
    critical_gap <- max(critical[i] - ren_critical, 0)
    noncritical_gap <- max(noncritical[i] - ren_noncritical, 0)
    grid_import <- 0
    grid_critical <- 0
    grid_noncritical <- 0
    batt_charge <- 0
    batt_discharge <- 0
    batt_critical <- 0
    batt_noncritical <- 0

    # Grid serves deficits when available, subject to the entered import limit.
    if (grid_available[i]) {
      grid_import <- min(critical_gap + noncritical_gap, grid_limit_kw)
      if (healthcare_priority) {
        grid_critical <- min(critical_gap, grid_import)
        grid_noncritical <- min(noncritical_gap, max(grid_import - grid_critical, 0))
      } else {
        grid_noncritical <- min(noncritical_gap, grid_import)
        grid_critical <- min(critical_gap, max(grid_import - grid_noncritical, 0))
      }
    }

    # Battery is dispatched for remaining deficits, with critical load first.
    remaining_critical <- max(critical_gap - grid_critical, 0)
    remaining_noncritical <- max(noncritical_gap - grid_noncritical, 0)
    if (remaining_critical + remaining_noncritical > 0) {
      max_output_from_energy <- max(energy - reserve, 0) * discharge_eff / dt[i]
      batt_discharge <- min(remaining_critical + remaining_noncritical,
                            max_battery_kw, max_output_from_energy)
      if (healthcare_priority) {
        batt_critical <- min(remaining_critical, batt_discharge)
        batt_noncritical <- min(remaining_noncritical,
                               max(batt_discharge - batt_critical, 0))
      } else {
        batt_noncritical <- min(remaining_noncritical, batt_discharge)
        batt_critical <- min(remaining_critical,
                            max(batt_discharge - batt_noncritical, 0))
      }
      energy <- energy - (batt_discharge / discharge_eff) * dt[i]
    }

    # Charge only from renewable surplus.
    surplus <- max(generation[i] - demand[i], 0)
    if (surplus > 0) {
      batt_charge <- min(surplus, max_battery_kw,
                         max((battery_capacity - energy) / (charge_eff * dt[i]), 0))
      energy <- energy + batt_charge * charge_eff * dt[i]
    }
    energy <- min(max(energy, reserve), battery_capacity)

    critical_unmet <- max(critical_gap - grid_critical - batt_critical, 0)
    noncritical_unmet <- max(noncritical_gap - grid_noncritical - batt_noncritical, 0)
    out[[i]] <- data.frame(
      timestamp = d$timestamp[i], ambient_temperature_c = d$ambient_temperature_c[i],
      heat_index_c = d$heat_index_c[i], extreme_weather_flag = d$extreme_weather_flag[i],
      pv_kw = pv[i], wind_kw = wind[i], tidal_kw = tidal[i],
      renewable_generation_kw = generation[i], demand_kw = demand[i],
      critical_demand_kw = critical[i], grid_available = as.integer(grid_available[i]),
      grid_import_kw = grid_import, battery_charge_kw = batt_charge,
      battery_discharge_kw = batt_discharge, battery_SOC_kwh = energy,
      battery_SOC_percent = 100 * energy / battery_capacity,
      critical_unmet_kw = critical_unmet,
      noncritical_unmet_kw = noncritical_unmet,
      total_unmet_kw = critical_unmet + noncritical_unmet,
      curtailed_kw = max(surplus - batt_charge, 0), dt_hours = dt[i]
    )
  }
  bind_rows(out)
}

kpis <- function(r) {
  demand_kwh <- sum(r$demand_kw * r$dt_hours)
  unmet_kwh <- sum(r$total_unmet_kw * r$dt_hours)
  list(
    generation = sum(r$renewable_generation_kw * r$dt_hours),
    unmet = unmet_kwh,
    healthcare = sum(r$critical_unmet_kw * r$dt_hours),
    min_soc = min(r$battery_SOC_percent),
    grid = sum(r$grid_import_kw * r$dt_hours),
    ens = if (demand_kwh > 0) 100 * unmet_kwh / demand_kwh else NA_real_
  )
}

ui <- fluidPage(
  titlePanel("Climate-Adaptive Hybrid Renewable Energy Digital Twin"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload measured hourly dataset", accept = ".csv"),
      helpText("Required columns are listed in the project README."),
      selectInput("scenario", "Climate stress scenario", choices = scenario_table$Scenario),
      sliderInput("solar_multiplier", "Additional PV multiplier", 0.5, 1.5, 1, 0.05),
      sliderInput("wind_multiplier", "Additional wind multiplier", 0.5, 1.5, 1, 0.05),
      sliderInput("tidal_multiplier", "Additional tidal multiplier", 0.5, 1.5, 1, 0.05),
      sliderInput("demand_multiplier", "Additional demand multiplier", 0.5, 1.5, 1, 0.05),
      numericInput("capacity", "Battery capacity (kWh)", 1000, min = 1),
      numericInput("battery_kw", "Maximum battery power (kW)", 250, min = 1),
      sliderInput("initial_soc", "Initial battery SOC (%)", 0, 100, 25),
      sliderInput("min_soc", "Minimum reserve SOC (%)", 0, 50, 0),
      sliderInput("efficiency", "Round-trip battery efficiency", 0.70, 1.00, 0.90, 0.01),
      numericInput("grid_limit", "Grid import limit (kW)", 1000, min = 0),
      checkboxInput("priority", "Prioritise medical cold-chain demand", TRUE),
      downloadButton("download_hourly", "Download hourly results"),
      downloadButton("download_scenarios", "Download scenario comparison")
    ),
    mainPanel(
      uiOutput("status"),
      fluidRow(
        column(4, h4("Renewable generation"), h3(textOutput("k_generation")), "kWh"),
        column(4, h4("Unmet energy"), h3(textOutput("k_unmet")), "kWh"),
        column(4, h4("Healthcare deficit"), h3(textOutput("k_health")), "kWh")
      ),
      fluidRow(
        column(4, h4("Minimum battery SOC"), h3(textOutput("k_soc")), "%"),
        column(4, h4("Grid import"), h3(textOutput("k_grid")), "kWh"),
        column(4, h4("Energy not served"), h3(textOutput("k_ens")), "%")
      ),
      hr(), h4("Renewable generation and demand"), plotlyOutput("p_generation", height = 320),
      h4("Battery state of charge"), plotlyOutput("p_soc", height = 280),
      h4("Healthcare and non-critical unmet load"), plotlyOutput("p_unmet", height = 280),
      h4("Monthly unmet energy"), plotlyOutput("p_monthly", height = 280),
      h4("Climate scenario comparison"), plotlyOutput("p_scenarios", height = 320),
      h4("Scenario performance summary"), tableOutput("scenario_table_out"),
      p("Research note: stress multipliers are exploratory assumptions. The model is a rule-based dispatch simulation, not a calibrated forecast or mathematical optimisation.")
    )
  )
)

server <- function(input, output, session) {
  data_in <- reactive({ read_energy_data(input$file) })

  current_run <- reactive({
    req(data_in())
    simulate_twin(data_in(), input$scenario, input$capacity, input$initial_soc,
                  input$battery_kw, input$grid_limit, input$min_soc,
                  input$efficiency, input$solar_multiplier, input$wind_multiplier,
                  input$tidal_multiplier, input$demand_multiplier, input$priority)
  })

  current_kpis <- reactive({ kpis(current_run()) })
  fmt <- function(x, digits = 1) format(round(x, digits), big.mark = ",", nsmall = digits)

  output$status <- renderUI({
    req(current_kpis())
    k <- current_kpis()
    msg <- if (k$healthcare > 0) "Healthcare-critical unmet demand detected" else if (k$unmet > 0) "Non-critical unmet demand detected" else "No unmet demand in this model run"
    div(strong("Scenario status: "), msg)
  })
  output$k_generation <- renderText({ fmt(current_kpis()$generation) })
  output$k_unmet <- renderText({ fmt(current_kpis()$unmet) })
  output$k_health <- renderText({ fmt(current_kpis()$healthcare) })
  output$k_soc <- renderText({ fmt(current_kpis()$min_soc) })
  output$k_grid <- renderText({ fmt(current_kpis()$grid) })
  output$k_ens <- renderText({ fmt(current_kpis()$ens, 2) })

  output$p_generation <- renderPlotly({
    r <- current_run()
    plot_ly(r, x = ~timestamp) %>%
      add_lines(y = ~renewable_generation_kw, name = "Renewable generation") %>%
      add_lines(y = ~demand_kw, name = "Total demand") %>%
      add_lines(y = ~grid_import_kw, name = "Grid import") %>%
      layout(xaxis = list(title = "Time"), yaxis = list(title = "Power (kW)"), hovermode = "x unified")
  })
  output$p_soc <- renderPlotly({
    r <- current_run()
    plot_ly(r, x = ~timestamp, y = ~battery_SOC_percent, type = "scatter", mode = "lines", name = "Battery SOC") %>%
      layout(xaxis = list(title = "Time"), yaxis = list(title = "SOC (%)", range = c(0, 100)))
  })
  output$p_unmet <- renderPlotly({
    r <- current_run()
    plot_ly(r, x = ~timestamp) %>%
      add_lines(y = ~critical_unmet_kw, name = "Healthcare-critical") %>%
      add_lines(y = ~noncritical_unmet_kw, name = "Non-critical") %>%
      layout(xaxis = list(title = "Time"), yaxis = list(title = "Unmet load (kW)"), hovermode = "x unified")
  })
  output$p_monthly <- renderPlotly({
    r <- current_run() %>% mutate(unmet_kwh = total_unmet_kw * dt_hours,
                                  month = floor_date(timestamp, "month")) %>%
      group_by(month) %>% summarise(unmet_kwh = sum(unmet_kwh), .groups = "drop")
    plot_ly(r, x = ~month, y = ~unmet_kwh, type = "bar") %>%
      layout(xaxis = list(title = "Month"), yaxis = list(title = "Unmet energy (kWh)"))
  })

  scenario_runs <- reactive({
    req(data_in())
    bind_rows(lapply(scenario_table$Scenario, function(s) {
      r <- simulate_twin(data_in(), s, input$capacity, input$initial_soc,
                         input$battery_kw, input$grid_limit, input$min_soc,
                         input$efficiency, input$solar_multiplier, input$wind_multiplier,
                         input$tidal_multiplier, input$demand_multiplier, input$priority)
      k <- kpis(r)
      data.frame(Scenario = s, Unmet_energy_kWh = k$unmet,
                 Healthcare_unmet_kWh = k$healthcare,
                 Energy_not_served_percent = k$ens,
                 Minimum_SOC_percent = k$min_soc)
    }))
  })
  output$p_scenarios <- renderPlotly({
    z <- scenario_runs() %>% pivot_longer(c(Unmet_energy_kWh, Healthcare_unmet_kWh),
                                           names_to = "Indicator", values_to = "Energy_kWh")
    plot_ly(z, x = ~Scenario, y = ~Energy_kWh, color = ~Indicator, type = "bar") %>%
      layout(barmode = "group", xaxis = list(title = "Scenario"), yaxis = list(title = "Unmet energy (kWh)"))
  })
  output$scenario_table_out <- renderTable({ scenario_runs() }, digits = 2)
  output$download_hourly <- downloadHandler(
    filename = function() paste0("digital_twin_", gsub(" ", "_", input$scenario), ".csv"),
    content = function(file) write.csv(current_run(), file, row.names = FALSE)
  )
  output$download_scenarios <- downloadHandler(
    filename = function() "digital_twin_scenario_comparison.csv",
    content = function(file) write.csv(scenario_runs(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
# ============================================================
# CLIMATE-ADAPTIVE HYBRID RENEWABLE ENERGY DIGITAL TWIN
# Coastal Bangladesh | Dashboard
#
# install.packages(c("shiny", "ggplot2", "bslib", "DT"))
# Run: shiny::runApp()
# ============================================================

library(shiny)
library(ggplot2)
library(bslib)
library(DT)

# ---------- Palette ----------
ink    <- "#1E3445"
teal   <- "#3FA98C"
amber  <- "#D9A04A"
red    <- "#D9625E"
blue   <- "#5B8DB8"
violet <- "#8A93B0"
slate  <- "#5B6B7A"
plum   <- "#7B6AA6"

required_columns <- c(
  "timestamp", "ambient_temperature_c", "relative_humidity_percent",
  "heat_index_c", "thermal_stress_index", "solar_ghi_wm2",
  "wind_speed_10m_ms", "tidal_velocity_ms", "extreme_weather_flag",
  "pv_power_kw", "wind_power_kw", "tidal_power_kw", "total_generation_kw",
  "medical_cold_chain_load_kw", "total_load_demand_kw", "grid_availability",
  "bess_soc_percent", "bess_charge_kw", "bess_discharge_kw",
  "unmet_load_kw", "healthcare_outage_risk"
)

candidate_files <- list.files(
  pattern = "^climate_health_hybrid_energy_digital_twin.*\\.csv$",
  ignore.case = TRUE
)
initial_file <- if (length(candidate_files) > 0) candidate_files[1] else NULL

read_energy_data <- function(path) {
  dat <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

  missing_cols <- setdiff(required_columns, names(dat))
  if (length(missing_cols) > 0) {
    stop(paste("The CSV is missing these required columns:",
               paste(missing_cols, collapse = ", ")))
  }

  raw_ts <- dat$timestamp
  parsed <- as.POSIXct(raw_ts, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Dhaka")
  if (any(is.na(parsed))) parsed <- as.POSIXct(raw_ts, tz = "Asia/Dhaka")
  dat$timestamp <- parsed

  for (nm in setdiff(required_columns, "timestamp")) {
    dat[[nm]] <- suppressWarnings(as.numeric(dat[[nm]]))
  }

  dat <- dat[!is.na(dat$timestamp), ]
  dat <- dat[order(dat$timestamp), ]
  dat$date <- as.Date(dat$timestamp, tz = "Asia/Dhaka")

  if (nrow(dat) == 0) stop("No valid timestamp records were found in the CSV.")
  dat
}

# ---------- Helpers ----------
theme_pro <- function() {
  theme_minimal(base_size = 12) +
    theme(
      axis.title       = element_text(colour = slate, size = 10),
      axis.text        = element_text(colour = "#374151"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#E8ECF0"),
      legend.position  = "bottom",
      legend.title     = element_blank()
    )
}

kpi_card <- function(label, value, unit, color) {
  div(class = "kpi", style = paste0("--c:", color),
      div(class = "l", label),
      div(class = "v", value),
      div(class = "u", unit))
}

plot_card <- function(title, id, height = "300px", note = NULL) {
  div(class = "plot-card",
      h5(title),
      plotOutput(id, height = height),
      if (!is.null(note)) div(class = "note", note))
}

agg <- function(dat, cols, FUN = mean) {
  f <- as.formula(paste0("cbind(", paste(cols, collapse = ","), ") ~ period"))
  aggregate(f, data = dat, FUN = FUN, na.rm = TRUE)
}

fmt <- function(x, d = 0) format(round(x, d), big.mark = ",", nsmall = d)

# ---------- Theme + CSS ----------
app_theme <- bs_theme(
  version = 5, primary = ink,
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

css <- "
body { background:#EEF1F5; color:#1F2937; }
.container-fluid { padding:0 !important; }

.hero { background:linear-gradient(135deg,#1E3445 0%,#2A4C61 100%);
        color:#fff; padding:28px 42px 30px; }
.eyebrow { color:#7FC9B3; letter-spacing:.2em; font-size:.74rem; font-weight:700; text-transform:uppercase; }
.hero h1 { font-size:2.1rem; font-weight:700; margin:8px 0 4px; color:#fff; }
.hero .sub { color:#C7D3DD; font-size:1rem; }
.pill { display:inline-block; margin-top:16px; padding:7px 16px; border-radius:999px;
        background:rgba(255,255,255,.12); color:#E5EDF3; font-size:.72rem;
        letter-spacing:.14em; font-weight:700; text-transform:uppercase; }
.pill.ok { background:rgba(63,169,140,.28); color:#A6EBD3; }

.layout { display:grid; grid-template-columns:340px minmax(0,1fr); gap:32px;
          padding:32px 42px; align-items:start; }

.side { background:#233B4D; color:#DCE6EE; border-radius:18px; padding:26px;
        position:sticky; top:16px; }
.side h6 { color:#6FC3AB; letter-spacing:.18em; font-size:.72rem; font-weight:700;
           text-transform:uppercase; margin:0 0 14px; }
.side label, .side .control-label { color:#DCE6EE; font-size:.88rem; font-weight:500; }
.side .form-control, .side .form-select { background:#2D4A5F; border:1px solid #3B5A70;
           color:#EAF1F6; border-radius:10px; }
.side .form-control::placeholder { color:#8FA6B8; }
.side .btn-default, .side .btn-file { background:#33546B; border:1px solid #3B5A70; color:#fff; }
.side hr { border-color:rgba(255,255,255,.12); margin:22px 0; opacity:1; }
.side .hint { font-size:.78rem; color:#9FB3C4; margin-top:8px; }
.status-ok  { color:#A6EBD3; font-weight:600; font-size:.82rem; }
.status-err { color:#FFA7A3; font-weight:600; font-size:.82rem; }

.sec { font-size:.78rem; letter-spacing:.2em; font-weight:700; color:#5B6B7A;
       text-transform:uppercase; margin:34px 0 14px; }
.sec:first-child { margin-top:0; }

.empty { border:1px dashed #C5CDD6; border-radius:16px; text-align:center;
         padding:36px 20px; margin-bottom:8px; }
.empty h4 { font-weight:700; color:#33546B; }
.empty p { color:#6B7280; margin:0; }

.kpi-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:20px; }
.kpi { background:#fff; border-radius:14px; padding:20px 24px 18px; border-top:3px solid var(--c);
       box-shadow:0 2px 8px rgba(15,23,42,.06); }
.kpi .l { font-size:.72rem; letter-spacing:.15em; font-weight:700; color:#6B7280; text-transform:uppercase; }
.kpi .v { font-size:2rem; font-weight:700; color:#1E3445; margin:16px 0 6px; }
.kpi .u { font-size:.85rem; color:#8A94A3; }

.grid2 { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:20px; margin-bottom:20px; }
.grid3 { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:20px; margin-bottom:20px; }
.grid-wide { grid-template-columns:2fr 1fr; }
.grid1 { display:grid; grid-template-columns:1fr; gap:20px; margin-bottom:20px; }
.plot-card { background:#fff; border-radius:14px; padding:18px 20px 14px;
             box-shadow:0 2px 8px rgba(15,23,42,.06); }
.plot-card h5 { font-size:.76rem; letter-spacing:.14em; text-transform:uppercase;
                color:#1E3445; font-weight:700; margin:0 0 10px; }
.plot-card .note { font-size:.74rem; color:#8A94A3; margin-top:6px; }
.disclaimer { color:#6B7280; font-size:.8rem; }

table.dataTable thead th { background:#1E3445; color:#fff; border:none; }
.btn-export { background:#1E3445; color:#fff; border:none; border-radius:8px; font-size:.8rem; }

@media (max-width:1000px) {
  .layout { grid-template-columns:1fr; padding:20px; }
  .hero { padding:22px 20px; }
  .kpi-grid, .grid3 { grid-template-columns:repeat(2,minmax(0,1fr)); }
  .grid2, .grid-wide { grid-template-columns:1fr; }
}
"

# ---------- UI ----------
ui <- fluidPage(
  theme = app_theme,
  title = "Hybrid Renewable Energy Digital Twin",
  tags$head(tags$style(HTML(css))),

  div(class = "hero",
      div(class = "eyebrow", "Climate-adaptive \u00B7 Coastal Bangladesh"),
      h1("Hybrid Renewable Energy Digital Twin"),
      div(class = "sub", "Energy reliability and healthcare resilience under climate stress"),
      uiOutput("status_pill")),

  div(class = "layout",

      div(class = "side",
          h6("Data input"),
          fileInput("datafile", NULL, accept = ".csv",
                    buttonLabel = "Browse...",
                    placeholder = "Upload hourly dataset (.csv)"),
          uiOutput("load_message"),
          div(class = "hint",
              "Needs timestamp, weather, generation, load, battery and healthcare-risk columns."),
          hr(),
          h6("Time window"),
          dateRangeInput("dates", "Analysis period", format = "yyyy-mm-dd"),
          selectInput("resolution", "Chart resolution",
                      choices = c("Daily", "Weekly", "Monthly"), selected = "Daily")
      ),

      div(class = "main",
          uiOutput("empty_state"),
          div(class = "sec", "System performance overview"),
          uiOutput("kpi_cards"),
          uiOutput("sections"))
  )
)

# ---------- Server ----------
server <- function(input, output, session) {

  data_result <- reactiveVal(NULL)
  data_error  <- reactiveVal(NULL)

  observe({
    if (is.null(input$datafile) && !is.null(initial_file) && is.null(data_result())) {
      tryCatch({
        data_result(read_energy_data(initial_file))
        data_error(NULL)
      }, error = function(e) data_error(conditionMessage(e)))
    }
  })

  observeEvent(input$datafile, {
    req(input$datafile$datapath)
    tryCatch({
      data_result(read_energy_data(input$datafile$datapath))
      data_error(NULL)
    }, error = function(e) {
      data_result(NULL)
      data_error(conditionMessage(e))
    })
  })

  observeEvent(data_result(), {
    dat <- data_result()
    req(dat)
    updateDateRangeInput(session, "dates",
                         start = min(dat$date), end = max(dat$date),
                         min = min(dat$date), max = max(dat$date))
  })

  output$status_pill <- renderUI({
    if (is.null(data_result())) {
      div(class = "pill", "Awaiting data")
    } else {
      dat <- data_result()
      div(class = "pill ok",
          paste0("Data loaded \u00B7 ", format(nrow(dat), big.mark = ","), " hourly records"))
    }
  })

  output$load_message <- renderUI({
    if (!is.null(data_error())) {
      div(class = "status-err", paste("Dataset error:", data_error()))
    } else if (!is.null(data_result())) {
      dat <- data_result()
      div(class = "status-ok",
          paste0("Loaded: ", format(min(dat$date)), " to ", format(max(dat$date))))
    }
  })

  output$empty_state <- renderUI({
    if (is.null(data_result())) {
      div(class = "empty",
          h4("Upload an hourly dataset to begin"),
          p("Use the Data input panel on the left. Every chart and indicator updates with your selections."))
    }
  })

  filtered_data <- reactive({
    validate(need(!is.null(data_result()), "Upload the dataset to view this panel."))
    dat <- data_result()
    if (!is.null(input$dates) && length(input$dates) == 2 && !any(is.na(input$dates))) {
      dat <- dat[dat$date >= input$dates[1] & dat$date <= input$dates[2], ]
    }
    validate(need(nrow(dat) > 0, "No records fall within the selected date range."))
    unit <- switch(input$resolution, Daily = "day", Weekly = "week", Monthly = "month")
    dat$period <- as.Date(cut(dat$date, breaks = unit))
    dat
  })

  # ---- KPI cards ----
  output$kpi_cards <- renderUI({
    dat <- tryCatch(filtered_data(), error = function(e) NULL)

    if (is.null(dat)) {
      v <- rep("\u2014", 6)
    } else {
      total_unmet <- sum(dat$unmet_load_kw, na.rm = TRUE)
      total_dem   <- sum(dat$total_load_demand_kw, na.rm = TRUE)
      v <- c(
        fmt(sum(dat$total_generation_kw, na.rm = TRUE)),
        fmt(total_unmet),
        fmt(sum(dat$healthcare_outage_risk > 0, na.rm = TRUE)),
        fmt(min(dat$bess_soc_percent, na.rm = TRUE), 1),
        fmt(sum(dat$grid_availability == 0, na.rm = TRUE)),
        fmt(100 * total_unmet / total_dem, 2)
      )
    }

    div(class = "kpi-grid",
        kpi_card("Total generation",   v[1], "kWh", teal),
        kpi_card("Unmet energy",       v[2], "kWh", amber),
        kpi_card("Healthcare risk",    v[3], "hours", red),
        kpi_card("Battery minimum SOC", v[4], "%", blue),
        kpi_card("Grid outage",        v[5], "hours", violet),
        kpi_card("Energy not served",  v[6], "% of demand", slate))
  })

  # ---- Page sections ----
  output$sections <- renderUI({
    req(data_result())
    tagList(
      div(class = "sec", "Energy system"),
      div(class = "grid2 grid-wide",
          plot_card("Generation and demand", "daily_energy", "320px"),
          plot_card("Average generation by source", "generation_mix", "320px")),
      div(class = "grid2",
          plot_card("Generation by technology", "energy_components"),
          plot_card("Generation compared with demand", "load_supply")),
      div(class = "grid3",
          plot_card("Unmet energy", "unmet_load_plot", "260px"),
          plot_card("Battery state of charge", "battery_soc", "260px"),
          plot_card("Grid availability", "grid_status", "260px")),

      div(class = "sec", "Climate conditions"),
      div(class = "grid2",
          plot_card("Mean ambient temperature", "temperature_plot"),
          plot_card("Mean heat index", "heat_index_plot")),
      div(class = "grid2",
          plot_card("Flagged extreme-weather hours", "weather_events"),
          plot_card("Climate and resource indicators", "solar_wind_tidal",
                    note = "Wind speed and tidal velocity are multiplied by 10 for display only.")),

      div(class = "sec", "Healthcare resilience"),
      div(class = "grid2",
          plot_card("Cold-chain and total demand", "healthcare_load"),
          plot_card("Healthcare outage-risk hours", "healthcare_risk")),
      div(class = "grid1",
          plot_card("Unmet load vs medical cold-chain demand", "unmet_vs_coldchain")),

      div(class = "sec", "Data and summary"),
      div(class = "grid1",
          div(class = "plot-card", h5("Selected-period summary"), DTOutput("summary_table")),
          div(class = "plot-card", h5("Hourly records"), DTOutput("data_table"),
              br(), downloadButton("download_csv", "Export filtered data (CSV)",
                                   class = "btn-export btn-sm"))),
      p(class = "disclaimer",
        "The dashboard visualises the supplied hourly observations. It does not independently validate sensor measurements, infer causal relationships, or run a future climate projection or dispatch optimisation.")
    )
  })

  # ---- Energy system ----
  output$daily_energy <- renderPlot({
    d <- agg(filtered_data(), c("total_generation_kw", "total_load_demand_kw"))
    ggplot(d, aes(x = period)) +
      geom_line(aes(y = total_generation_kw, colour = "Generation"), linewidth = 0.85) +
      geom_line(aes(y = total_load_demand_kw, colour = "Demand"), linewidth = 0.85) +
      scale_colour_manual(values = c("Generation" = teal, "Demand" = ink)) +
      labs(x = NULL, y = "Mean power (kW)") + theme_pro()
  })

  output$generation_mix <- renderPlot({
    dat <- filtered_data()
    mix <- data.frame(
      Source = c("Solar PV", "Wind", "Tidal"),
      Mean_kW = c(mean(dat$pv_power_kw, na.rm = TRUE),
                  mean(dat$wind_power_kw, na.rm = TRUE),
                  mean(dat$tidal_power_kw, na.rm = TRUE)))
    ggplot(mix, aes(x = reorder(Source, Mean_kW), y = Mean_kW, fill = Source)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = round(Mean_kW, 1)), hjust = -0.15, size = 3.6, colour = "#374151") +
      scale_fill_manual(values = c("Solar PV" = amber, "Wind" = blue, "Tidal" = teal)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      coord_flip() + labs(x = NULL, y = "Mean power (kW)") + theme_pro()
  })

  output$energy_components <- renderPlot({
    d <- agg(filtered_data(), c("pv_power_kw", "wind_power_kw", "tidal_power_kw"))
    ggplot(d, aes(x = period)) +
      geom_line(aes(y = pv_power_kw, colour = "Solar PV"), linewidth = 0.8) +
      geom_line(aes(y = wind_power_kw, colour = "Wind"), linewidth = 0.8) +
      geom_line(aes(y = tidal_power_kw, colour = "Tidal"), linewidth = 0.8) +
      scale_colour_manual(values = c("Solar PV" = amber, "Wind" = blue, "Tidal" = teal)) +
      labs(x = NULL, y = "Mean power (kW)") + theme_pro()
  })

  output$load_supply <- renderPlot({
    d <- agg(filtered_data(), c("total_generation_kw", "total_load_demand_kw"))
    ggplot(d, aes(x = total_generation_kw, y = total_load_demand_kw)) +
      geom_point(alpha = 0.65, colour = ink) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = slate) +
      labs(x = "Mean generation (kW)", y = "Mean demand (kW)") + theme_pro()
  })

  output$unmet_load_plot <- renderPlot({
    d <- aggregate(unmet_load_kw ~ period, data = filtered_data(), FUN = sum, na.rm = TRUE)
    ggplot(d, aes(x = period, y = unmet_load_kw)) +
      geom_col(fill = amber) +
      labs(x = NULL, y = "Unmet energy (kWh)") + theme_pro()
  })

  output$battery_soc <- renderPlot({
    ggplot(filtered_data(), aes(x = timestamp, y = bess_soc_percent)) +
      geom_line(linewidth = 0.4, colour = blue) +
      labs(x = NULL, y = "State of charge (%)") + theme_pro()
  })

  output$grid_status <- renderPlot({
    dat <- filtered_data()
    s <- data.frame(
      Status = c("Available", "Unavailable"),
      Hours = c(sum(dat$grid_availability == 1, na.rm = TRUE),
                sum(dat$grid_availability == 0, na.rm = TRUE)))
    ggplot(s, aes(x = Status, y = Hours, fill = Status)) +
      geom_col(width = 0.55, show.legend = FALSE) +
      geom_text(aes(label = format(Hours, big.mark = ",")), vjust = -0.4, size = 3.6) +
      scale_fill_manual(values = c("Available" = teal, "Unavailable" = red)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Hours") + theme_pro()
  })

  # ---- Climate ----
  output$temperature_plot <- renderPlot({
    d <- agg(filtered_data(), "ambient_temperature_c")
    ggplot(d, aes(x = period, y = ambient_temperature_c)) +
      geom_line(colour = amber, linewidth = 0.85) +
      labs(x = NULL, y = "Temperature (\u00B0C)") + theme_pro()
  })

  output$heat_index_plot <- renderPlot({
    d <- agg(filtered_data(), "heat_index_c")
    ggplot(d, aes(x = period, y = heat_index_c)) +
      geom_line(colour = red, linewidth = 0.85) +
      labs(x = NULL, y = "Heat index (\u00B0C)") + theme_pro()
  })

  output$weather_events <- renderPlot({
    d <- aggregate(extreme_weather_flag ~ period, data = filtered_data(), FUN = sum, na.rm = TRUE)
    ggplot(d, aes(x = period, y = extreme_weather_flag)) +
      geom_col(fill = plum) +
      labs(x = NULL, y = "Flagged hours") + theme_pro()
  })

  output$solar_wind_tidal <- renderPlot({
    d <- agg(filtered_data(), c("solar_ghi_wm2", "wind_speed_10m_ms", "tidal_velocity_ms"))
    ggplot(d, aes(x = period)) +
      geom_line(aes(y = solar_ghi_wm2, colour = "Solar GHI"), linewidth = 0.75) +
      geom_line(aes(y = wind_speed_10m_ms * 10, colour = "Wind speed \u00D710"), linewidth = 0.75) +
      geom_line(aes(y = tidal_velocity_ms * 10, colour = "Tidal velocity \u00D710"), linewidth = 0.75) +
      scale_colour_manual(values = c("Solar GHI" = amber, "Wind speed \u00D710" = blue,
                                     "Tidal velocity \u00D710" = teal)) +
      labs(x = NULL, y = "Displayed value") + theme_pro()
  })

  # ---- Healthcare ----
  output$healthcare_load <- renderPlot({
    d <- agg(filtered_data(), c("medical_cold_chain_load_kw", "total_load_demand_kw"))
    ggplot(d, aes(x = period)) +
      geom_line(aes(y = medical_cold_chain_load_kw, colour = "Cold-chain load"), linewidth = 0.85) +
      geom_line(aes(y = total_load_demand_kw, colour = "Total demand"), linewidth = 0.85) +
      scale_colour_manual(values = c("Cold-chain load" = red, "Total demand" = ink)) +
      labs(x = NULL, y = "Mean power (kW)") + theme_pro()
  })

  output$healthcare_risk <- renderPlot({
    d <- aggregate(healthcare_outage_risk ~ period, data = filtered_data(), FUN = sum, na.rm = TRUE)
    ggplot(d, aes(x = period, y = healthcare_outage_risk)) +
      geom_col(fill = red) +
      labs(x = NULL, y = "Risk hours") + theme_pro()
  })

  output$unmet_vs_coldchain <- renderPlot({
    ggplot(filtered_data(), aes(x = medical_cold_chain_load_kw, y = unmet_load_kw)) +
      geom_point(alpha = 0.35, colour = blue) +
      labs(x = "Medical cold-chain load (kW)", y = "Unmet load (kW)") + theme_pro()
  })

  # ---- Tables ----
  output$summary_table <- renderDT({
    dat <- filtered_data()
    tbl <- data.frame(
      Indicator = c("Hourly observations", "Average generation (kW)", "Average demand (kW)",
                    "Total unmet energy (kWh)", "Grid outage hours",
                    "Healthcare outage-risk hours", "Extreme-weather flagged hours",
                    "Average battery SOC (%)", "Average medical cold-chain load (kW)"),
      Value = c(nrow(dat),
                round(mean(dat$total_generation_kw, na.rm = TRUE), 2),
                round(mean(dat$total_load_demand_kw, na.rm = TRUE), 2),
                round(sum(dat$unmet_load_kw, na.rm = TRUE), 2),
                sum(dat$grid_availability == 0, na.rm = TRUE),
                sum(dat$healthcare_outage_risk > 0, na.rm = TRUE),
                sum(dat$extreme_weather_flag > 0, na.rm = TRUE),
                round(mean(dat$bess_soc_percent, na.rm = TRUE), 2),
                round(mean(dat$medical_cold_chain_load_kw, na.rm = TRUE), 2)))
    datatable(tbl, rownames = FALSE, class = "stripe hover compact",
              options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  display_cols <- c("timestamp", "pv_power_kw", "wind_power_kw", "tidal_power_kw",
                    "total_generation_kw", "total_load_demand_kw", "grid_availability",
                    "bess_soc_percent", "unmet_load_kw", "healthcare_outage_risk")

  output$data_table <- renderDT({
    dat <- filtered_data()[, display_cols]
    dat$timestamp <- format(dat$timestamp, "%Y-%m-%d %H:%M")
    datatable(dat, rownames = FALSE, class = "stripe hover compact",
              options = list(pageLength = 15, scrollX = TRUE, dom = "frtip")) |>
      formatRound(setdiff(display_cols, c("timestamp", "grid_availability",
                                           "healthcare_outage_risk")), 2)
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("filtered_energy_data_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- filtered_data()
      write.csv(d[, setdiff(names(d), c("date", "period"))], file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
