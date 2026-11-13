# ============================================================
# CLIMATE-ADAPTIVE HYBRID RENEWABLE ENERGY DIGITAL TWIN
# Coastal Bangladesh | Shiny dashboard
#
# Required packages:
# install.packages(c("shiny", "ggplot2"))
#
# Run:
# 1. Save this complete script as app.R
# 2. Put your CSV in the same folder OR upload it in the dashboard.
# 3. In RStudio, set the working directory to this folder.
# 4. Run: shiny::runApp()
# ============================================================

library(shiny)
library(ggplot2)

required_columns <- c(
  "timestamp",
  "ambient_temperature_c",
  "relative_humidity_percent",
  "heat_index_c",
  "thermal_stress_index",
  "solar_ghi_wm2",
  "wind_speed_10m_ms",
  "tidal_velocity_ms",
  "extreme_weather_flag",
  "pv_power_kw",
  "wind_power_kw",
  "tidal_power_kw",
  "total_generation_kw",
  "medical_cold_chain_load_kw",
  "total_load_demand_kw",
  "grid_availability",
  "bess_soc_percent",
  "bess_charge_kw",
  "bess_discharge_kw",
  "unmet_load_kw",
  "healthcare_outage_risk"
)

# Look for the original dataset beside app.R.
# If it is not found, upload the CSV through the dashboard.
candidate_files <- list.files(
  pattern = "^climate_health_hybrid_energy_digital_twin.*\\.csv$",
  ignore.case = TRUE
)
initial_file <- if (length(candidate_files) > 0) candidate_files[1] else NULL

read_energy_data <- function(path) {
  dat <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

  missing_cols <- setdiff(required_columns, names(dat))
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "The CSV is missing these required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  dat$timestamp <- as.POSIXct(
    dat$timestamp,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "Asia/Dhaka"
  )

  if (any(is.na(dat$timestamp))) {
    dat$timestamp <- as.POSIXct(
      dat$timestamp,
      tz = "Asia/Dhaka"
    )
  }

  numeric_cols <- setdiff(required_columns, "timestamp")
  for (nm in numeric_cols) {
    dat[[nm]] <- suppressWarnings(as.numeric(dat[[nm]]))
  }

  dat <- dat[!is.na(dat$timestamp), ]
  dat <- dat[order(dat$timestamp), ]
  dat$date <- as.Date(dat$timestamp)

  if (nrow(dat) == 0) {
    stop("No valid timestamp records were found in the CSV.")
  }

  dat
}

make_card <- function(label, value, note = "") {
  div(
    class = "metric-card",
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    div(class = "metric-note", note)
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("Coastal Bangladesh Renewable Energy Digital Twin"),
    tags$style(HTML("
      body { background: #f4f6f9; color: #202938; }
      .main-title { font-size: 25px; font-weight: 700; margin-bottom: 4px; }
      .subtitle { color: #667085; margin-bottom: 18px; }
      .top-panel {
        background: white; padding: 18px 20px; border-radius: 10px;
        margin-bottom: 16px; border: 1px solid #e5e9f0;
      }
      .metric-grid {
        display: grid; grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
        gap: 12px; margin: 14px 0 18px 0;
      }
      .metric-card {
        background: white; padding: 15px; border-radius: 10px;
        border: 1px solid #e5e9f0; min-height: 105px;
      }
      .metric-label { color: #667085; font-size: 13px; }
      .metric-value { font-size: 25px; font-weight: 700; margin: 7px 0; }
      .metric-note { color: #7b8494; font-size: 11px; }
      .panel {
        background: white; padding: 15px; border-radius: 10px;
        border: 1px solid #e5e9f0; margin-bottom: 15px;
      }
      .section-heading { font-size: 17px; font-weight: 650; margin-bottom: 10px; }
      .disclaimer { color: #667085; font-size: 12px; margin-top: 12px; }
      .well { background: white; border: 1px solid #e5e9f0; }
      .nav-tabs { margin-bottom: 15px; }
    "))
  ),

  div(
    class = "top-panel",
    div(class = "main-title", "Climate-Adaptive Hybrid Renewable Energy System"),
    div(
      class = "subtitle",
      "An R-based digital twin for energy reliability and environmental health resilience in coastal Bangladesh"
    ),
    fluidRow(
      column(
        8,
        fileInput(
          "datafile",
          "Upload your original CSV dataset",
          accept = c(".csv")
        ),
        helpText(
          "If the CSV is saved in the same folder as app.R, the dashboard will try to load it automatically."
        )
      ),
      column(
        4,
        dateRangeInput(
          "dates",
          "Analysis period",
          start = as.Date("2025-01-01"),
          end = as.Date("2025-12-31"),
          format = "yyyy-mm-dd"
        )
      )
    ),
    uiOutput("load_message")
  ),

  uiOutput("dashboard_body")
)

server <- function(input, output, session) {

  data_result <- reactiveVal(NULL)
  data_error <- reactiveVal(NULL)

  # Load a CSV automatically if one is beside app.R.
  observe({
    if (is.null(input$datafile) && !is.null(initial_file) &&
        is.null(data_result())) {
      tryCatch(
        {
          data_result(read_energy_data(initial_file))
          data_error(NULL)
        },
        error = function(e) data_error(conditionMessage(e))
      )
    }
  })

  # Load a user-selected CSV.
  observeEvent(input$datafile, {
    req(input$datafile$datapath)
    tryCatch(
      {
        data_result(read_energy_data(input$datafile$datapath))
        data_error(NULL)
        dat <- data_result()
        updateDateRangeInput(
          session,
          "dates",
          start = min(dat$date),
          end = max(dat$date),
          min = min(dat$date),
          max = max(dat$date)
        )
      },
      error = function(e) {
        data_result(NULL)
        data_error(conditionMessage(e))
      }
    )
  })

  output$load_message <- renderUI({
    if (!is.null(data_error())) {
      div(
        style = "color:#b42318; font-weight:600;",
        paste("Dataset error:", data_error())
      )
    } else if (is.null(data_result())) {
      div(
        style = "color:#667085;",
        "No dataset loaded yet. Upload the CSV file to start the dashboard."
      )
    } else {
      dat <- data_result()
      div(
        style = "color:#16794b; font-weight:600;",
        paste0(
          "Dataset loaded: ",
          format(min(dat$date)), " to ", format(max(dat$date)),
          " | ", format(nrow(dat), big.mark = ","), " hourly records"
        )
      )
    }
  })

  filtered_data <- reactive({
    req(data_result())
    dat <- data_result()

    if (!is.null(input$dates) && length(input$dates) == 2 &&
        !any(is.na(input$dates))) {
      dat <- dat[
        dat$date >= input$dates[1] &
          dat$date <= input$dates[2],
      ]
    }
    validate(need(nrow(dat) > 0, "No records fall within the selected date range."))
    dat
  })

  output$dashboard_body <- renderUI({
    if (is.null(data_result())) {
      return(
        div(
          class = "panel",
          h4("Start by loading the dataset"),
          p("Upload climate_health_hybrid_energy_digital_twin_v2.csv above.")
        )
      )
    }

    tagList(
      uiOutput("kpi_row"),
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Overview",
          br(),
          fluidRow(
            column(8, div(class = "panel", plotOutput("daily_energy", height = "330px"))),
            column(4, div(class = "panel", plotOutput("generation_mix", height = "330px")))
          ),
          fluidRow(
            column(6, div(class = "panel", plotOutput("grid_status", height = "290px"))),
            column(6, div(class = "panel", plotOutput("battery_soc", height = "290px")))
          )
        ),
        tabPanel(
          "Energy system",
          br(),
          fluidRow(
            column(6, div(class = "panel", plotOutput("energy_components", height = "330px"))),
            column(6, div(class = "panel", plotOutput("load_supply", height = "330px")))
          ),
          div(class = "panel", plotOutput("unmet_load_plot", height = "320px"))
        ),
        tabPanel(
          "Climate conditions",
          br(),
          fluidRow(
            column(6, div(class = "panel", plotOutput("temperature_plot", height = "320px"))),
            column(6, div(class = "panel", plotOutput("heat_index_plot", height = "320px")))
          ),
          fluidRow(
            column(6, div(class = "panel", plotOutput("weather_events", height = "300px"))),
            column(6, div(class = "panel", plotOutput("solar_wind_tidal", height = "300px")))
          )
        ),
        tabPanel(
          "Healthcare resilience",
          br(),
          fluidRow(
            column(6, div(class = "panel", plotOutput("healthcare_load", height = "320px"))),
            column(6, div(class = "panel", plotOutput("healthcare_risk", height = "320px")))
          ),
          div(class = "panel", plotOutput("unmet_vs_coldchain", height = "320px"))
        ),
        tabPanel(
          "Data and summary",
          br(),
          div(class = "panel", h4("Selected-period summary"), tableOutput("summary_table")),
          div(class = "panel", h4("Hourly records"), tableOutput("data_table")),
          p(
            class = "disclaimer",
            "The dashboard visualises the supplied hourly observations. It does not independently validate sensor measurements, infer causal relationships, or run a future climate projection or dispatch optimisation."
          )
        )
      )
    )
  })

  output$kpi_row <- renderUI({
    dat <- filtered_data()

    mean_gen <- mean(dat$total_generation_kw, na.rm = TRUE)
    mean_demand <- mean(dat$total_load_demand_kw, na.rm = TRUE)
    unmet_energy <- sum(dat$unmet_load_kw, na.rm = TRUE)
    grid_outage <- sum(dat$grid_availability == 0, na.rm = TRUE)
    risk_hours <- sum(dat$healthcare_outage_risk > 0, na.rm = TRUE)
    extreme_hours <- sum(dat$extreme_weather_flag > 0, na.rm = TRUE)

    div(
      class = "metric-grid",
      make_card("Average generation", paste0(round(mean_gen, 1), " kW"), "Mean hourly total generation"),
      make_card("Average demand", paste0(round(mean_demand, 1), " kW"), "Mean hourly total load"),
      make_card("Unmet energy", paste0(format(round(unmet_energy, 1), big.mark = ","), " kWh"), "Sum of hourly unmet load; 1-hour records"),
      make_card("Grid outage", paste0(grid_outage, " hours"), "Hours with grid_availability = 0"),
      make_card("Healthcare risk", paste0(risk_hours, " hours"), "Hours with healthcare_outage_risk > 0"),
      make_card("Extreme weather", paste0(extreme_hours, " hours"), "Hours with extreme_weather_flag > 0")
    )
  })

  output$daily_energy <- renderPlot({
    dat <- filtered_data()
    daily <- aggregate(
      cbind(total_generation_kw, total_load_demand_kw) ~ date,
      data = dat,
      FUN = mean,
      na.rm = TRUE
    )
    ggplot(daily, aes(x = date)) +
      geom_line(aes(y = total_generation_kw, colour = "Generation"), linewidth = 0.7) +
      geom_line(aes(y = total_load_demand_kw, colour = "Demand"), linewidth = 0.7) +
      scale_colour_manual(values = c("Generation" = "#16865b", "Demand" = "#315b9a")) +
      labs(title = "Daily average generation and demand", x = NULL, y = "Power (kW)", colour = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$generation_mix <- renderPlot({
    dat <- filtered_data()
    mix <- data.frame(
      Source = c("Solar PV", "Wind", "Tidal"),
      Mean_kW = c(
        mean(dat$pv_power_kw, na.rm = TRUE),
        mean(dat$wind_power_kw, na.rm = TRUE),
        mean(dat$tidal_power_kw, na.rm = TRUE)
      )
    )
    ggplot(mix, aes(x = reorder(Source, Mean_kW), y = Mean_kW, fill = Source)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Average generation by source", x = NULL, y = "Mean power (kW)") +
      theme_minimal(base_size = 12)
  })

  output$grid_status <- renderPlot({
    dat <- filtered_data()
    status <- data.frame(
      Status = c("Available", "Unavailable"),
      Hours = c(
        sum(dat$grid_availability == 1, na.rm = TRUE),
        sum(dat$grid_availability == 0, na.rm = TRUE)
      )
    )
    ggplot(status, aes(x = Status, y = Hours, fill = Status)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Grid availability", x = NULL, y = "Hours") +
      theme_minimal(base_size = 12)
  })

  output$battery_soc <- renderPlot({
    dat <- filtered_data()
    ggplot(dat, aes(x = timestamp, y = bess_soc_percent)) +
      geom_line(linewidth = 0.45, colour = "#7257a8") +
      labs(title = "Battery state of charge", x = NULL, y = "SOC (%)") +
      theme_minimal(base_size = 12)
  })

  output$energy_components <- renderPlot({
    dat <- filtered_data()
    daily <- aggregate(
      cbind(pv_power_kw, wind_power_kw, tidal_power_kw) ~ date,
      data = dat,
      FUN = mean,
      na.rm = TRUE
    )
    ggplot(daily, aes(x = date)) +
      geom_line(aes(y = pv_power_kw, colour = "Solar PV"), linewidth = 0.65) +
      geom_line(aes(y = wind_power_kw, colour = "Wind"), linewidth = 0.65) +
      geom_line(aes(y = tidal_power_kw, colour = "Tidal"), linewidth = 0.65) +
      labs(title = "Daily average generation by technology", x = NULL, y = "Power (kW)", colour = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$load_supply <- renderPlot({
    dat <- filtered_data()
    daily <- aggregate(
      cbind(total_generation_kw, total_load_demand_kw) ~ date,
      data = dat,
      FUN = mean,
      na.rm = TRUE
    )
    ggplot(daily, aes(x = total_generation_kw, y = total_load_demand_kw)) +
      geom_point(alpha = 0.65, colour = "#315b9a") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(title = "Generation compared with demand", x = "Average generation (kW)", y = "Average demand (kW)") +
      theme_minimal(base_size = 12)
  })

  output$unmet_load_plot <- renderPlot({
    dat <- filtered_data()
    daily <- aggregate(unmet_load_kw ~ date, data = dat, FUN = sum, na.rm = TRUE)
    ggplot(daily, aes(x = date, y = unmet_load_kw)) +
      geom_col(fill = "#b94a48") +
      labs(title = "Daily unmet energy", x = NULL, y = "Unmet energy (kWh)") +
      theme_minimal(base_size = 12)
  })

  output$temperature_plot <- renderPlot({
    dat <- filtered_data()
    daily <- aggregate(
      cbind(ambient_temperature_c, relative_humidity_percent) ~ date,
      data = dat,
      FUN = mean,
      na.rm = TRUE
    )
    ggplot(daily, aes(x = date, y = ambient_temperature_c)) +
      geom_line(colour = "#c56a36", linewidth = 0.7) +
      labs(title = "Daily mean ambient temperature", x = NULL, y = "Temperature (°C)") +
      theme_minimal(base_size = 12)
  })

  output$heat_index_plot <- renderPlot({
    dat <- filtered_data()
    daily <- aggregate(
      cbind(heat_index_c, thermal_stress_index) ~ date,
      data = dat,
      FUN = mean,
      na.rm = TRUE
    )
    ggplot(daily, aes(x = date, y = heat_index_c)) +
      geom_line(colour = "#a44b43", linewidth = 0.7) +
      labs(title = "Daily mean heat index", x = NULL, y = "Heat index (°C)") +
      theme_minimal(base_size = 12)
  })

  output$weather_events <- renderPlot({
    dat <- filtered_data()
    daily <- aggregate(extreme_weather_flag ~ date, data = dat, FUN = sum, na.rm = TRUE)
    ggplot(daily, aes(x = date, y = extreme_weather_flag)) +
      geom_col(fill = "#7b6aa6") +
      labs(title = "Daily flagged extreme-weather hours", x = NULL, y = "Flagged hours") +
      theme_minimal(base_size = 12)
  })

  output$solar_wind_tidal <- renderPlot({
    dat <- filtered_data()
    daily <- aggregate(
      cbind(solar_ghi_wm2, wind_speed_10m_ms, tidal_velocity_ms) ~ date,
      data = dat,
      FUN = mean,
      na.rm = TRUE
    )
    ggplot(daily, aes(x = date)) +
      geom_line(aes(y = solar_ghi_wm2, colour = "Solar GHI"), linewidth = 0.65) +
      geom_line(aes(y = wind_speed_10m_ms * 10, colour = "Wind speed ×10"), linewidth = 0.65) +
      geom_line(aes(y = tidal_velocity_ms * 10, colour = "Tidal velocity ×10"), linewidth = 0.65) +
      labs(
        title = "Daily climate and resource indicators",
        subtitle = "Wind and tidal velocity are multiplied by 10 for display only",
        x = NULL, y = "Displayed value", colour = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$healthcare_load <- renderPlot({
    dat <- filtered_data()
    daily <- aggregate(
      cbind(medical_cold_chain_load_kw, total_load_demand_kw) ~ date,
      data = dat,
      FUN = mean,
      na.rm = TRUE
    )
    ggplot(daily, aes(x = date)) +
      geom_line(aes(y = medical_cold_chain_load_kw, colour = "Cold-chain load"), linewidth = 0.7) +
      geom_line(aes(y = total_load_demand_kw, colour = "Total demand"), linewidth = 0.7) +
      labs(title = "Healthcare cold-chain and total demand", x = NULL, y = "Power (kW)", colour = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$healthcare_risk <- renderPlot({
    dat <- filtered_data()
    daily <- aggregate(healthcare_outage_risk ~ date, data = dat, FUN = sum, na.rm = TRUE)
    ggplot(daily, aes(x = date, y = healthcare_outage_risk)) +
      geom_col(fill = "#bd6b42") +
      labs(title = "Daily healthcare outage-risk hours", x = NULL, y = "Risk hours") +
      theme_minimal(base_size = 12)
  })

  output$unmet_vs_coldchain <- renderPlot({
    dat <- filtered_data()
    ggplot(dat, aes(x = medical_cold_chain_load_kw, y = unmet_load_kw)) +
      geom_point(alpha = 0.35, colour = "#315b9a") +
      labs(
        title = "Unmet load in relation to medical cold-chain demand",
        x = "Medical cold-chain load (kW)",
        y = "Unmet load (kW)"
      ) +
      theme_minimal(base_size = 12)
  })

  output$summary_table <- renderTable({
    dat <- filtered_data()
    data.frame(
      Indicator = c(
        "Hourly observations",
        "Average generation (kW)",
        "Average demand (kW)",
        "Total unmet energy (kWh)",
        "Grid outage hours",
        "Healthcare outage-risk hours",
        "Extreme-weather flagged hours",
        "Average battery SOC (%)",
        "Average medical cold-chain load (kW)"
      ),
      Value = c(
        nrow(dat),
        round(mean(dat$total_generation_kw, na.rm = TRUE), 2),
        round(mean(dat$total_load_demand_kw, na.rm = TRUE), 2),
        round(sum(dat$unmet_load_kw, na.rm = TRUE), 2),
        sum(dat$grid_availability == 0, na.rm = TRUE),
        sum(dat$healthcare_outage_risk > 0, na.rm = TRUE),
        sum(dat$extreme_weather_flag > 0, na.rm = TRUE),
        round(mean(dat$bess_soc_percent, na.rm = TRUE), 2),
        round(mean(dat$medical_cold_chain_load_kw, na.rm = TRUE), 2)
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$data_table <- renderTable({
    dat <- filtered_data()
    head(
      dat[, c(
        "timestamp", "pv_power_kw", "wind_power_kw", "tidal_power_kw",
        "total_generation_kw", "total_load_demand_kw",
        "grid_availability", "bess_soc_percent",
        "unmet_load_kw", "healthcare_outage_risk"
      )],
      20
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui = ui, server = server)
