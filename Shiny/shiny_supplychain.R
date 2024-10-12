# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(forecast)
library(lubridate)
library(DT)
library(shinyjs)

# Hypothetical supply chain dataset
supply_chain_data <- data.frame(
  Date = seq(as.Date("2022-01-01"), by = "months", length.out = 12),
  Demand = c(100, 120, 90, 110, 130, 95, 105, 115, 125, 85, 120, 100),
  Stock = c(80, 100, 75, 95, 110, 80, 90, 100, 110, 70, 100, 80)
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Supply Chain Predictive Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    useShinyjs(),
    box(
      title = "Demand Forecasting",
      plotOutput("demand_forecast_chart", height = 300)
    ),
    box(
      title = "Stock Levels Over Time",
      plotOutput("stock_levels_chart", height = 300)
    ),
    box(
      title = "Supply Chain Metrics Table",
      DTOutput("supply_chain_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Forecast demand
  output$demand_forecast_chart <- renderPlot({
    supply_chain_ts <- ts(supply_chain_data$Demand, frequency = 12)
    fit <- auto.arima(supply_chain_ts)
    forecast_plot <- autoplot(forecast(fit), main = "Demand Forecasting")
    forecast_plot
  })
  
  # Stock levels over time
  output$stock_levels_chart <- renderPlot({
    ggplot(supply_chain_data, aes(x = Date)) +
      geom_line(aes(y = Stock, color = "Stock")) +
      geom_line(aes(y = Demand, color = "Demand"), linetype = "dashed") +
      labs(title = "Stock Levels Over Time", y = "Quantity") +
      scale_color_manual(values = c("Stock" = "blue", "Demand" = "red"))
  })
  
  # Supply chain metrics table
  output$supply_chain_table <- renderDT({
    datatable(
      supply_chain_data,
      options = list(
        paging = FALSE,
        searching = FALSE,
        dom = 't',
        rowCallback = JS(
          'function(row, data) {',
          'var cols = data.length;',
          'for (var i = 0; i < cols; i++) {',
          'var cell = row.getElementsByTagName("td")[i];',
          'cell.setAttribute("data-toggle", "popover");',
          'cell.setAttribute("title", cols[i]);',
          '}',
          '}'
        )
      )
    )
  })
  
  # Add shinyjs callback for tooltips
  shinyjs::runjs('
    $(document).ready(function() {
      $("[data-toggle=popover]").popover();
    });
  ')
}

# Run the Shiny app
shinyApp(ui, server)
