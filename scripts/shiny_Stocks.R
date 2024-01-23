# Load required libraries
library(shiny)
library(shinydashboard)
library(quantmod)
library(dygraphs)
library(dplyr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Stock Price Dashboard"),
  dashboardSidebar(
    textInput("symbol", "Enter Stock Symbol:", value = "AAPL"),
    dateInput("start_date", "Start Date:", value = Sys.Date() - 365),
    dateInput("end_date", "End Date:", value = Sys.Date()),
    sliderInput("ma_period", "Moving Average Period", min = 1, max = 50, value = 10),
    actionButton("reset_button", "Reset Selection")
  ),
  dashboardBody(
    dygraphOutput("stock_plot"),
    tableOutput("summary_table")
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive function to fetch stock data
  stock_data <- reactive({
    getSymbols(input$symbol, src = "yahoo", from = input$start_date, to = input$end_date, auto.assign = FALSE)
  })

  # Render dygraph plot with rubber band selection
  output$stock_plot <- renderDygraph({
    stock_data_df <- data.frame(Date = index(stock_data()), Price = as.numeric(Cl(stock_data())))
    
    dygraph(stock_data_df, main = paste("Stock Price for", input$symbol)) %>%
      dyRangeSelector() %>%
      dyOptions(gridLineWidth = 0, axisLabelFontSize = 0)
  })

  # Render summary table with key metrics
  output$summary_table <- renderTable({
    stock_data_df <- data.frame(Date = index(stock_data()), Price = as.numeric(Cl(stock_data())))
    
    # Calculate moving average
    stock_data_df$MA <- SMA(Cl(stock_data()), n = input$ma_period)

    # Calculate VWAP
    stock_data_df$VWAP <- sum(stock_data_df$Price * stock_data_df$Volume) / sum(stock_data_df$Volume)
    
    # Extract key metrics
    summary_data <- data.frame(
      "Total Days" = nrow(stock_data_df),
      "Average Price" = mean(stock_data_df$Price),
      "Moving Average" = stock_data_df$MA[nrow(stock_data_df)],
      "VWAP" = stock_data_df$VWAP
    )
    
    # Remove row names
    rownames(summary_data) <- NULL
    
    summary_data
  }, rownames = FALSE)

  # Add reset button functionality
  observeEvent(input$reset_button, {
    session$sendCustomMessage(type = 'reset_dygraph')
  })

  # Custom message handler to reset dygraph selection
  observeEvent(input$reset_dygraph, {
    proxy <- session$findDygraph()
    proxy %>% dySetSelection(NULL)
  })
}

# Run the Shiny app
shinyApp(ui, server)
