# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)

# Sample dataset
countries_data <- data.frame(
  Country = c("USA", "China", "India", "Brazil", "Russia"),
  Population = c(331002651, 1444216107, 1380004385, 212559417, 145934462),
  GDP = c(21433225, 14342903, 2875148, 1839755, 1665048),
  HDI = c(0.926, 0.758, 0.645, 0.758, 0.824)
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Country Information Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    box(
      title = "Population by Country",
      plotOutput("population_chart", height = 300)
    ),
    box(
      title = "GDP by Country",
      plotlyOutput("gdp_chart", height = 300)
    ),
    box(
      title = "Country Information Table",
      tableOutput("country_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Population bar chart
  output$population_chart <- renderPlot({
    ggplot(countries_data, aes(x = Country, y = Population, fill = Country)) +
      geom_bar(stat = "identity") +
      labs(title = "Population by Country", y = "Population")
  })
  
  # GDP pie chart
  output$gdp_chart <- renderPlotly({
    plot_ly(countries_data, labels = ~Country, values = ~GDP, type = "pie") %>%
      layout(title = "GDP by Country")
  })
  
  # Country information table
  output$country_table <- renderTable({
    countries_data
  })
}

# Run the Shiny app
shinyApp(ui, server)
