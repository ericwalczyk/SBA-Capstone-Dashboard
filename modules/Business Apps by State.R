library(shiny)
library(plotly)
library(readr)
library(tidyverse)

# Load and reshape the data
apps_data <- read_csv("bfs_county_apps_annual.csv")

apps_long <- apps_data %>%
  pivot_longer(cols = `2005`:`2023`, names_to = "Year", values_to = "Applications") %>%
  mutate(Year = as.integer(Year))

ui <- fluidPage(
  titlePanel("Business Applications by State"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year", min = 2005, max = 2023, value = 2023)
    ),
    mainPanel(
      plotlyOutput("appsBar")
    )
  )
)

server <- function(input, output, session) {
  output$appsBar <- renderPlotly({
    filtered <- apps_long %>%
      filter(Year == input$year) %>%
      group_by(State) %>%
      summarise(Apps = sum(Applications, na.rm = TRUE)) %>%
      arrange(desc(Apps))
    
    plot_ly(
      data = filtered,
      x = ~reorder(State, Apps),
      y = ~Apps,
      type = "bar",
      marker = list(color = "royalblue")
    ) %>%
      layout(
        title = paste("Applications in", input$year),
        xaxis = list(title = "State", tickangle = -45),
        yaxis = list(title = "Applications")
      )
  })
}

shinyApp(ui = ui, server = server)


