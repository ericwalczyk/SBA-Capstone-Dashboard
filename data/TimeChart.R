# app.R

library(shiny)
library(plotly)
library(dplyr)

# 1. LOAD & AGGREGATE GDP
gdp_natl <- readRDS("state_gdp.rds") %>%
  mutate(
    year  = as.integer(year),
    value = as.numeric(value)
  ) %>%
  group_by(year) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# 2. LOAD ACS SUMMARY (per-capita income & unemployment rate)
acs_df <- readRDS("acs_summary.rds") %>%
  mutate(
    year               = as.integer(year),
    per_capita_income  = as.numeric(per_capita_income),
    unemployment_rate  = as.numeric(unemployment_rate)  # already numeric in your file
  )

# 3. AGGREGATE PER CAPITA INCOME
inc_natl <- acs_df %>%
  group_by(year) %>%
  summarise(
    value = mean(per_capita_income, na.rm = TRUE),
    .groups = "drop"
  )

# 4. AGGREGATE UNEMPLOYMENT RATE
unemp_natl <- acs_df %>%
  group_by(year) %>%
  summarise(
    value = mean(unemployment_rate, na.rm = TRUE),
    .groups = "drop"
  )

# 5. UI: dropdown to pick the series, slider to pick years
years_all <- sort(unique(c(gdp_natl$year, inc_natl$year, unemp_natl$year)))

ui <- fluidPage(
  titlePanel("Time Series: GDP, Income & Unemployment Rate"),
  sidebarLayout(
    sidebarPanel(
      selectInput("series", "Choose series:",
                  choices  = c("GDP", "Per Capita Income", "Unemployment Rate"),
                  selected = "GDP"),
      sliderInput("years", "Year range:",
                  min   = min(years_all),
                  max   = max(years_all),
                  value = c(min(years_all), max(years_all)),
                  sep   = ""
      )
    ),
    mainPanel(
      plotlyOutput("linePlot", height = "500px")
    )
  )
)

# 6. SERVER: filter & plot the chosen series
server <- function(input, output, session) {
  df_sel <- reactive({
    yrs <- input$years
    df <- switch(input$series,
                 "GDP"                = gdp_natl,
                 "Per Capita Income"  = inc_natl,
                 "Unemployment Rate"  = unemp_natl
    )
    df %>% filter(year >= yrs[1], year <= yrs[2])
  })
  
  output$linePlot <- renderPlotly({
    df <- df_sel()
    plot_ly(df, x = ~year, y = ~value,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = paste(input$series, "over time"),
        xaxis = list(title = "Year"),
        yaxis = list(title = input$series)
      )
  })
}

# 7. RUN APP
shinyApp(ui, server)

