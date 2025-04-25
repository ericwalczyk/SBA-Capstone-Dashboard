rm(list=ls())

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)

########### load & clean data

# load all contracts
allfedcon <- read_csv("all_fedcon.csv")

allfedcon$fiscal_year <- as.integer(allfedcon$fiscal_year)
allfedcon <- allfedcon[allfedcon$fiscal_year <= 2023, ]
county_year_summary <- aggregate(total_obligation ~ county_fips + fiscal_year + state, data = allfedcon, sum)
county_year_summary$dataset <- "All contracts"

# load small contracts only
allfedcon_small <- read_csv("small_contracts.csv")

allfedcon_small$fiscal_year <- as.integer(allfedcon_small$fiscal_year)
allfedcon_small <- allfedcon_small[allfedcon_small$fiscal_year <= 2023, ]
smallbiz_summary <- aggregate(total_obligation ~ county_fips + fiscal_year + state, data = allfedcon_small, sum)
smallbiz_summary$dataset <- "Small contracts (≤ $250k)"

# combine both contract datasets
contract_data <- rbind(county_year_summary, smallbiz_summary)

# load gdp data
county_gdp <- read_csv("county_gdp.csv")
county_gdp <- county_gdp[county_gdp$geo_fips != "00000" & county_gdp$description == "All industry total", ]
county_gdp$fiscal_year <- as.integer(county_gdp$year)

# load business applications
bfs <- read_csv("bfs_county_apps_annual.csv")
bfs <- bfs[, !(names(bfs) %in% as.character(2005:2016))]
bfs_long <- pivot_longer(bfs, cols = matches("^201[7-9]$|^202[0-5]$"), names_to = "fiscal_year", values_to = "business_apps")
bfs_long$fiscal_year <- as.integer(bfs_long$fiscal_year)
bfs_long$county_fips <- as.character(bfs_long$`County Code`)
bfs_long <- bfs_long[, c("county_fips", "fiscal_year", "business_apps", "County")]

# merge everything
merged_data <- merge(contract_data, county_gdp, by.x = c("county_fips", "fiscal_year"), by.y = c("geo_fips", "fiscal_year"), all.x = TRUE)
merged_data <- merge(merged_data, bfs_long, by = c("county_fips", "fiscal_year"), all.x = TRUE)
merged_data <- merged_data[!(merged_data$state %in% c("AS", "GU", "PR", "MP", "VI")), ]

######### ui
ui <- fluidPage(
  titlePanel("County Level Federal Spending Correlation Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("contract_type", "Contract Type:",
                  choices = c("All contracts", "Small contracts (≤ $250k)"),
                  selected = "All contracts"),
      
      selectInput("selected_year", "Select Fiscal Year:",
                  choices = sort(unique(merged_data$fiscal_year)),
                  selected = max(merged_data$fiscal_year)),
      
      selectInput("selected_state", "Select State:",
                  choices = c("All states", sort(unique(merged_data$state))),
                  selected = "All states"),
      
      selectInput("y_var", "Select Y-axis Variable:",
                  choices = c("County GDP (Millions)" = "gdp_millions",
                              "New Business Applications" = "business_apps"),
                  selected = "gdp_millions"), 
      
      selectInput("display_mode", "Display Mode:",
                  choices = c("Raw values", "Percentiles"),
                  selected = "Raw values")
    ),
    
    mainPanel(
      plotlyOutput("corPlot"),
      br(),
      conditionalPanel(
        condition = "input.display_mode == 'Percentiles' && input.y_var == 'gdp_millions'",
        p("Counties above the line were underrepresented in federal contracting relative to their economic size."),
        p("Counties beneath the line were overrepresented in federal contracting relative to their economic size.")
      ),
      conditionalPanel(
        condition = "input.display_mode == 'Percentiles' && input.y_var == 'business_apps'",
        p("Counties above the line were underrepresented in federal contracting relative to their number of new business applications."),
        p("Counties beneath the line were overrepresented in federal contracting relative to their number of new business applications.")
      )
    )  
  )   
)    

########### server
server <- function(input, output) {
  output$corPlot <- renderPlotly({
    filtered_data <- merged_data[merged_data$fiscal_year == input$selected_year &
                                   merged_data$dataset == input$contract_type &
                                   !is.na(merged_data$total_obligation) &
                                   !is.na(merged_data[[input$y_var]]), ]
    
    if (input$selected_state != "All states") {
      filtered_data <- filtered_data[filtered_data$state == input$selected_state, ]
    }
    
    if (nrow(filtered_data) > 0) {
      if (input$selected_state == "All states") {
        # National percentiles
        obligation_ecdf <- ecdf(merged_data$total_obligation[merged_data$fiscal_year == input$selected_year &
                                                               merged_data$dataset == input$contract_type &
                                                               !is.na(merged_data$total_obligation)])
        
        y_ecdf <- ecdf(merged_data[[input$y_var]][merged_data$fiscal_year == input$selected_year &
                                                    merged_data$dataset == input$contract_type &
                                                    !is.na(merged_data[[input$y_var]])])
      } else {
        # State-level percentiles
        obligation_ecdf <- ecdf(filtered_data$total_obligation)
        y_ecdf <- ecdf(filtered_data[[input$y_var]])
      }
      
      filtered_data$obligation_percentile <- obligation_ecdf(filtered_data$total_obligation)
      filtered_data$y_percentile <- y_ecdf(filtered_data[[input$y_var]])
    } else {
      return(plotly_empty() %>% layout(title = "No data available for this selection"))
    }
    
    if (input$display_mode == "Raw values") {
      x_var <- filtered_data$total_obligation
      y_var <- filtered_data[[input$y_var]]
      x_label <- "Total Federal Contract Obligations (USD)"
      y_label <- ifelse(input$y_var == "gdp_millions", "County GDP (Millions USD)", "New Business Applications")
      x_format <- comma
      y_format <- comma
      filtered_data$hover_text <- paste0(
        filtered_data$County,
        "<br>Obligation: $", comma(filtered_data$total_obligation),
        "<br>", ifelse(input$y_var == "gdp_millions", "GDP: $", "Business Apps: "),
        comma(filtered_data[[input$y_var]])
      )
    } else {
      x_var <- filtered_data$obligation_percentile
      y_var <- filtered_data$y_percentile
      context_label <- ifelse(input$selected_state == "All states", "Nation", "State")
      x_label <- paste("County Percentile in", context_label, "for Federal Obligations")
      y_label <- ifelse(input$y_var == "gdp_millions",
                        paste("County Percentile in", context_label, "for GDP"),
                        paste("County Percentile in", context_label, "for Business Applications"))
      x_format <- percent_format(accuracy = 1)
      y_format <- percent_format(accuracy = 1)
      filtered_data$hover_text <- paste0(
        filtered_data$County,
        "<br>Obligation Percentile: ", percent(filtered_data$obligation_percentile, accuracy = 1),
        "<br>", ifelse(input$y_var == "gdp_millions", "GDP Percentile: ", "Business Apps Percentile: "),
        percent(filtered_data$y_percentile, accuracy = 1)
      )
    }
    
    p <- ggplot(filtered_data, aes(
      x = x_var,
      y = y_var,
      text = filtered_data$hover_text
    )) +
      geom_point(alpha = 0.6, color = "steelblue", size = 1.5) +
      labs(
        x = x_label,
        y = y_label,
        title = paste("Year:", input$selected_year,
                      "| State:", input$selected_state,
                      "|", input$contract_type)
      ) +
      scale_x_continuous(labels = x_format) +
      scale_y_continuous(labels = y_format) +
      theme_minimal()
    
    if (input$display_mode == "Percentiles") {
      p <- p + geom_abline(intercept = 0, slope = 1, color = "lightgray", linetype = "dashed", linewidth = 0.5)
    }
    
    ggplotly(p, tooltip = "text")
  })
}

# === Run App ===
shinyApp(ui = ui, server = server)

