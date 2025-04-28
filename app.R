
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(sf)
library(shinyWidgets)
library(shinycssloaders)  


## To keep tidytable from blowing things up
library(dplyr)
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
group_by <- dplyr::group_by
summarise <- dplyr::summarise
left_join <- dplyr::left_join


## Load data
fedcon <- readRDS("data/smallcon.rds")
state_gdp <- readRDS("data/state_gdp.rds")
county_gdp <- readRDS("data/county_gdp.rds")
sbcs <- readRDS("data/sbcs.rds")
acs_summary <- readRDS("data/acs_summary.rds")
state_sf <- readRDS("data/state_sf.rds")
county_sf <- readRDS("data/county_sf.rds") %>%
  select(GEOID, NAME, STATEFP, geometry) %>%
  st_simplify(dTolerance = 100)  # <- simplifies geometry

state_gdp_clean <- state_gdp %>%
  select(year, state_abbr = state_abbr.x, gdp = value)

## Pre-processing large files
# Summarized obligations by state-year
state_obligation_summary <- fedcon %>%
  group_by(state, fiscal_year) %>%
  summarise(total_obligation = sum(total_obligation, na.rm = TRUE), .groups = "drop")

# Merge state GDP
state_summary_joined <- state_obligation_summary %>%
  left_join(state_gdp, by = c("state" = "state_abbr.x", "fiscal_year" = "year")) %>%
  mutate(
    pct_gdp = total_obligation / (value * 1e6)
  )

county_obligation_summary <- fedcon %>%
  group_by(county_fips, fiscal_year) %>%
  summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop")



# Pre-join county shapefile with ACS and Small Biz contract data
econ_summary_joined <- county_sf %>%
  left_join(acs_summary, by = c("GEOID" = "geo_fips")) %>%
  left_join(
    fedcon %>%
      filter(is_small_business == TRUE) %>%
      group_by(county_fips, fiscal_year) %>%
      summarise(total_smallbiz_obligation = sum(total_obligation, na.rm = TRUE), .groups = "drop"),
    by = c("GEOID" = "county_fips")
  ) %>%
  mutate(
    contracts_per_capita = total_smallbiz_obligation / total_population,
    contracts_per_capita = ifelse(is.finite(contracts_per_capita), contracts_per_capita, NA)
  )

# --- UI Layout ---
ui <- dashboardPage(
  dashboardHeader(title = "Federal Contracts Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "filters", icon = icon("sliders-h")),
      selectInput("year", "Select Year", choices = sort(unique(fedcon$fiscal_year)), selected = 2023),
      pickerInput("agency", "Select Agencies:", 
                  choices = sort(unique(fedcon$parent_agency)), multiple = TRUE,
                  options = list(`actions-box` = TRUE), selected = unique(fedcon$parent_agency)),
      pickerInput("naics_group", "Select NAICS Groups:",
                  choices = sort(unique(fedcon$naics_group)), multiple = TRUE,
                  options = list(`actions-box` = TRUE), selected = unique(fedcon$naics_group)),
      selectInput("minority", "Minority Owned", choices = c("All", "Yes", "No")),
      selectInput("woman", "Woman Owned", choices = c("All", "Yes", "No")),
      selectInput("veteran", "Veteran Owned", choices = c("All", "Yes", "No"))
    )
  ),
  
  dashboardBody(
    fluidRow(
      valueBoxOutput("totalOblig") %>% withSpinner(color = "#007bff"),
      valueBoxOutput("gdp") %>% withSpinner(color = "#007bff"),
      valueBoxOutput("pctGDP") %>% withSpinner(color = "#007bff")
    ),
    
    fluidRow(
      box(title = "Federal Contract Obligations Map", width = 12,
          leafletOutput("map") %>% withSpinner(color = "#007bff"),
          actionButton("reset_view", "Reset to National View")
      )
    ),
    
    fluidRow(
      tabBox(
        id = "trends_tabs",
        title = "Data Explorer",
        width = 12,
        
        ## Distribution Explorer Tab
        tabPanel("Distribution Explorer",
                 fluidRow(
                   column(6,
                          selectInput("dist_type", "Explore By:", 
                                      choices = c("Agency", "NAICS Group")),
                          uiOutput("dist_choice"))
                 ),
                 fluidRow(
                   plotOutput("distPlot") %>% withSpinner(color = "#007bff"))
                
                  ),
        
        ## Economic Indicators Tab
        fluidRow(
          column(4,
                 selectInput("econ_variable", "Select Economic Variable:", 
                             choices = c("Median Income", "Poverty Rate", 
                                         "Small Biz Contracts"),
                             selected = "Median Income"))
                ),
        
        fluidRow(
          box(title = "County-Level Economic Map", width = 12, 
              leafletOutput("econ_map") %>% withSpinner(color = "#007bff"))
        ),
                 
                 # (Optional) Add employment trends / other stuff after
        ),
        
        ## Obligations Over Time Tab
        tabPanel("Obligations Over Time",
                 plotOutput("trendPlot") %>% withSpinner(color = "#007bff")
        ),
        
        ## Breakdown by Industry Tab
        tabPanel("Breakdown by Industry",
                 plotOutput("barPlot") %>% withSpinner(color = "#007bff")
        ),
        
        ## Top NAICS Groups Over Time Tab
        tabPanel("Top NAICS Groups Over Time",
                 plotOutput("naicsTrendPlot") %>% withSpinner(color = "#007bff")
        ),
        
        ## Top Agencies Over Time Tab
        tabPanel("Top Agencies Over Time",
                 plotOutput("agencyTrendPlot") %>% withSpinner(color = "#007bff")
        ),
        
        ## Small Business Credit Survey Tab
        tabPanel("Small Business Credit Survey",
                 fluidRow(
                   column(4,
                          selectInput("survey_year", "Select Survey Year:", choices = NULL),
                          selectInput("survey_state", "Select State (optional):", choices = NULL)
                   )
                 ),
                 fluidRow(
                   box(title = "Revenue Changes", width = 6, plotOutput("revenuePlot") %>% withSpinner(color = "#007bff")),
                   box(title = "Employment Changes", width = 6, plotOutput("employmentPlot") %>% withSpinner(color = "#007bff"))
                 ),
                 fluidRow(
                   box(title = "Financing Access", width = 12, plotOutput("financingPlot") %>% withSpinner(color = "#007bff"))
               )
             )
           ) 
         )
       )
     )
  


## Server start
server <- function(input, output, session) {
  
  drilldown_mode <- reactiveVal("states")
  clicked_state <- reactiveVal(NULL)
  
## Filtered contract data and summaries
  filtered_data <- reactive({
    df <- fedcon %>% filter(fiscal_year == input$year)
    if (!is.null(input$agency)) df <- df %>% 
        filter(parent_agency %in% input$agency)
    if (!is.null(input$naics_group)) df <- df %>% 
        filter(naics_group %in% input$naics_group)
    if (input$minority != "All") df <- df %>% 
        filter(is_minority_owned == (input$minority == "Yes"))
    if (input$woman != "All") df <- df %>% 
        filter(is_woman_owned == (input$woman == "Yes"))
    if (input$veteran != "All") df <- df %>% 
        filter(is_veteran_owned == (input$veteran == "Yes"))
    if (!is.null(clicked_state())) {
      df <- df %>% 
        filter(state == (state_sf %>% filter(STATEFP == clicked_state()) %>% 
                           pull(STUSPS)))
    }
    df
  })

## American Community Survey Filter
  filtered_acs <- reactive({
    df <- acs_summary %>% filter(year == input$acs_year)
    if (input$acs_state != "All") {
      df <- df %>% filter(state == input$acs_state)
    }
    df
  })
  
## Small Business Credit Survey (SBCS) filter  
  filtered_survey <- reactive({
    req(input$Year)   # <-- Add this line to block errors
    
    df <- sbcs %>% filter(year == input$Year)
    
    if (input$survey_state != "All") {
      df <- df %>% filter(state == input$survey_state)
    }
    
    df
  })
  
## Reactive contract data for economic tab
  filtered_smallcon <- reactive({
    df <- fedcon %>%
      filter(fiscal_year == input$acs_year,  # Match selected year
             is_small_business == TRUE)
    
    df %>%
      group_by(county_fips) %>%
      summarise(total_small_contracts = sum(total_obligation, na.rm = TRUE)) %>%
      ungroup()
  })
  
  state_summary <- reactive({
    year_selected <- input$year
    
    merged <- state_summary_joined %>%
      filter(fiscal_year == year_selected)
    
    left_join(state_sf, merged, by = c("STUSPS" = "state"))
  })
  
  # County map reactive
  county_summary_data <- reactive({
    req(clicked_state(), input$year)
    
    county_sf %>%
      left_join(
        county_obligation_summary %>% filter(fiscal_year == input$year),
        by = c("GEOID" = "county_fips")
      ) %>%
      left_join(
        county_gdp %>% filter(year == input$year),
        by = c("GEOID" = "geo_fips")
      ) %>%
      mutate(
        gdp_dollars = gdp_millions * 1e6,
        pct_gdp = total / gdp_dollars
      ) %>%
      filter(STATEFP == clicked_state())
  })
  
  # Economic indicators map reactive
  econ_summary_data <- reactive({
    req(input$acs_year)
    
    county_sf %>%
      left_join(
        acs_summary %>% filter(year == input$acs_year),
        by = c("GEOID" = "geo_fips")
      ) %>%
      left_join(
        fedcon %>%
          filter(fiscal_year == input$acs_year, is_small_business == TRUE) %>%
          group_by(county_fips) %>%
          summarise(total_smallbiz_obligation = sum(total_obligation, na.rm = TRUE), .groups = "drop"),
        by = c("GEOID" = "county_fips")
      ) %>%
      mutate(
        contracts_per_capita = total_smallbiz_obligation / total_population,
        contracts_per_capita = ifelse(is.finite(contracts_per_capita), contracts_per_capita, NA)
      )
  })
  
  ## Main outputs: Map, value boxes, plots
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  observe({
    if (drilldown_mode() == "states") {
      df <- state_summary()
      pal <- colorQuantile("plasma", 
                           domain = df$pct_gdp, 
                           n = 5, 
                           na.color = "#cccccc")
      
      leafletProxy("map", session) %>%
        clearShapes() %>%
        addPolygons(
          data = df,
          fillColor = ~pal(pct_gdp), color = "white", weight = 1,
          opacity = 1, fillOpacity = 0.7,
          label = ~paste0(NAME, "<br>Total Obligations: ", 
                          scales::dollar(total_obligation),
                          "<br>GDP: ", scales::dollar(gdp_dollars),
                          "<br>Obligations as % of GDP: ", 
                          scales::percent(pct_gdp, accuracy = 0.1)),
          layerId = ~STATEFP
        )
    } else if (drilldown_mode() == "counties") {
      df <- county_summary()
      pal <- colorQuantile("plasma", 
                           domain = df$pct_gdp, 
                           n = 5, 
                           na.color = "#cccccc")
      
      leafletProxy("map", session) %>%
        clearShapes() %>%
        addPolygons(
          data = df,
          fillColor = ~pal(pct_gdp), color = "white", weight = 1,
          opacity = 1, fillOpacity = 0.7,
          label = ~paste0(NAME, "<br>Total: ", scales::dollar(total),
                          "<br>GDP: ", scales::dollar(gdp_dollars),
                          "<br>%GDP: ", scales::percent(pct_gdp, accuracy = 0.1)),
          layerId = ~GEOID
        )
    }
  })
  
  observeEvent(input$map_shape_click, {
    if (drilldown_mode() == "states") {
      clicked_state(input$map_shape_click$id)
      drilldown_mode("counties")
    }
  })
  
  observeEvent(county_summary(), {
    if (drilldown_mode() == "counties" && nrow(county_summary()) > 0) {
      bbox <- st_bbox(county_summary())
      leafletProxy("map") %>%
        flyToBounds(
          lng1 = bbox$xmin, lat1 = bbox$ymin,
          lng2 = bbox$xmax, lat2 = bbox$ymax
        )
    }
  })
  
  observeEvent(input$reset_view, {
    clicked_state(NULL)
    drilldown_mode("states")
    leafletProxy("map") %>%
      flyTo(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  ## Value Boxes: dynamic state  
  output$totalOblig <- renderValueBox({
    total <- sum(filtered_data()$total_obligation, na.rm = TRUE)
    
    valueBox(
      value = scales::dollar(total),
      subtitle = "Total Obligations",
      icon = icon("file-invoice-dollar"),
      color = "blue"
    )
  })
  
  output$gdp <- renderValueBox({
    gdp_value <- state_gdp_clean %>%
      filter(year == input$year) %>%
      pull(gdp)
    
    gdp_value <- ifelse(length(gdp_value) == 1 && !is.na(gdp_value), gdp_value * 1e6, NA_real_)
    
    valueBox(
      value = if (!is.na(gdp_value)) scales::dollar(gdp_value) else "No Data",
      subtitle = "Total GDP",
      icon = icon("landmark"),
      color = "green"
    )
  })
  
  output$pctGDP <- renderValueBox({
    total_oblig <- sum(filtered_data()$total_obligation, na.rm = TRUE)
    
    gdp_value <- state_gdp_clean %>%
      filter(year == input$year) %>%
      pull(gdp)
    
    gdp_value <- ifelse(length(gdp_value) == 1 && !is.na(gdp_value), gdp_value * 1e6, NA_real_)
    
    pct <- if (!is.na(gdp_value) && gdp_value > 0) total_oblig / gdp_value else NA_real_
    
    valueBox(
      value = if (!is.na(pct)) scales::percent(pct, accuracy = 0.01) else "No Data",
      subtitle = "Obligations as % GDP",
      icon = icon("percentage"),
      color = "orange"
    )
  })
  
  ## Dynamic dropdown for Agency or NAICS
  output$dist_choice <- renderUI({
    if (input$dist_type == "Agency") {
      pickerInput("dist_choice_value", "Select Agency:", 
                  choices = sort(unique(fedcon$parent_agency)), multiple = FALSE,
                  options = list(`live-search` = TRUE))
    } else {
      pickerInput("dist_choice_value", "Select NAICS Group:", 
                  choices = sort(unique(fedcon$naics_group)), multiple = FALSE,
                  options = list(`live-search` = TRUE))
    }
  })
  
  ## Plots: Distribution Plot
  output$distPlot <- renderPlot({
    req(input$dist_choice_value)
    
    df <- fedcon
    
    if (input$dist_type == "Agency") {
      df <- df %>% filter(parent_agency == input$dist_choice_value)
    } else {
      df <- df %>% filter(naics_group == input$dist_choice_value)
    }
    
    state_summary <- df %>%
      group_by(state) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice_head(n = 10)
    
    ggplot(state_summary, aes(x = reorder(state, total), y = total)) +
      geom_col(fill = "#1F77B4") +
      coord_flip() +
      labs(title = "Top States by Obligations", 
           x = "State", 
           y = "Total Obligations ($)") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  ## Plots: dynamic state
  # Obligations Over Time
  output$trendPlot <- renderPlot({
    df_national <- fedcon
    
    if (!is.null(input$agency)) df_national <- df_national %>% 
        filter(parent_agency %in% input$agency)
    if (!is.null(input$naics_group)) df_national <- df_national %>% 
        filter(naics_group %in% input$naics_group)
    if (input$minority != "All") df_national <- df_national %>% 
        filter(is_minority_owned == (input$minority == "Yes"))
    if (input$woman != "All") df_national <- df_national %>% 
        filter(is_woman_owned == (input$woman == "Yes"))
    if (input$veteran != "All") df_national <- df_national %>% 
        filter(is_veteran_owned == (input$veteran == "Yes"))
    
    df_national <- df_national %>%
      group_by(fiscal_year) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), 
                .groups = "drop") %>%
      mutate(scope = "National")
    
    df_state <- fedcon
    
    if (!is.null(input$agency)) df_state <- df_state %>% 
      filter(parent_agency %in% input$agency)
    if (!is.null(input$naics_group)) df_state <- df_state %>% 
      filter(naics_group %in% input$naics_group)
    if (input$minority != "All") df_state <- df_state %>% 
      filter(is_minority_owned == (input$minority == "Yes"))
    if (input$woman != "All") df_state <- df_state %>% 
      filter(is_woman_owned == (input$woman == "Yes"))
    if (input$veteran != "All") df_state <- df_state %>% 
      filter(is_veteran_owned == (input$veteran == "Yes"))
    
    if (!is.null(clicked_state())) {
      df_state <- df_state %>% 
        filter(state == (state_sf %>% filter(STATEFP == clicked_state()) %>% 
                           pull(STUSPS)))
    }
    
    df_state <- df_state %>%
      group_by(fiscal_year) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), 
                .groups = "drop") %>%
      mutate(scope = "Selected State")
    
    df_combined <- bind_rows(df_national, df_state)
    
    ggplot(df_combined, aes(x = fiscal_year, 
                            y = total, 
                            color = scope, 
                            group = scope)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("National" = "gray60", 
                                    "Selected State" = "#2C3E50")) +
      labs(title = "Federal Obligations Over Time", 
           x = "Year", 
           y = "Total Obligations ($)", 
           color = "") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  ## Breakdown by industry
  output$barPlot <- renderPlot({
    df <- filtered_data() %>%
      group_by(naics_group) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), 
                .groups = "drop") %>%
      slice_max(total, n = 10)
    
    ggplot(df, aes(x = reorder(naics_group, total), y = total)) +
      geom_col(fill = "#1F77B4") +
      coord_flip() +
      labs(title = "Top NAICS Groups", 
           x = "NAICS Group", 
           y = "Total Obligations ($)") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  ## Top NAICS Groups over time
  output$naicsTrendPlot <- renderPlot({
    df <- fedcon
    
    if (!is.null(input$agency)) df <- df %>% 
        filter(parent_agency %in% input$agency)
    if (!is.null(input$naics_group)) df <- df %>% 
        filter(naics_group %in% input$naics_group)
    if (input$minority != "All") df <- df %>% 
        filter(is_minority_owned == (input$minority == "Yes"))
    if (input$woman != "All") df <- df %>% 
        filter(is_woman_owned == (input$woman == "Yes"))
    if (input$veteran != "All") df <- df %>% 
        filter(is_veteran_owned == (input$veteran == "Yes"))
    
    if (!is.null(clicked_state())) {
      df <- df %>% 
        filter(state == (state_sf %>% 
                           filter(STATEFP == clicked_state()) %>% 
                           pull(STUSPS)))
    }
    
    df_plot <- df %>%
      group_by(naics_group, fiscal_year) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), 
                .groups = "drop") %>%
      filter(!is.na(naics_group))
    
    top_naics <- df_plot %>%
      group_by(naics_group) %>%
      summarise(total_all = sum(total)) %>%
      arrange(desc(total_all)) %>%
      slice_head(n = 5) %>%
      pull(naics_group)
    
    df_plot <- df_plot %>% filter(naics_group %in% top_naics)
    
    ggplot(df_plot, aes(x = fiscal_year, y = total, color = naics_group)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(title = "Top NAICS Groups Over Time", 
           x = "Year", 
           y = "Obligations ($)", 
           color = "NAICS Group") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  ## Agency trend plot
  output$agencyTrendPlot <- renderPlot({
    df <- fedcon
    
    # Apply same filters
    if (!is.null(input$agency)) df <- df %>% 
        filter(parent_agency %in% input$agency)
    if (!is.null(input$naics_group)) df <- df %>% 
        filter(naics_group %in% input$naics_group)
    if (input$minority != "All") df <- df %>% 
        filter(is_minority_owned == (input$minority == "Yes"))
    if (input$woman != "All") df <- df %>% 
        filter(is_woman_owned == (input$woman == "Yes"))
    if (input$veteran != "All") df <- df %>% 
        filter(is_veteran_owned == (input$veteran == "Yes"))
    
    # Filter to state if one is clicked
    if (!is.null(clicked_state())) {
      df <- df %>% 
        filter(state == (state_sf %>% 
                           filter(STATEFP == clicked_state()) %>% 
                           pull(STUSPS)))
    }
    
    df_plot <- df %>%
      group_by(parent_agency, fiscal_year) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), 
                .groups = "drop") %>%
      filter(!is.na(parent_agency))
    
    top_agencies <- df_plot %>%
      group_by(parent_agency) %>%
      summarise(total_all = sum(total)) %>%
      arrange(desc(total_all)) %>%
      slice_head(n = 5) %>%
      pull(parent_agency)
    
    df_plot <- df_plot %>% filter(parent_agency %in% top_agencies)
    
    ggplot(df_plot, aes(x = fiscal_year, y = total, color = parent_agency)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(title = "Top Agencies Over Time", 
           x = "Year", 
           y = "Obligations ($)", 
           color = "Agency") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  ## Dynamic dropdown for Agency or NAICS
  output$dist_choice <- renderUI({
    if (input$dist_type == "Agency") {
      pickerInput("dist_choice_value", "Select Agency:", 
                  choices = sort(unique(fedcon$parent_agency)), multiple = FALSE,
                  options = list(`live-search` = TRUE))
    } else {
      pickerInput("dist_choice_value", "Select NAICS Group:", 
                  choices = sort(unique(fedcon$naics_group)), multiple = FALSE,
                  options = list(`live-search` = TRUE))
    }
  })
  
  # Distribution Plot
  output$distPlot <- renderPlot({
    req(input$dist_choice_value)
    
    df <- fedcon
    
    # Apply agency or naics filter
    if (input$dist_type == "Agency") {
      df <- df %>% filter(parent_agency == input$dist_choice_value)
    } else {
      df <- df %>% filter(naics_group == input$dist_choice_value)
    }
    
    # State-level summary
    state_summary <- df %>%
      group_by(state) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice_head(n = 10)
    
    ggplot(state_summary, aes(x = reorder(state, total), y = total)) +
      geom_col(fill = "#1F77B4") +
      coord_flip() +
      labs(title = "Top States by Obligations", 
           x = "State", 
           y = "Total Obligations ($)") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
## ACS Plots
  # --- Unified Economic Indicators Map (final version) ---
  
  # Output for Economic Indicators Map
  output$econ_map <- renderLeaflet({
    df <- econ_summary_data()
    
    pal_income <- colorQuantile("Greens", domain = df$median_household_income, n = 5, na.color = "#cccccc")
    
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal_income(median_household_income),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~paste0(NAME, "<br>",
                        "Median Income: ", scales::dollar(median_household_income),
                        "<br>Contracts per Capita: ", scales::percent(contracts_per_capita, accuracy = 0.1))
      ) %>%
      addLegend(
        pal = pal_income,
        values = ~median_household_income,
        title = "Median Household Income",
        position = "bottomright"
      )
  })

## SBCS Revenue Plot   
  output$revenuePlot <- renderPlot({
    df <- filtered_survey()
    df %>%
      count(revenue_change) %>%
      mutate(pct = n / sum(n)) %>%
      ggplot(aes(x = revenue_change, y = pct)) +
      geom_col(fill = "#1f77b4") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Revenue Change", y = "% of Businesses", title = "Reported Revenue Changes") +
      theme_minimal()
  })
  
## SBCS Employment Change Plot
  output$employmentPlot <- renderPlot({
    df <- filtered_survey()
    df %>%
      count(employment_change) %>%
      mutate(pct = n / sum(n)) %>%
      ggplot(aes(x = employment_change, y = pct)) +
      geom_col(fill = "#2ca02c") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Employment Change", 
           y = "% of Businesses", 
           title = "Reported Employment Changes") +
      theme_minimal()
  })
  
## SBCS Financing Access Plot
  output$financingPlot <- renderPlot({
    df <- filtered_survey()
    df %>%
      count(financing_access) %>%
      mutate(pct = n / sum(n)) %>%
      ggplot(aes(x = financing_access, y = pct)) +
      geom_col(fill = "#ff7f0e") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Financing Outcome", 
           y = "% of Applicants", 
           title = "Reported Financing Access") +
      theme_minimal()
  })
 
  
  ## Tab memory
  observeEvent(clicked_state(), {
    if (!is.null(input$trends_tabs)) {
      updateTabsetPanel(session, "trends_tabs", selected = input$trends_tabs)
    }
  })
  ## Close Server
}

## Launch App
shinyApp(ui = ui, server = server)
