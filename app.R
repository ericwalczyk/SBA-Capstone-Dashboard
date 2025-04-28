# --- 0) Load Libraries ---
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(sf)
library(shinyWidgets)
library(shinycssloaders)

## For tidytable compatibility
library(dplyr)
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
group_by <- dplyr::group_by
summarise <- dplyr::summarise
left_join <- dplyr::left_join

# --- 1) Load and Preprocess Data ---
fedcon <- readRDS("data/smallcon.rds")
state_gdp <- readRDS("data/state_gdp.rds")
county_gdp <- readRDS("data/county_gdp.rds")
sbcs <- readRDS("data/sbcs.rds")
acs_summary <- readRDS("data/acs_summary.rds")
state_sf <- readRDS("data/state_sf.rds")
county_sf <- readRDS("data/county_sf.rds") %>%
  select(GEOID, NAME, STATEFP, geometry) %>%
  st_simplify(dTolerance = 100)

state_gdp_clean <- state_gdp %>%
  select(year, state_abbr = state_abbr.x, gdp = value)

state_obligation_summary <- fedcon %>%
  group_by(state, fiscal_year) %>%
  summarise(total_obligation = sum(total_obligation, na.rm = TRUE), .groups = "drop")

state_summary_joined <- state_obligation_summary %>%
  left_join(state_gdp, by = c("state" = "state_abbr.x", "fiscal_year" = "year")) %>%
  mutate(pct_gdp = total_obligation / (value * 1e6))

county_obligation_summary <- fedcon %>%
  group_by(county_fips, fiscal_year) %>%
  summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop")


# --- 3) UI ---
ui <- dashboardPage(
  dashboardHeader(title = "Federal Contracts Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "filters", icon = icon("sliders-h")),
      selectInput("year", "Select Year", choices = sort(unique(fedcon$fiscal_year)), selected = 2023),
      pickerInput("agency", "Select Agencies:", choices = sort(unique(fedcon$parent_agency)), multiple = TRUE, options = list(`actions-box` = TRUE), selected = unique(fedcon$parent_agency)),
      pickerInput("naics_group", "Select NAICS Groups:", choices = sort(unique(fedcon$naics_group)), multiple = TRUE, options = list(`actions-box` = TRUE), selected = unique(fedcon$naics_group)),
      selectInput("minority", "Minority Owned", choices = c("All", "Yes", "No")),
      selectInput("woman", "Woman Owned", choices = c("All", "Yes", "No")),
      selectInput("veteran", "Veteran Owned", choices = c("All", "Yes", "No"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-sidebar .dropdown-menu {
          z-index: 9999 !important;
        }
      "))
    ),  
    
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
        
        tabPanel("Distribution Explorer",
                 fluidRow(
                   column(6,
                          selectInput("dist_type", "Explore By:", choices = c("Agency", "NAICS Group")),
                          uiOutput("dist_choice"))
                 ),
                 fluidRow(
                   plotOutput("distPlot") %>% withSpinner(color = "#007bff")
                 )
        ),
        
        tabPanel("Obligations Over Time",
                 plotOutput("trendPlot") %>% withSpinner(color = "#007bff")
        ),
        
        tabPanel("Breakdown by Industry",
                 plotOutput("barPlot") %>% withSpinner(color = "#007bff")
        ),
        
        tabPanel("Top NAICS Groups Over Time",
                 plotOutput("naicsTrendPlot") %>% withSpinner(color = "#007bff")
        ),
        
        tabPanel("Top Agencies Over Time",
                 plotOutput("agencyTrendPlot") %>% withSpinner(color = "#007bff")
        ),
        
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
# --- 4) Server ---
server <- function(input, output, session) {
  drilldown_mode <- reactiveVal("states")
  clicked_state <- reactiveVal(NULL)
  
  
  legend_title <- reactive({
    if (input$trends_tabs == "Distribution Explorer") {
      "% GDP Obligations"
    } else if (input$econ_variable == "Median Income") {
      "Median Household Income ($)"
    } else if (input$econ_variable == "Poverty Rate") {
      "Poverty Rate (%)"
    } else if (input$econ_variable == "Small Biz Contracts") {
      "Small Business Contracts ($)"
    } else {
      ""  # fallback
    }
  })
  
  # --- Helper Functions ---
  apply_all_filters <- function(df, input) {
    if (!is.null(input$agency)) {
      df <- df %>% filter(parent_agency %in% input$agency)
    }
    if (!is.null(input$naics_group)) {
      df <- df %>% filter(naics_group %in% input$naics_group)
    }
    if (input$minority != "All") {
      df <- df %>% filter(is_minority_owned == (input$minority == "Yes"))
    }
    if (input$woman != "All") {
      df <- df %>% filter(is_woman_owned == (input$woman == "Yes"))
    }
    if (input$veteran != "All") {
      df <- df %>% filter(is_veteran_owned == (input$veteran == "Yes"))
    }
    df
  }
  
  apply_summary_filters <- function(df, input) {
    if (!is.null(input$agency)) {
      df <- df %>% filter(parent_agency %in% input$agency)
    }
    if (!is.null(input$naics_group)) {
      df <- df %>% filter(naics_group %in% input$naics_group)
    }
    df
  }
  
  # --- Filtered data ---
  filtered_data <- reactive({
    df <- fedcon %>% filter(fiscal_year == input$year)
    apply_all_filters(df, input)
  })
  
  filtered_data_summary <- reactive({
    df <- fedcon %>% filter(fiscal_year == input$year)
    df <- apply_summary_filters(df, input)
    
    if (!is.null(clicked_state())) {
      df <- df %>% 
        filter(state == (state_sf %>% filter(STATEFP == clicked_state()) %>% pull(STUSPS)))
    }
    
    df
  })
  
  filtered_survey <- reactive({
    req(input$survey_year)
    df <- sbcs %>% filter(year == input$survey_year)
    if (input$survey_state != "All") df <- df %>% filter(state == input$survey_state)
    df
  })
  
  state_summary <- reactive({
    state_sf %>%
      left_join(
        state_summary_joined %>% filter(fiscal_year == input$year),
        by = c("STUSPS" = "state")
      ) %>%
      mutate(
        gdp_dollars = value * 1e6
      )
  })
  
  county_summary_data <- reactive({
    req(clicked_state(), input$year)
    county_sf %>%
      left_join(county_obligation_summary %>% filter(fiscal_year == input$year),
                by = c("GEOID" = "county_fips")) %>%
      left_join(county_gdp %>% filter(year == input$year),
                by = c("GEOID" = "geo_fips")) %>%
      mutate(
        gdp_dollars = gdp_millions * 1e6,
        pct_gdp = total / gdp_dollars
      ) %>%
      filter(STATEFP == clicked_state())
  })
  
  # --- Outputs ---
  
  # --- 4a) Map ---
  output$map <- renderLeaflet({ leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(lng = -98.5, lat = 39.8, zoom = 4) })
  
  observe({
    if (drilldown_mode() == "states") {
      df <- state_summary()
      pal <- colorQuantile("plasma", domain = df$pct_gdp, n = 5, na.color = "#cccccc")
      
      leafletProxy("map", session) %>%
        clearShapes() %>%
        addPolygons(
          data = df,
          fillColor = ~pal(pct_gdp),
          color = "white",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.7,
          label = ~paste0(NAME, 
                          "<br>Total Obligations: ", scales::dollar(total_obligation),
                          "<br>GDP: ", scales::dollar(gdp_dollars),
                          "<br>Obligations as % of GDP: ", scales::percent(pct_gdp, accuracy = 0.1)),
          layerId = ~STATEFP
        ) %>%
        addLegend(
          pal = pal,
          values = df$pct_gdp,
          title = legend_title(),    # <<-- DYNAMIC now!!
          position = "bottomright",
          labFormat = labelFormat(suffix = "%", transform = function(x) x * 100)
        )
      
    } else if (drilldown_mode() == "counties") {
      df <- county_summary_data()
      pal <- colorQuantile("plasma", domain = df$pct_gdp, n = 5, na.color = "#cccccc")
      
      leafletProxy("map", session) %>%
        clearShapes() %>%
        addPolygons(
          data = df,
          fillColor = ~pal(pct_gdp),
          color = "white",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.7,
          label = ~paste0(NAME, 
                          "<br>Total: ", scales::dollar(total),
                          "<br>GDP: ", scales::dollar(gdp_dollars),
                          "<br>% GDP: ", scales::percent(pct_gdp, accuracy = 0.1)),
          layerId = ~GEOID
        ) %>%
        addLegend(
          pal = pal,
          values = df$pct_gdp,
          title = "% GDP Obligations",
          position = "bottomright",
          labFormat = labelFormat(suffix = "%", transform = function(x) x * 100)
        )
    }
  })
  
  observeEvent(input$map_shape_click, {
    if (drilldown_mode() == "states") {
      clicked_state(input$map_shape_click$id)
      drilldown_mode("counties")
    }
  })
  
  observeEvent(county_summary_data(), {
    if (drilldown_mode() == "counties" && nrow(county_summary_data()) > 0) {
      bbox <- st_bbox(county_summary_data())
      leafletProxy("map") %>% flyToBounds(lng1 = bbox$xmin, lat1 = bbox$ymin, lng2 = bbox$xmax, lat2 = bbox$ymax)
    }
  })
  
  observeEvent(input$reset_view, {
    clicked_state(NULL)
    drilldown_mode("states")
    leafletProxy("map") %>% flyTo(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  # --- 4b) Value Boxes ---
  output$totalOblig <- renderValueBox({
    total <- sum(filtered_data_summary()$total_obligation, na.rm = TRUE)
    
    valueBox(
      value = scales::dollar(total),
      subtitle = if (is.null(clicked_state())) "Total Obligations" else "State Obligations",
      icon = icon("file-invoice-dollar"),
      color = "blue"
    )
  })
  
  output$gdp <- renderValueBox({
    if (is.null(clicked_state())) {
      # National GDP
      gdp_value <- state_gdp_clean %>%
        filter(year == input$year) %>%
        summarise(total_gdp = sum(gdp, na.rm = TRUE)) %>%
        pull(total_gdp)
    } else {
      # State GDP
      selected_state <- state_sf %>%
        filter(STATEFP == clicked_state()) %>%
        pull(STUSPS)
      
      gdp_value <- state_gdp_clean %>%
        filter(year == input$year, state_abbr == selected_state) %>%
        pull(gdp)
    }
    
    gdp_value <- ifelse(!is.na(gdp_value), gdp_value * 1e6, NA_real_)
    
    valueBox(
      value = if (!is.na(gdp_value)) scales::dollar(gdp_value) else "No Data",
      subtitle = if (is.null(clicked_state())) "Total GDP" else paste(selected_state, "GDP"),
      icon = icon("landmark"),
      color = "green"
    )
  })
  
  output$pctGDP <- renderValueBox({
    total_oblig <- sum(filtered_data_summary()$total_obligation, na.rm = TRUE)
    
    if (is.null(clicked_state())) {
      gdp_value <- state_gdp_clean %>%
        filter(year == input$year) %>%
        summarise(total_gdp = sum(gdp, na.rm = TRUE)) %>%
        pull(total_gdp)
    } else {
      selected_state <- state_sf %>%
        filter(STATEFP == clicked_state()) %>%
        pull(STUSPS)
      
      gdp_value <- state_gdp_clean %>%
        filter(year == input$year, state_abbr == selected_state) %>%
        pull(gdp)
    }
    
    gdp_value <- ifelse(!is.na(gdp_value), gdp_value * 1e6, NA_real_)
    
    pct <- if (!is.na(gdp_value) && gdp_value > 0) total_oblig / gdp_value else NA_real_
    
    valueBox(
      value = if (!is.na(pct)) scales::percent(pct, accuracy = 0.01) else "No Data",
      subtitle = "Obligations as % GDP",
      icon = icon("percentage"),
      color = "orange"
    )
  })
  
  # --- 4c) Distribution Explorer ---
  output$dist_choice <- renderUI({
    if (input$dist_type == "Agency") {
      pickerInput("dist_choice_value", "Select Agency:", choices = sort(unique(fedcon$parent_agency)), multiple = FALSE, options = list(`live-search` = TRUE))
    } else {
      pickerInput("dist_choice_value", "Select NAICS Group:", choices = sort(unique(fedcon$naics_group)), multiple = FALSE, options = list(`live-search` = TRUE))
    }
  })
  
  output$distPlot <- renderPlot({
    req(input$dist_choice_value)
    df <- fedcon
    df <- if (input$dist_type == "Agency") df %>% filter(parent_agency == input$dist_choice_value) else df %>% filter(naics_group == input$dist_choice_value)
    
    df %>%
      group_by(state) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE)) %>%
      slice_max(total, n = 10) %>%
      ggplot(aes(x = reorder(state, total), y = total)) +
      geom_col(fill = "#1F77B4") +
      coord_flip() +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  # --- Obligations Over Time ---
  output$trendPlot <- renderPlot({
    df <- fedcon_data()
    
    if (!is.null(input$agency)) df <- df %>% filter(parent_agency %in% input$agency)
    if (!is.null(input$naics_group)) df <- df %>% filter(naics_group %in% input$naics_group)
    if (input$minority != "All") df <- df %>% filter(is_minority_owned == (input$minority == "Yes"))
    if (input$woman != "All") df <- df %>% filter(is_woman_owned == (input$woman == "Yes"))
    if (input$veteran != "All") df <- df %>% filter(is_veteran_owned == (input$veteran == "Yes"))
    
    df_national <- df %>%
      group_by(fiscal_year) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      mutate(scope = "National")
    
    df_state <- df
    if (!is.null(clicked_state())) {
      df_state <- df_state %>%
        filter(state == (state_data() %>% filter(STATEFP == clicked_state()) %>% pull(STUSPS)))
    }
    df_state <- df_state %>%
      group_by(fiscal_year) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      mutate(scope = "Selected State")
    
    df_combined <- bind_rows(df_national, df_state)
    
    ggplot(df_combined, aes(x = fiscal_year, y = total, color = scope, group = scope)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(title = "Federal Obligations Over Time", x = "Year", y = "Total Obligations ($)", color = "") +
      scale_y_continuous(labels = scales::dollar) +
      scale_color_manual(values = c("National" = "gray60", "Selected State" = "#2C3E50")) +
      theme_minimal()
  })
  
  # --- ACS Income Map ---
  output$income_map <- renderLeaflet({
    df <- filtered_acs()
    
    pal <- colorQuantile("Greens", domain = df$median_household_income, n = 5)
    
    df_map <- county_data() %>%
      left_join(df, by = c("GEOID" = "geo_fips")) %>%
      st_as_sf()
    
    leaflet(df_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(median_household_income),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = ~paste0(NAME, "<br>Median Income: $", scales::comma(median_household_income))
      ) %>%
      addLegend(pal = pal, values = df$median_household_income, title = "Median Income", position = "bottomright")
  })
  
  # --- ACS Poverty Map ---
  output$poverty_map <- renderLeaflet({
    df <- filtered_acs()
    
    pal <- colorQuantile("Purples", domain = df$poverty_rate, n = 5)
    
    df_map <- county_data() %>%
      left_join(df, by = c("GEOID" = "geo_fips")) %>%
      st_as_sf()
    
    leaflet(df_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(poverty_rate),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = ~paste0(NAME, "<br>Poverty Rate: ", scales::percent(poverty_rate/100, accuracy = 0.1))
      ) %>%
      addLegend(pal = pal, values = df$poverty_rate, title = "Poverty Rate (%)", position = "bottomright")
  })
  
  # --- Small Business Contract Distribution Map ---
  output$smallbiz_contract_map <- renderLeaflet({
    df <- filtered_data() %>%
      filter(is_small_business == TRUE) %>%
      group_by(county_fips) %>%
      summarise(total_small_contracts = sum(total_obligation, na.rm = TRUE), .groups = "drop")
    
    pal <- colorQuantile("YlOrRd", domain = df$total_small_contracts, n = 5)
    
    df_map <- county_data() %>%
      left_join(df, by = c("GEOID" = "county_fips")) %>%
      st_as_sf()
    
    leaflet(df_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(total_small_contracts),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = ~paste0(NAME, "<br>Small Business Contracts: $", scales::comma(total_small_contracts))
      ) %>%
      addLegend(pal = pal, values = df$total_small_contracts, title = "Small Business Contracts", position = "bottomright")
  })
  
  # --- ACS Employment Rate Trend ---
  output$employment_trends <- renderPlot({
    df <- acs_data()
    
    if (input$acs_state != "All") {
      df <- df %>% filter(state == input$acs_state)
    }
    
    df %>%
      group_by(year) %>%
      summarise(avg_employment_rate = mean(employment_rate, na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = avg_employment_rate)) +
      geom_line(color = "#2E86AB", linewidth = 1.2) +
      geom_point(color = "#2E86AB", size = 2) +
      labs(title = "Average Employment Rate Over Time", x = "Year", y = "Employment Rate (%)") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme_minimal()
  })
  
  # --- Breakdown by Industry ---
  output$barPlot <- renderPlot({
    df <- filtered_data() %>%
      group_by(naics_group) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      slice_max(total, n = 10)
    
    ggplot(df, aes(x = reorder(naics_group, total), y = total)) +
      geom_col(fill = "#1F77B4") +
      coord_flip() +
      labs(title = "Top NAICS Groups", x = "NAICS Group", y = "Total Obligations ($)") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  # --- Top NAICS Groups Over Time ---
  output$naicsTrendPlot <- renderPlot({
    df <- fedcon_data()
    
    if (!is.null(input$agency)) df <- df %>% filter(parent_agency %in% input$agency)
    if (!is.null(input$naics_group)) df <- df %>% filter(naics_group %in% input$naics_group)
    if (input$minority != "All") df <- df %>% filter(is_minority_owned == (input$minority == "Yes"))
    if (input$woman != "All") df <- df %>% filter(is_woman_owned == (input$woman == "Yes"))
    if (input$veteran != "All") df <- df %>% filter(is_veteran_owned == (input$veteran == "Yes"))
    
    if (!is.null(clicked_state())) {
      df <- df %>% filter(state == (state_data() %>% filter(STATEFP == clicked_state()) %>% pull(STUSPS)))
    }
    
    df_plot <- df %>%
      group_by(naics_group, fiscal_year) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(naics_group))
    
    top_naics <- df_plot %>%
      group_by(naics_group) %>%
      summarise(total_all = sum(total)) %>%
      arrange(desc(total_all)) %>%
      slice_head(n = 5) %>%
      pull(naics_group)
    
    df_plot <- df_plot %>% filter(naics_group %in% top_naics)
    
    ggplot(df_plot, aes(x = fiscal_year, y = total, color = naics_group)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(title = "Top NAICS Groups Over Time", x = "Year", y = "Total Obligations ($)", color = "NAICS Group") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  # --- Top Agencies Over Time ---
  output$agencyTrendPlot <- renderPlot({
    df <- fedcon_data()
    
    if (!is.null(input$agency)) df <- df %>% filter(parent_agency %in% input$agency)
    if (!is.null(input$naics_group)) df <- df %>% filter(naics_group %in% input$naics_group)
    if (input$minority != "All") df <- df %>% filter(is_minority_owned == (input$minority == "Yes"))
    if (input$woman != "All") df <- df %>% filter(is_woman_owned == (input$woman == "Yes"))
    if (input$veteran != "All") df <- df %>% filter(is_veteran_owned == (input$veteran == "Yes"))
    
    if (!is.null(clicked_state())) {
      df <- df %>% filter(state == (state_data() %>% filter(STATEFP == clicked_state()) %>% pull(STUSPS)))
    }
    
    df_plot <- df %>%
      group_by(parent_agency, fiscal_year) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
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
      labs(title = "Top Agencies Over Time", x = "Year", y = "Total Obligations ($)", color = "Agency") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  # --- SBCS Revenue Changes ---
  output$revenuePlot <- renderPlot({
    df <- filtered_survey()
    
    df %>%
      count(revenue_change) %>%
      mutate(pct = n / sum(n)) %>%
      ggplot(aes(x = revenue_change, y = pct)) +
      geom_col(fill = "#1F77B4") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Small Business Revenue Changes", x = "Revenue Change", y = "% of Businesses") +
      theme_minimal()
  })
  
  # --- SBCS Employment Changes ---
  output$employmentPlot <- renderPlot({
    df <- filtered_survey()
    
    df %>%
      count(employment_change) %>%
      mutate(pct = n / sum(n)) %>%
      ggplot(aes(x = employment_change, y = pct)) +
      geom_col(fill = "#2CA02C") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Small Business Employment Changes", x = "Employment Change", y = "% of Businesses") +
      theme_minimal()
  })
  
  # --- SBCS Financing Access ---
  output$financingPlot <- renderPlot({
    df <- filtered_survey()
    
    df %>%
      count(financing_access) %>%
      mutate(pct = n / sum(n)) %>%
      ggplot(aes(x = financing_access, y = pct)) +
      geom_col(fill = "#FF7F0E") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Small Business Financing Access", x = "Financing Outcome", y = "% of Applicants") +
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

