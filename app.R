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
cbp <- readRDS("data/cbp_summary_clean.rds")
cbp_full <- readRDS("data/cbp_2017_2022.rds")
bizform <- readRDS("data/business_apps.rds")
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
# Summarize CBP data at county-year level

cbp_clean_with_sector_naics <- cbp_full %>%
  filter(str_detect(naics, "^[0-9]{2}[-]{4}")) %>%
  mutate(naics_2digit = str_sub(naics, 1, 2))

county_obligation_summary <- fedcon %>%
  group_by(county_fips, fiscal_year) %>%
  summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop")

# Merge CBP and obligations
county_merged <- county_obligation_summary %>%
  left_join(cbp, by = c("county_fips", "fiscal_year" = "year"))

## NAICS lookup table 
naics_lookup <- cbp_full %>%
  select(naics) %>%
  distinct() %>%
  filter(!is.na(naics)) %>%
  arrange(naics)

naics_group_lookup <- tribble(
  ~naics_2digit, ~naics_group,
  "11", "Agriculture & Mining",
  "21", "Agriculture & Mining",
  "22", "Utilities & Energy",
  "23", "Construction",
  "31", "Manufacturing",
  "32", "Manufacturing",
  "33", "Manufacturing",
  "42", "Wholesale & Retail Trade",
  "44", "Wholesale & Retail Trade",
  "45", "Wholesale & Retail Trade",
  "48", "Transportation & Warehousing",
  "49", "Transportation & Warehousing",
  "51", "Information & Technology",
  "52", "Information & Technology",
  "53", "Information & Technology",
  "54", "Professional, Scientific & Technical Services",
  "55", "Professional, Scientific & Technical Services",
  "56", "Professional, Scientific & Technical Services",
  "61", "Other",
  "62", "Healthcare & Social Assistance",
  "71", "Other",
  "72", "Other",
  "81", "Other",
  "92", "Public Admin & Support Services"
)

cbp_clean_with_sector_naics <- cbp_full %>%
  filter(str_detect(naics, "^[0-9]{2}[-]{4}")) %>%
  mutate(naics_2digit = str_sub(naics, 1, 2)) %>%
  left_join(naics_group_lookup, by = "naics_2digit")


## processing data for shayna's app

# 1. Create state abbreviation lookup table
state_abbr_lookup <- tibble::tibble(
  state_full = state.name,
  state = state.abb
) %>%
  bind_rows(tibble(state_full = "District of Columbia", state = "DC"),
            tibble(state_full = "Puerto Rico", state = "PR"))

# 2. Process and clean bizform (business applications)
apps_long <- bizform %>%
  rename(state = State) %>%
  pivot_longer(cols = `2017`:`2023`, names_to = "year", values_to = "applications") %>%
  mutate(year = as.integer(year))

# 3. Clean population data (convert full names to 2-letter abbrevs)
pop_long <- acs_summary %>%
  rename(state_full = state, population = total_population) %>%
  left_join(state_abbr_lookup, by = "state_full") %>%
  filter(!is.na(state)) %>%
  mutate(year = as.integer(year)) %>%
  group_by(state, year) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

# --- 3) UI ---
ui <- dashboardPage(
  dashboardHeader(title = "Federal Contracts Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "filters", icon = icon("sliders-h")),
      selectInput(
        "year", "Select Year", 
        choices = sort(unique(fedcon$fiscal_year)), 
        selected = 2023),
      pickerInput("agency", "Select Agencies:", 
                  choices = sort(unique(fedcon$parent_agency)), 
                  multiple = TRUE, 
                  options = list(`actions-box` = TRUE, title = "All"), 
                  selected = unique(fedcon$parent_agency)),
      pickerInput("naics_group", "Select NAICS Groups:", 
                  choices = sort(unique(fedcon$naics_group)), 
                  multiple = TRUE, 
                  options = list(`actions-box` = TRUE, title = "All"), 
                  selected = unique(fedcon$naics_group)),
      selectInput("minority", "Minority Owned", 
                  choices = c("All", "Yes", "No")),
      selectInput("woman", "Woman Owned", 
                  choices = c("All", "Yes", "No")),
      selectInput("veteran", "Veteran Owned", 
                  choices = c("All", "Yes", "No"))
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
      valueBoxOutput("totalOblig") %>% 
        withSpinner(color = "#007bff"),
      valueBoxOutput("gdp") %>% 
        withSpinner(color = "#007bff"),
      valueBoxOutput("pctGDP") %>% 
        withSpinner(color = "#007bff")
    ),
    
    fluidRow(
      box(title = "Federal Contract Obligations Map", 
          width = 12,
          leafletOutput("map") %>% 
            withSpinner(color = "#007bff"),
          actionButton("reset_view", "Reset to National View")
      )
    ),
    
    fluidRow(
      tabBox(
        id = "trends_tabs",
        title = "Data Explorer",
        width = 12,
        
        tabPanel("Economic Impact Comparison",
                 fluidRow(
                   valueBoxOutput("econ_valuebox") %>% 
                     withSpinner(color = "#007bff")
                 ),
                 fluidRow(
                   column(4,
                          selectInput("econ_agency", "Select Agency to Compare:",
                                      choices = sort(unique(fedcon$parent_agency)), 
                                      selected = "Department of Defense"),
                          selectInput("econ_outcome", "Select Outcome:",
                                      choices = c("Employment" = "total_emp",
                                                  "Establishments" = "total_est",
                                                  "Annual Payroll" = "total_ap"),
                                      selected = "total_emp"),
                          checkboxInput("econ_year_toggle", "Limit to selected year", value = FALSE),
                          checkboxInput("econ_reg_toggle", "Use adjusted regression estimate", value = FALSE),
                          checkboxInput("econ_use_median", "Use median instead of mean", value = TRUE)
                   ),
                   column(8,
                          plotlyOutput("econ_compare_plot") %>%
                            withSpinner(color = "#007bff"),
                          div(style = "font-size: 13px; color: #666; margin-top: 5px;",
                              textOutput("econ_plot_note")),
                          br(),
                          div(style = "background-color:#f9f9f9; border:1px solid #ddd; padding:15px; border-radius:8px; font-size:15px; line-height:1.5;",
                              textOutput("econ_t_test")
                          )
                   )
                 )
        ),
        
        tabPanel("Distribution Explorer",
                 fluidRow(
                   column(6,
                          selectInput("dist_type", "Explore By:", 
                                      choices = c("Agency", "NAICS Group")),
                          uiOutput("dist_choice"))
                 ),
                 fluidRow(
                   plotOutput("distPlot") %>% withSpinner(color = "#007bff")
                 )
        ),
        
        tabPanel("Obligations Over Time",
                 plotOutput("trendPlot") %>% 
                   withSpinner(color = "#007bff")
        ),
        
        tabPanel("Breakdown by Industry",
                 plotOutput("barPlot") %>%
                   withSpinner(color = "#007bff")
        ),
        
        tabPanel("Top NAICS Groups Over Time",
                 plotOutput("naicsTrendPlot") %>% 
                   withSpinner(color = "#007bff")
        ),
        
        tabPanel("Top Agencies Over Time",
                 plotOutput("agencyTrendPlot") %>% 
                   withSpinner(color = "#007bff")
        ),
        
        tabPanel("Small Business Credit Survey",
                 fluidRow(
                   column(4,
                          selectInput("survey_year", "Select Survey Year:", 
                                      choices = NULL),
                          selectInput("survey_state", "Select State (optional):", 
                                      choices = NULL)
                   )
                 ),
                 fluidRow(
                   box(title = "Revenue Changes", 
                       width = 6, 
                       plotOutput("revenuePlot") %>% 
                         withSpinner(color = "#007bff")),
                   box(title = "Employment Changes", 
                       width = 6, 
                       plotOutput("employmentPlot") %>% 
                         withSpinner(color = "#007bff"))
                 ),
                 fluidRow(
                   box(title = "Financing Access", 
                       width = 12, 
                       plotOutput("financingPlot") %>% 
                         withSpinner(color = "#007bff"))
                 )
        ),
        tabPanel("Business Apps & Population",
                 fluidRow(
                   column(3,
                          radioButtons("apps_view", "View Type:",
                                       choices = c("Bar Chart", "Line Chart"),
                                       inline = TRUE),
                          selectInput("apps_metric", "Select Metric:",
                                      choices = c("Applications", "Population", "Applications per Capita", "Growth Rate")),
                          conditionalPanel(
                            condition = "input.apps_view == 'Bar Chart'",
                            sliderInput("apps_year", "Select Year:",
                                        min = 2006, max = 2023, value = 2023, step = 1, sep = ""),
                            selectInput("apps_rank", "Show:", choices = c("All States", "Top 10", "Bottom 10"))
                          ),
                          conditionalPanel(
                            condition = "input.apps_view == 'Line Chart'",
                            selectInput("apps_state", "Select State:",
                                        choices = sort(unique(apps_long$state)))
                          )
                   ),
                   column(9,
                          conditionalPanel(
                            condition = "input.apps_view == 'Bar Chart'",
                            plotlyOutput("apps_bar_plot", height = "600px") %>%
                              withSpinner(color = "#007bff")
                          ),
                          conditionalPanel(
                            condition = "input.apps_view == 'Line Chart'",
                            plotlyOutput("apps_line_plot", height = "600px") %>%
                              withSpinner(color = "#007bff")
                          )
                   )
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
  
  econ_compare_data <- reactive({
    agency_df <- fedcon %>%
      filter(agency == input$econ_agency)
    
    if (input$econ_year_toggle) {
      agency_df <- agency_df %>% filter(fiscal_year == input$year)
    }
    
    if (!is.null(clicked_state())) {
      selected_state_abbr <- state_sf %>%
        filter(STATEFP == clicked_state()) %>%
        pull(STUSPS)
      agency_df <- agency_df %>% filter(state == selected_state_abbr)
    }
    
    agency_df <- agency_df %>%
      distinct(county_fips, fiscal_year) %>%
      mutate(received_contract = TRUE)
    
    cbp_filtered <- cbp
    if (input$econ_year_toggle) {
      cbp_filtered <- cbp_filtered %>% filter(year == input$year)
    }
    
    if (!is.null(clicked_state())) {
      county_fips_list <- fedcon %>%
        filter(state == selected_state_abbr) %>%
        distinct(county_fips) %>%
        pull(county_fips)
      cbp_filtered <- cbp_filtered %>% filter(county_fips %in% county_fips_list)
    }
    
    df <- cbp_filtered %>%
      left_join(agency_df, by = c("county_fips", "year" = "fiscal_year")) %>%
      mutate(received_contract = ifelse(is.na(received_contract), FALSE, received_contract))
    
    df <- df %>%
      left_join(acs_summary %>% 
                  select(geo_fips, year, population = total_population),
                by = c("county_fips" = "geo_fips", "year"))
    
    df <- df %>%
      mutate(state_fips = substr(county_fips, 1, 2)) %>%
      left_join(state_sf %>% 
                  select(STATEFP, STUSPS), 
                by = c("state_fips" = "STATEFP")) %>%
      rename(state_abbr = STUSPS)
    
    return(df)
  })
  
  legend_title <- reactive({
    if (input$trends_tabs == "Distribution Explorer") {
      "% GDP Obligations"
    } else if (!is.null(input$econ_variable)) {
      if (input$econ_variable == "Median Income") {
        "Median Household Income ($)"
      } else if (input$econ_variable == "Poverty Rate") {
        "Poverty Rate (%)"
      } else if (input$econ_variable == "Small Biz Contracts") {
        "Small Business Contracts ($)"
      } else {
        ""
      }
    } else {
      NULL
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
  
  get_selected_state <- function() {
    if (is.null(clicked_state())) return(NULL)
    state_sf %>% filter(STATEFP == clicked_state()) %>% pull(STUSPS)
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
        filter(state == (state_sf %>% filter(STATEFP == clicked_state()) %>% 
                           pull(STUSPS)))
    }
    
    df
  })
  
  filtered_survey <- reactive({
    req(input$survey_year)
    df <- sbcs %>% 
      filter(year == input$survey_year)
    if (input$survey_state != "All") df <- df %>% 
      filter(state == input$survey_state)
    df
  })
  
  state_summary <- reactive({
    filtered <- filtered_data()  # includes all your filters
    
    df <- filtered %>%
      group_by(state) %>%
      summarise(total_obligation = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      left_join(state_gdp %>% filter(year == input$year), by = c("state" = "state_abbr.x")) %>%
      mutate(
        gdp_dollars = value * 1e6,
        pct_gdp = total_obligation / gdp_dollars
      )
    
    state_sf %>%
      left_join(df, by = c("STUSPS" = "state"))
  })
  
  county_summary_data <- reactive({
    req(clicked_state(), input$year)
    state_abbr <- state_sf %>% filter(STATEFP == clicked_state()) %>% pull(STUSPS)
    
    filtered <- filtered_data() %>% filter(state == state_abbr)
    
    df <- filtered %>%
      group_by(county_fips) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      left_join(county_gdp %>% filter(year == input$year), by = c("county_fips" = "geo_fips")) %>%
      mutate(
        gdp_dollars = gdp_millions * 1e6,
        pct_gdp = total / gdp_dollars
      )
    
    county_sf %>%
      filter(STATEFP == clicked_state()) %>%
      left_join(df, by = c("GEOID" = "county_fips"))
  })
  
  ## reactive for economic model tab
  econ_model <- reactive({
    req(input$econ_outcome)
    df <- econ_compare_data() %>%
      filter(!is.na(received_contract), 
             !is.na(total_est), 
             !is.na(total_ap),
             !is.na(population), 
             !is.na(state_abbr), 
             !is.na(year))
    
    model_formula <- if (is.null(clicked_state())) {
      as.formula(
        paste0(input$econ_outcome,
               " ~ received_contract + log(population + 1) + total_est + total_ap + factor(state_abbr) + factor(year)")
      )
    } else {
      as.formula(
        paste0(input$econ_outcome,
               " ~ received_contract + log(population + 1) + total_est + total_ap + factor(year)")
      )
    }
    
    lm(model_formula, data = df)
  })
  
  # --- Outputs ---
  
  # --- 4a) Map ---
  output$map <- renderLeaflet({ leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(lng = -98.5, lat = 39.8, zoom = 4) })
  
  observe({
    if (drilldown_mode() == "states") {
      df <- state_summary()
      pal <- colorQuantile("plasma", 
                           domain = df$pct_gdp, 
                           n = 5, na.color = "#cccccc")
      
      leafletProxy("map", session) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data = df,
          fillColor = ~pal(pct_gdp),
          color = "white",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.7,
          label = ~paste0(NAME, 
                          "<br>Total Obligations: ", 
                          scales::dollar(total_obligation),
                          "<br>GDP: ", 
                          scales::dollar(gdp_dollars),
                          "<br>Obligations as % of GDP: ", 
                          scales::percent(pct_gdp, accuracy = 0.1)),
          layerId = ~STATEFP
        ) %>%
        addLegend(
          pal = pal,
          values = df$pct_gdp,
          title = legend_title(),    
          position = "bottomright",
          labFormat = labelFormat(suffix = "%", transform = function(x) x * 100)
        )
      
    } else if (drilldown_mode() == "counties") {
      df <- county_summary_data()
      pal <- colorQuantile("plasma", 
                           domain = df$pct_gdp, 
                           n = 5, 
                           na.color = "#cccccc")
      
      leafletProxy("map", session) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data = df,
          fillColor = ~pal(pct_gdp),
          color = "white",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.7,
          label = ~paste0(NAME, 
                          "<br>Total: ", 
                          scales::dollar(total),
                          "<br>GDP: ", 
                          scales::dollar(gdp_dollars),
                          "<br>% GDP: ", 
                          scales::percent(pct_gdp, accuracy = 0.1)),
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
    data <- filtered_data()
    
    if (drilldown_mode() == "counties" && !is.null(clicked_state())) {
      selected_state <- state_sf %>% filter(STATEFP == clicked_state()) %>% pull(STUSPS)
      data <- data %>% filter(state == selected_state)
    }
    
    total <- sum(data$total_obligation, na.rm = TRUE)
    
    valueBox(
      value = scales::dollar(total),
      subtitle = if (drilldown_mode() == "counties") "State Obligations" else "Total Obligations",
      icon = icon("file-invoice-dollar"),
      color = "blue"
    )
  })
  
  output$gdp <- renderValueBox({
    data <- filtered_data()
    subtitle <- "Total GDP"
    gdp_value <- NA_real_
    
    if (drilldown_mode() == "counties" && !is.null(clicked_state())) {
      selected_state <- state_sf %>%
        filter(STATEFP == clicked_state()) %>%
        pull(STUSPS)
      
      if (length(selected_state) == 1 && any(data$state == selected_state)) {
        gdp_raw <- state_gdp_clean %>%
          filter(year == input$year, state_abbr == selected_state) %>%
          summarise(gdp = sum(gdp, na.rm = TRUE)) %>%
          pull(gdp)
        if (length(gdp_raw) == 1 && !is.na(gdp_raw)) {
          gdp_value <- gdp_raw * 1e6
          subtitle <- paste(selected_state, "GDP")
        }
      }
    } else {
      selected_states <- unique(data$state)
      
      gdp_raw <- state_gdp_clean %>%
        filter(year == input$year, state_abbr %in% selected_states) %>%
        summarise(gdp = sum(gdp, na.rm = TRUE)) %>%
        pull(gdp)
      if (length(gdp_raw) == 1 && !is.na(gdp_raw)) {
        gdp_value <- gdp_raw * 1e6
      }
    }
    
    valueBox(
      value = if (length(gdp_value) == 1 && !is.na(gdp_value) && gdp_value > 0) {
        scales::dollar(gdp_value)
      } else {
        "No Data"
      },
      subtitle = subtitle,
      icon = icon("landmark"),
      color = "green"
    )
  })
  
  output$pctGDP <- renderValueBox({
    data <- filtered_data()
    total_oblig <- NA_real_
    gdp_value <- NA_real_
    
    if (drilldown_mode() == "counties" && !is.null(clicked_state())) {
      selected_state <- state_sf %>%
        filter(STATEFP == clicked_state()) %>%
        pull(STUSPS)
      
      if (length(selected_state) == 1 && any(data$state == selected_state)) {
        data <- data %>% filter(state == selected_state)
        total_oblig <- sum(data$total_obligation, na.rm = TRUE)
        
        gdp_raw <- state_gdp_clean %>%
          filter(year == input$year, state_abbr == selected_state) %>%
          pull(gdp)
        if (length(gdp_raw) == 1 && !is.na(gdp_raw)) {
          gdp_value <- gdp_raw * 1e6
        }
      }
    } else {
      total_oblig <- sum(data$total_obligation, na.rm = TRUE)
      selected_states <- unique(data$state)
      
      gdp_raw <- state_gdp_clean %>%
        filter(year == input$year, state_abbr %in% selected_states) %>%
        summarise(gdp = sum(gdp, na.rm = TRUE)) %>%
        pull(gdp)
      if (length(gdp_raw) == 1 && !is.na(gdp_raw)) {
        gdp_value <- gdp_raw * 1e6
      }
    }
    
    pct <- if (!is.na(gdp_value) && gdp_value > 0) total_oblig / gdp_value else NA_real_
    
    valueBox(
      value = if (!is.na(pct)) {
        if (pct < 0.0001) "< 0.01%" else scales::percent(pct, accuracy = 0.01)
      } else {
        "No Data"
      },
      subtitle = "Obligations as % GDP",
      icon = icon("percentage"),
      color = "orange"
    )
  })
  
  # --- economic impact comparison --- 
  # --- 4b) Economic Impact Value Box ---
  output$econ_valuebox <- renderValueBox({
    req(input$econ_outcome, input$econ_agency)
    df <- econ_compare_data()
    
    label_map <- c(
      total_emp = "Jobs",
      total_est = "Businesses",
      total_ap  = "in Payroll ($)"
    )
    label <- label_map[[input$econ_outcome]] %||% input$econ_outcome
    
    if (input$econ_reg_toggle) {
      model <- econ_model()
      estimate <- coef(model)["received_contractTRUE"]
      diff <- estimate
      r_squared <- summary(model)$r.squared
    } else {
      if (input$econ_use_median) {
        stats <- df %>%
          group_by(received_contract) %>%
          summarise(val = median(.data[[input$econ_outcome]], na.rm = TRUE), .groups = "drop")
      } else {
        stats <- df %>%
          group_by(received_contract) %>%
          summarise(val = mean(.data[[input$econ_outcome]], na.rm = TRUE), .groups = "drop")
      }
      treated <- stats %>% filter(received_contract == TRUE) %>% pull(val)
      untreated <- stats %>% filter(received_contract == FALSE) %>% pull(val)
      diff <- treated - untreated
      r_squared <- NA
    }
    
    location_suffix <- if (!is.null(clicked_state())) {
      state_name <- state_sf %>% filter(STATEFP == clicked_state()) %>% pull(NAME)
      paste0(" in ", state_name)
    } else {
      " (National)"
    }
    
    subtitle_text <- if (input$econ_reg_toggle) {
      if (is.null(clicked_state())) {
        paste0(
          "Adjusted effect on ", label, " from ", input$econ_agency, " Contracts (National)",
          "<br><small>R² = ", round(r_squared, 3),
          " <i class='fas fa-info-circle' title='Linear model controlling for population, establishments, payroll, state, and year.'></i></small>"
        )
      } else {
        paste0(
          "Adjusted effect on ", label, " from ", input$econ_agency, " Contracts in ", state_name,
          "<br><small>R² = ", round(r_squared, 3),
          " <i class='fas fa-info-circle' title='Linear model controlling for population, establishments, payroll, and year (state fixed effects omitted due to within-state focus).'></i></small>"
        )
      }
    } else {
      method <- if (input$econ_use_median) "Median" else "Mean"
      paste0(method, " difference in ", label, " from ", input$econ_agency, " Contracts", location_suffix)
    }
    
    valueBox(
      value = paste0(ifelse(diff >= 0, "+", ""), scales::comma(round(diff))),
      subtitle = HTML(subtitle_text),
      icon = icon("chart-line"),
      color = if (diff > 0) "green" else "red"
    )
  })
  
  # text description output
  output$econ_plot_note <- renderText({
    method <- if (input$econ_reg_toggle) {
      "Adjusted linear regression controlling for population, payroll, establishments, region, and year."
    } else {
      "Descriptive comparison using group medians."
    }
    
    year_text <- if (input$econ_year_toggle) {
      paste("Showing data for year", input$year)
    } else {
      "Showing data across all available years"
    }
    
    paste(method, year_text)
  })
  
  # --- 4c) Economic Impact Plot ---
  output$econ_compare_plot <- renderPlotly({
    req(input$econ_outcome, input$econ_agency)
    df <- econ_compare_data()
    
    # Label lookup
    label_map <- c(
      total_emp = "Employment",
      total_est = "Establishments",
      total_ap  = "Annual Payroll"
    )
    label <- label_map[[input$econ_outcome]] %||% input$econ_outcome
    
    df <- df %>%
      mutate(contract_group = ifelse(received_contract, "Received Contract", "No Contract"))
    
    # Determine summary stat
    summary_stat <- if (input$econ_use_median) median else mean
    stat_name <- if (input$econ_use_median) "Median" else "Mean"
    
    # Prepare summary point data
    stat_df <- df %>%
      group_by(contract_group) %>%
      summarise(stat_val = summary_stat(.data[[input$econ_outcome]], na.rm = TRUE), .groups = "drop")
    
    # Build violin plot
    p <- ggplot(df, aes(x = contract_group, y = .data[[input$econ_outcome]])) +
      geom_violin(fill = "#2C3E50", alpha = 0.8, scale = "width", trim = TRUE) +
      geom_point(data = stat_df, aes(x = contract_group, y = stat_val),
                 color = "red", size = 3, shape = 18) +
      labs(
        title = paste0(stat_name, " ", label, " by Contract Group"),
        subtitle = paste("for", input$econ_agency,
                         if (!is.null(clicked_state())) {
                           state_name <- state_sf %>%
                             filter(STATEFP == clicked_state()) %>%
                             pull(NAME)
                           paste("in", state_name)
                         } else {
                           "(National)"
                         }),
        x = NULL,
        y = label
      ) +
      theme_minimal(base_size = 14) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(p)
  })
  
  # plot note for year toggle
  output$econ_plot_note <- renderText({
    if (input$econ_year_toggle) {
      paste("Showing data for year", input$year)
    } else {
      "Showing data across all available years"
    }
  })
  
  # --- 4c) Economic Impact Summary Text ---
  output$econ_t_test <- renderText({
    df <- econ_compare_data()
    label_map <- c(
      total_emp = "jobs",
      total_est = "businesses",
      total_ap  = "payroll dollars"
    )
    label <- label_map[[input$econ_outcome]] %||% input$econ_outcome
    
    if (input$econ_reg_toggle) {
      model <- econ_model()
      summary_model <- summary(model)
      coef_data <- summary_model$coefficients["received_contractTRUE", ]
      estimate <- coef_data["Estimate"]
      std_error <- coef_data["Std. Error"]
      p_val <- coef_data["Pr(>|t|)"]
      
      ci_lower <- estimate - 1.96 * std_error
      ci_upper <- estimate + 1.96 * std_error
      
      location <- if (!is.null(clicked_state())) {
        state_name <- state_sf %>% filter(STATEFP == clicked_state()) %>% pull(NAME)
        paste("in", state_name)
      } else {
        "nationally"
      }
      
      effect_note <- if (!is.null(clicked_state())) {
        "This model controls for population, establishments, payroll, and year (state fixed effects omitted due to single-state scope)."
      } else {
        "This model controls for population, establishments, payroll, state, and year."
      }
      
      paste0(
        "After adjusting for business counts and payroll, receiving a contract from ", input$econ_agency,
        " is associated with an estimated change of ", scales::comma(round(estimate)), " ", label, " ", location, ". ",
        "The 95% confidence interval ranges from ", scales::comma(round(ci_lower)), " to ", scales::comma(round(ci_upper)), ". ",
        "This estimate is statistically significant (p < ", formatC(p_val, format = "e", digits = 2), "). ", effect_note
      )
    } else {
      t <- t.test(df[[input$econ_outcome]] ~ df$received_contract)
      treated_mean <- round(t$estimate[[2]])
      untreated_mean <- round(t$estimate[[1]])
      diff <- treated_mean - untreated_mean
      
      intro <- if (!is.null(clicked_state())) {
        state_name <- state_sf %>% filter(STATEFP == clicked_state()) %>% pull(NAME)
        paste0("In ", state_name, ", ")
      } else {
        "Nationally, "
      }
      
      paste0(
        intro,
        "counties that received contracts from ", input$econ_agency,
        " had an average of ", scales::comma(treated_mean), " ", label,
        ", compared to ", scales::comma(untreated_mean), " in counties that did not. ",
        "This difference of ", scales::comma(diff), " is statistically significant (p < ",
        formatC(t$p.value, format = "e", digits = 2), ")."
      )
    }
  })
  
  
  # --- 4c) Distribution Explorer ---
  output$dist_choice <- renderUI({
    if (input$dist_type == "Agency") {
      pickerInput("dist_choice_value", "Select Agency:", 
                  choices = sort(unique(fedcon$parent_agency)), 
                  multiple = FALSE, 
                  options = list(`live-search` = TRUE))
    } else {
      pickerInput("dist_choice_value", "Select NAICS Group:", 
                  choices = sort(unique(fedcon$naics_group)), 
                  multiple = FALSE, 
                  options = list(`live-search` = TRUE))
    }
  })
  
  output$distPlot <- renderPlot({
    req(input$dist_choice_value)
    
    df <- fedcon %>%
      filter(fiscal_year == input$year)
    
    # Apply filter for Agency or NAICS
    if (input$dist_type == "Agency") {
      df <- df %>% filter(parent_agency == input$dist_choice_value)
    } else {
      df <- df %>% filter(naics_group == input$dist_choice_value)
    }
    
    # If a state is clicked, show counties
    if (!is.null(clicked_state())) {
      selected_state <- state_sf %>% 
        filter(STATEFP == clicked_state()) %>% 
        pull(STUSPS)
      
      df <- df %>% filter(state == selected_state)
      
      # Summarize and join with county names
      df_county <- df %>%
        group_by(county_fips) %>%
        summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
        left_join(county_sf %>% st_drop_geometry() %>%
                    mutate(county_name = paste0(NAME, ", ", STATEFP)),
                  by = c("county_fips" = "GEOID"))
      
      df_county %>%
        slice_max(total, n = 10) %>%
        ggplot(aes(x = reorder(county_name, total), y = total)) +
        geom_col(fill = "#1F77B4") +
        coord_flip() +
        scale_y_continuous(labels = scales::dollar) +
        labs(title = paste("Top Counties in", selected_state),
             x = NULL, y = "Total Obligations ($)") +
        theme_minimal()
      
    } else {
      # Default view: show top states
      df %>%
        group_by(state) %>%
        summarise(total = sum(total_obligation, na.rm = TRUE)) %>%
        slice_max(total, n = 10) %>%
        ggplot(aes(x = reorder(state, total), y = total)) +
        geom_col(fill = "#1F77B4") +
        coord_flip() +
        scale_y_continuous(labels = scales::dollar) +
        labs(title = "Top States by Obligations",
             x = NULL, y = "Total Obligations ($)") +
        theme_minimal()
    }
  })
  
  # --- Obligations Over Time ---
  output$trendPlot <- renderPlot({
    df <- fedcon
    df <- apply_all_filters(df, input)
    
    selected_state <- get_selected_state()
    
    if (!is.null(selected_state)) {
      # State-specific trend
      df_plot <- df %>%
        filter(state == selected_state) %>%
        group_by(fiscal_year) %>%
        summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop")
      
      title_text <- paste("Federal Obligations Over Time in", selected_state)
    } else {
      # National trend
      df_plot <- df %>%
        group_by(fiscal_year) %>%
        summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop")
      
      title_text <- "Federal Obligations Over Time (National)"
    }
    
    ggplot(df_plot, aes(x = fiscal_year, y = total)) +
      geom_line(linewidth = 1.2, color = "#2C3E50") +
      geom_point(size = 2, color = "#2C3E50") +
      labs(
        title = title_text,
        x = "Fiscal Year",
        y = "Total Obligations ($)"
      ) +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal()
  })
  
  # --- Breakdown by Industry ---
  output$barPlot <- renderPlot({
    df <- filtered_data()
    selected_state <- get_selected_state()
    if (!is.null(selected_state)) {
      df <- df %>% filter(state == selected_state)
    }
    
    df %>%
      group_by(naics_group) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      slice_max(total, n = 10) %>%
      ggplot(aes(x = reorder(naics_group, total), y = total)) +
      geom_col(fill = "#1F77B4") +
      coord_flip() +
      labs(title = "Top NAICS Groups",
           subtitle = if (!is.null(selected_state)) paste("in", selected_state) else "Nationally",
           x = "NAICS Group", y = "Total Obligations ($)") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  # --- Top NAICS Groups Over Time ---
  output$naicsTrendPlot <- renderPlot({
    df <- fedcon
    selected_state <- get_selected_state()
    if (!is.null(selected_state)) {
      df <- df %>% filter(state == selected_state)
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
    
    df_plot %>%
      filter(naics_group %in% top_naics) %>%
      ggplot(aes(x = fiscal_year, y = total, color = naics_group)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Top NAICS Groups Over Time",
        subtitle = if (!is.null(selected_state)) paste("in", selected_state) else "Nationally",
        x = "Year", y = "Total Obligations ($)", color = "NAICS Group"
      ) +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  # --- Top Agencies Over Time ---
  output$agencyTrendPlot <- renderPlot({
    df <- fedcon
    selected_state <- get_selected_state()
    if (!is.null(selected_state)) {
      df <- df %>% filter(state == selected_state)
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
    
    df_plot %>%
      filter(parent_agency %in% top_agencies) %>%
      ggplot(aes(x = fiscal_year, y = total, color = parent_agency)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Top Agencies Over Time",
        subtitle = if (!is.null(selected_state)) paste("in", selected_state) else "Nationally",
        x = "Year", y = "Total Obligations ($)", color = "Agency"
      ) +
      scale_y_continuous(labels = scales::dollar) +
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
  
  
  ## filter warning
  output$filter_warning <- renderText({
    if (nrow(filtered_data()) == 0) {
      "⚠️ No results match the selected filters."
    } else {
      ""
    }
  })
  
  ## Business apps bar plot
  output$apps_bar_plot <- renderPlotly({
    req(input$apps_metric, input$apps_year)
    
    year <- input$apps_year
    metric <- input$apps_metric
    
    df_apps <- apps_long %>% filter(year == year)
    df_pop <- pop_long %>% filter(year == year)
    
    df <- df_apps %>%
      group_by(state, year) %>%
      summarise(applications = sum(applications, na.rm = TRUE), .groups = "drop") %>%
      left_join(
        df_pop %>%
          group_by(state, year) %>%
          summarise(population = sum(population, na.rm = TRUE), .groups = "drop"),
        by = c("state", "year")
      )
    
    df <- df %>% mutate(
      value = case_when(
        metric == "Applications" ~ applications,
        metric == "Population" ~ population,
        metric == "Applications per Capita" ~ applications / population
      )
    )
    
    if (metric == "Growth Rate") {
      # Need 2 years of apps data only
      df_growth <- apps_long %>%
        filter(year %in% c(year, year - 1)) %>%
        group_by(state, year) %>%
        summarise(applications = sum(applications, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = year, values_from = applications)
      
      df <- df_growth %>%
        mutate(value = (`{year}` - `{year - 1}`) / `{year - 1}`) %>%
        select(state, value)
    }
    
    # Apply rank filter
    if (input$apps_rank == "Top 10") {
      df <- df %>% slice_max(value, n = 10)
    } else if (input$apps_rank == "Bottom 10") {
      df <- df %>% slice_min(value, n = 10)
    }
    
    plot_ly(
      data = df,
      x = ~reorder(state, value),
      y = ~value,
      type = "bar",
      marker = list(color = switch(
        metric,
        "Applications" = "royalblue",
        "Population" = "seagreen",
        "Applications per Capita" = "purple",
        "Growth Rate" = "darkorange"
      ))
    ) %>%
      layout(
        title = paste(metric, "by State in", year),
        xaxis = list(title = "State", tickangle = -45),
        yaxis = list(
          title = metric,
          tickformat = ifelse(metric %in% c("Applications per Capita", "Growth Rate"), ".0%", "")
        )
      )
  })
  
  ## business apps line plot
  output$apps_line_plot <- renderPlotly({
    req(input$apps_state, input$apps_metric)
    
    metric <- input$apps_metric
    state_abbr <- input$apps_state
    
    df_apps <- apps_long %>% filter(!is.na(applications))
    df_pop <- pop_long %>% filter(!is.na(population))
    
    df_combined <- df_apps %>%
      group_by(state, year) %>%
      summarise(applications = sum(applications, na.rm = TRUE), .groups = "drop") %>%
      left_join(
        df_pop %>%
          group_by(state, year) %>%
          summarise(population = sum(population, na.rm = TRUE), .groups = "drop"),
        by = c("state", "year")
      )
    
    state_line <- df_combined %>%
      filter(state == state_abbr) %>%
      arrange(year) %>%
      mutate(
        value = case_when(
          metric == "Applications" ~ applications,
          metric == "Population" ~ population,
          metric == "Applications per Capita" ~ applications / population,
          metric == "Growth Rate" ~ (applications - lag(applications)) / lag(applications)
        ),
        scope = state_abbr
      )
    
    national_line <- df_combined %>%
      group_by(year) %>%
      summarise(
        applications = sum(applications, na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(year) %>%
      mutate(
        value = case_when(
          metric == "Applications" ~ applications,
          metric == "Population" ~ population,
          metric == "Applications per Capita" ~ applications / population,
          metric == "Growth Rate" ~ (applications - lag(applications)) / lag(applications)
        ),
        scope = "U.S. Average"
      )
    
    df_plot <- bind_rows(state_line, national_line) %>%
      filter(!is.na(value))
    
    plot_ly(df_plot, x = ~year, y = ~value, color = ~scope, type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = paste(metric, "in", state_abbr, "vs U.S. Average"),
        yaxis = list(
          title = metric,
          tickformat = ifelse(metric %in% c("Applications per Capita", "Growth Rate"), ".0%", "")
        ),
        xaxis = list(title = "Year"),
        legend = list(title = list(text = ""), orientation = "h", x = 0.3, y = -0.2)
      )
  })
  
  
  ## Close Server
}

## Launch App
shinyApp(ui = ui, server = server)
