




library(tidycensus)
library(tidyverse)
library(sf)

# Set your API key (if not already saved in .Renviron)
census_api_key("f5c61af8842e32202adea0a0756bd9d10349bb5a", install = FALSE)

# Load list of states (excluding PR and territories)
states <- unique(fips_codes$state)[1:51]  # AK to WY

# Pull 2023 ACS population + geometry for all tracts
tract_sf <- map_dfr(states, ~{
  message("Downloading tracts for: ", .x)
  get_acs(
    geography = "tract",
    variables = "B01003_001",  # total population (dummy)
    state = .x,
    year = 2023,
    geometry = TRUE,
    cache_table = TRUE
  ) %>%
    select(GEOID, NAME, estimate, geometry)
})

# Optional: simplify for faster leaflet rendering
tract_sf_simplified <- st_simplify(tract_sf, dTolerance = 50)

# Save for use in your Shiny app
saveRDS(tract_sf_simplified, "data/tract_sf.rds")






# Get a lookup table for states → Census Division
library(tidyverse)
library(tigris)

data("fips_codes")

division_lookup <- fips_codes %>%
  select(state_name, state_code, state) %>%
  distinct() %>%
  mutate(
    census_division = case_when(
      state %in% c("CT", "ME", "MA", "NH", "RI", "VT") ~ "New England",
      state %in% c("NY", "NJ", "PA") ~ "Middle Atlantic",
      state %in% c("IL", "IN", "MI", "OH", "WI") ~ "East North Central",
      state %in% c("IA", "KS", "MN", "MO", "NE", "ND", "SD") ~ "West North Central",
      state %in% c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV") ~ "South Atlantic",
      state %in% c("AL", "KY", "MS", "TN") ~ "East South Central",
      state %in% c("AR", "LA", "OK", "TX") ~ "West South Central",
      state %in% c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY") ~ "Mountain",
      state %in% c("AK", "CA", "HI", "OR", "WA") ~ "Pacific"
    )
  )


county_sf <- county_sf %>%
  left_join(division_lookup, by = c("STATEFP" = "state_code"))


# Clean up geometries before simplifying
division_sf <- division_sf %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON")  # Ensures consistent geometry type

# Then simplify
division_sf <- st_simplify(division_sf, dTolerance = 500)

# Save to use in your app
saveRDS(division_sf, "data/division_sf.rds")


leafletProxy("map") %>%
  addPolygons(
    data = division_sf,
    layerId = ~census_division,
    color = "darkred",
    fillColor = "transparent",
    weight = 2,
    label = ~census_division
  )








# Ensure county_sf includes census_division info
data("fips_codes")

division_lookup <- fips_codes %>%
  select(state_name, state_code, state) %>%
  distinct() %>%
  mutate(
    census_division = case_when(
      state %in% c("CT", "ME", "MA", "NH", "RI", "VT") ~ "New England",
      state %in% c("NY", "NJ", "PA") ~ "Middle Atlantic",
      state %in% c("IL", "IN", "MI", "OH", "WI") ~ "East North Central",
      state %in% c("IA", "KS", "MN", "MO", "NE", "ND", "SD") ~ "West North Central",
      state %in% c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV") ~ "South Atlantic",
      state %in% c("AL", "KY", "MS", "TN") ~ "East South Central",
      state %in% c("AR", "LA", "OK", "TX") ~ "West South Central",
      state %in% c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY") ~ "Mountain",
      state %in% c("AK", "CA", "HI", "OR", "WA") ~ "Pacific"
    )
  )

# Ensure county_sf has STATEFP as character for joining
county_sf <- readRDS("data/county_sf.rds") %>%
  select(GEOID, NAME, STATEFP, geometry) %>%
  mutate(STATEFP = as.character(STATEFP)) %>%
  left_join(division_lookup, by = c("STATEFP" = "state_code")) %>%
  st_simplify(dTolerance = 100)

saveRDS(county_sf, "data/county_sf.rds")










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

# Null coalescing operator (if not already loaded)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# --- 1) Load Data ---
load_data <- function() {
  list(
    fedcon = readRDS("data/smallcon.rds"),
    state_gdp = readRDS("data/state_gdp.rds"),
    county_gdp = readRDS("data/county_gdp.rds"),
    sbcs = readRDS("data/sbcs.rds"),
    cbp = readRDS("data/cbp_summary_clean.rds"),
    cbp_full = readRDS("data/cbp_2017_2022.rds"),
    bizform = readRDS("data/business_apps.rds"),
    acs_summary = readRDS("data/acs_summary.rds"),
    state_sf = readRDS("data/state_sf.rds"),
    county_sf = readRDS("data/county_sf.rds")
  )
}

data <- load_data()

## Cleanly define
fedcon <- data$fedcon
acs_summary <- data$acs_summary
sbcs <- data$sbcs
cbp <- data$cbp
county_gdp <- data$county_gdp


# --- 2) Simplify and preprocess geometry (better performance)
county_sf <- data$county_sf %>%
  select(GEOID, NAME, STATEFP, geometry) %>%
  st_simplify(dTolerance = 100)

state_sf <- data$state_sf %>%
  select(STATEFP, STUSPS, geometry)

# --- 3) Clean GDP ---
state_gdp_clean <- data$state_gdp %>%
  select(year, state_abbr = state_abbr.x, gdp = value)

county_gdp_clean <- data$county_gdp %>%
  select(year, GEOID = geo_fips, gdp_millions)

# --- 4) Summarize obligations for merging ---
county_obligation_summary <- data$fedcon %>%
  group_by(county_fips, fiscal_year) %>%
  summarise(total_obligation = sum(total_obligation, na.rm = TRUE), .groups = "drop")

# --- 5) NAICS mapping (only once)
naics_lookup <- data$cbp_full %>%
  filter(str_detect(naics, "^[0-9]{2}[-]{4}")) %>%
  select(naics) %>%
  distinct() %>%
  drop_na()

naics_group_lookup <- tribble(
  ~naics_2digit, ~naics_group,
  "11", "Agriculture & Mining", "21", "Agriculture & Mining",
  "22", "Utilities & Energy", "23", "Construction",
  "31", "Manufacturing", "32", "Manufacturing", "33", "Manufacturing",
  "42", "Wholesale & Retail Trade", "44", "Wholesale & Retail Trade", "45", "Wholesale & Retail Trade",
  "48", "Transportation & Warehousing", "49", "Transportation & Warehousing",
  "51", "Information & Technology", "52", "Information & Technology", "53", "Information & Technology",
  "54", "Professional Services", "55", "Professional Services", "56", "Professional Services",
  "61", "Other", "62", "Healthcare & Social Assistance", "71", "Other", "72", "Other",
  "81", "Other", "92", "Public Admin & Support"
)

# --- 6) Clean CBP data ---
cbp_clean_with_sector <- data$cbp_full %>%
  filter(str_detect(naics, "^[0-9]{2}[-]{4}")) %>%
  mutate(naics_2digit = str_sub(naics, 1, 2)) %>%
  left_join(naics_group_lookup, by = "naics_2digit")




# Loaded Eric's UI code. Now ready for grouped refactor.

ui <- dashboardPage(
  dashboardHeader(title = "Federal Contracts Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "filters", icon = icon("sliders-h")),
      div(id = "yearFilters"),
      div(id = "agencyFilters"),
      div(id = "groupFilters"),
      div(id = "ownershipFilters")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(".main-sidebar .dropdown-menu { z-index: 9999 !important; }"))
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
        
        tabPanel("Economic Impact Comparison",
                 fluidRow(valueBoxOutput("econ_valuebox") %>% withSpinner(color = "#007bff")),
                 fluidRow(
                   column(4,
                          radioButtons("map_metric", "Map Metric:",
                                       choices = c("Per Capita Obligations" = "obligation_per_capita",
                                                   "% of County GDP" = "obligation_pct_gdp"),
                                       selected = "obligation_per_capita"),
                          selectInput("econ_agency", "Select Agency to Compare:", choices = NULL),
                          selectInput("econ_outcome", "Select Outcome:",
                                      choices = c("Employment" = "total_emp",
                                                  "Establishments" = "total_est",
                                                  "Annual Payroll" = "total_ap"))
                   ),
                   column(8,
                          plotlyOutput("econ_compare_plot") %>% withSpinner(color = "#007bff"),
                          br(),
                          div(style = "background-color:#f9f9f9; border:1px solid #ddd; padding:15px; border-radius:8px; font-size:15px; line-height:1.5;",
                              textOutput("econ_t_test"))
                   )
                 )
        ),
        
        tabPanel("Distribution Explorer",
                 fluidRow(
                   column(6,
                          selectInput("dist_type", "Explore By:", choices = c("Agency", "NAICS Group")),
                          uiOutput("dist_choice"))
                 ),
                 fluidRow(plotOutput("distPlot") %>% withSpinner(color = "#007bff"))
        ),
        
        tabPanel("Obligations Over Time",
                 plotOutput("trendPlot") %>% withSpinner(color = "#007bff")),
        
        tabPanel("Breakdown by Industry",
                 plotOutput("barPlot") %>% withSpinner(color = "#007bff")),
        
        tabPanel("Top NAICS Groups Over Time",
                 plotOutput("naicsTrendPlot") %>% withSpinner(color = "#007bff")),
        
        tabPanel("Top Agencies Over Time",
                 plotOutput("agencyTrendPlot") %>% withSpinner(color = "#007bff")),
        
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

## Server

server <- function(input, output, session) {
  drilldown_mode <- reactiveVal("counties")
  clicked_county <- reactiveVal(NULL)
  
  # --- Filtered Contracts ---
  filtered_data <- reactive({
    req(input$year)
    df <- fedcon %>% filter(fiscal_year == input$year)
    apply_all_filters(df, input)
  })
  
  # --- Filtered Survey ---
  filtered_survey <- reactive({
    req(input$survey_year)
    df <- sbcs %>% filter(year == input$survey_year)
    if (input$survey_state != "All") df <- df %>% filter(state == input$survey_state)
    df
  })
  
  # --- Summary Filters (for aggregates like totals) ---
  filtered_data_summary <- reactive({
    df <- fedcon %>% filter(fiscal_year == input$year)
    df <- apply_summary_filters(df, input)
    
    if (!is.null(clicked_county())) {
      df <- df %>% filter(county_fips == clicked_county())
    }
    
    df
  })
  
  # --- lazy filters --- 
  # Fix: Define filtered ACS data
  filtered_acs <- reactive({
    acs_summary %>% filter(year == input$year)
  })
  
  # Fix: Define county_data as alias for summary geometry
  county_data <- reactive({
    county_summary_data()
  })
  
  
  # --- County Summary Data (for map & panel) ---
  county_summary_data <- reactive({
    req(input$year)
    df <- county_sf %>%
      left_join(
        filtered_data() %>%
          group_by(county_fips) %>%
          summarise(total_obligations = sum(total_obligation, na.rm = TRUE), .groups = "drop"),
        by = c("GEOID" = "county_fips")
      ) %>%
      left_join(
        acs_summary %>% filter(year == input$year) %>% select(GEOID = geo_fips, population = total_population),
        by = "GEOID"
      ) %>%
      left_join(
        county_gdp %>% filter(year == input$year) %>% select(GEOID = geo_fips, gdp_millions),
        by = "GEOID"
      ) %>%
      mutate(
        gdp_dollars = gdp_millions * 1e6,
        obligation_per_capita = if_else(population > 0, total_obligations / population, NA_real_),
        obligation_pct_gdp = if_else(gdp_dollars > 0, total_obligations / gdp_dollars, NA_real_)
      )
    df
  })
  
  # --- County-Specific Reactive (used by value boxes, etc) ---
  selected_county_data <- reactive({
    req(clicked_county())
    county_summary_data() %>% filter(GEOID == clicked_county())
  })
  
  # --- Economic Comparison Reactive ---
  econ_compare_data <- reactive({
    agency_df <- fedcon %>%
      filter(fiscal_year == 2022, agency == input$econ_agency) %>%
      distinct(county_fips) %>%
      mutate(received_contract = TRUE)
    
    cbp %>%
      filter(year == 2022) %>%
      left_join(agency_df, by = "county_fips") %>%
      mutate(received_contract = ifelse(is.na(received_contract), FALSE, received_contract))
  })
  
  # --- Legend Title (used for maps) ---
  legend_title <- reactive({
    if (input$trends_tabs == "Distribution Explorer") {
      "% GDP Obligations"
    } else if (!is.null(input$econ_variable)) {
      case_when(
        input$econ_variable == "Median Income" ~ "Median Household Income ($)",
        input$econ_variable == "Poverty Rate" ~ "Poverty Rate (%)",
        input$econ_variable == "Small Biz Contracts" ~ "Small Business Contracts ($)",
        TRUE ~ ""
      )
    } else {
      NULL
    }
  })
  
  # --- Map Click Observer ---
  observeEvent(input$map_shape_click, {
    clicked_county(input$map_shape_click$id)
  })
  
  # --- Tab Memory ---
  observeEvent(clicked_county(), {
    if (!is.null(input$trends_tabs)) {
      updateTabsetPanel(session, "trends_tabs", selected = input$trends_tabs)
    }
  })



  # --- Map Initialization ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  # --- Map Polygons ---
  observe({
    req(drilldown_mode() == "counties", input$map_metric)
    df <- county_summary_data()
    metric <- input$map_metric
    pal <- colorQuantile("Blues", domain = df[[metric]], n = 5, na.color = "#cccccc")
    
    leafletProxy("map", session) %>%
      clearShapes() %>%
      addPolygons(
        data = df,
        fillColor = ~pal(.data[[metric]]),
        color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste0(NAME,
                        "<br>Total: ", scales::dollar(total_obligations),
                        "<br>Per Capita: ", scales::dollar(obligation_per_capita),
                        "<br>% GDP: ", scales::percent(obligation_pct_gdp, accuracy = 0.1)),
        layerId = ~GEOID
      ) %>%
      addLegend(
        pal = pal,
        values = df[[metric]],
        title = ifelse(metric == "obligation_per_capita", "Per Capita Obligations", "% of County GDP"),
        position = "bottomright",
        labFormat = if (metric == "obligation_pct_gdp") {
          labelFormat(suffix = "%", transform = function(x) x * 100)
        } else {
          labelFormat(prefix = "$", big.mark = ",")
        }
      )
  })
  
  # --- Map Bounds on Update ---
  observeEvent(county_summary_data(), {
    req(drilldown_mode() == "counties")
    df <- county_summary_data()
    if (nrow(df) == 0) return(NULL)
    bbox <- st_bbox(df)
    
    leafletProxy("map") %>%
      flyToBounds(lng1 = bbox$xmin, lat1 = bbox$ymin, lng2 = bbox$xmax, lat2 = bbox$ymax)
  })
  
  # --- Reset View Button ---
  observeEvent(input$reset_view, {
    clicked_county(NULL)
    leafletProxy("map") %>%
      flyTo(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  # --- Value Box: Total Obligations ---
  output$totalOblig <- renderValueBox({
    if (!is.null(clicked_county())) {
      df <- selected_county_data()
      total <- df$total_obligations
      subtitle <- df$NAME
    } else {
      total <- sum(filtered_data_summary()$total_obligation, na.rm = TRUE)
      subtitle <- "Total Obligations"
    }
    
    valueBox(
      value = if (!is.na(total)) scales::dollar(total) else "No Data",
      subtitle = subtitle,
      icon = icon("file-invoice-dollar"),
      color = "blue"
    )
  })
  
  # --- Value Box: GDP ---
  output$gdp <- renderValueBox({
    if (!is.null(clicked_county())) {
      df <- selected_county_data()
      gdp_value <- df$gdp_dollars
      subtitle <- paste(df$NAME, "GDP")
    } else {
      gdp_value <- state_gdp_clean %>%
        filter(year == input$year) %>%
        summarise(total_gdp = sum(gdp, na.rm = TRUE)) %>%
        pull(total_gdp) * 1e6
      subtitle <- "Total GDP"
    }
    
    valueBox(
      value = if (!is.na(gdp_value)) scales::dollar(gdp_value) else "No Data",
      subtitle = subtitle,
      icon = icon("landmark"),
      color = "green"
    )
  })
  
  # --- Value Box: Obligations as % GDP ---
  output$pctGDP <- renderValueBox({
    if (!is.null(clicked_county())) {
      df <- selected_county_data()
      pct <- df$obligation_pct_gdp
    } else {
      total_oblig <- sum(filtered_data_summary()$total_obligation, na.rm = TRUE)
      gdp_value <- state_gdp_clean %>%
        filter(year == input$year) %>%
        summarise(total_gdp = sum(gdp, na.rm = TRUE)) %>%
        pull(total_gdp) * 1e6
      pct <- if (!is.na(gdp_value) && gdp_value > 0) total_oblig / gdp_value else NA_real_
    }
    
    valueBox(
      value = if (!is.na(pct)) scales::percent(pct, accuracy = 0.01) else "No Data",
      subtitle = "Obligations as % GDP",
      icon = icon("percentage"),
      color = "orange"
    )
  })
  
  # --- Outputs: Economic Impact ---
  
  output$econ_valuebox <- renderValueBox({
    req(input$econ_outcome, input$econ_agency)
    df <- econ_compare_data()
    t <- t.test(df[[input$econ_outcome]] ~ df$received_contract)
    label_map <- c(total_emp = "Jobs", total_est = "Businesses", total_ap = "in Payroll ($)")
    label <- label_map[[input$econ_outcome]]
    diff <- t$estimate[[2]] - t$estimate[[1]]
    
    valueBox(
      value = paste0("+", scales::comma(round(diff))),
      subtitle = paste("Avg. Gain in", label, "\nfrom", input$econ_agency, "Contracts"),
      icon = icon("chart-line"),
      color = if (diff > 0) "green" else "red"
    )
  })
  
  output$econ_compare_plot <- renderPlotly({
    label_map <- c(total_emp = "Employment", total_est = "Establishments", total_ap = "Annual Payroll")
    label <- label_map[[input$econ_outcome]] %||% input$econ_outcome
    
    plot <- econ_compare_data() %>%
      mutate(contract_group = ifelse(received_contract, "Received Contract", "No Contract")) %>%
      ggplot(aes(x = contract_group, y = .data[[input$econ_outcome]])) +
      geom_boxplot(fill = "#2C3E50") +
      labs(title = paste("Economic Impact of", input$econ_agency, "Contracts (2022)"), y = label, x = NULL) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal(base_size = 14)
    
    ggplotly(plot)
  })
  
  output$econ_t_test <- renderText({
    df <- econ_compare_data()
    t <- t.test(df[[input$econ_outcome]] ~ df$received_contract)
    label_map <- c(total_emp = "jobs", total_est = "businesses", total_ap = "in payroll ($)")
    label <- label_map[[input$econ_outcome]] %||% input$econ_outcome
    
    paste0(
      "In 2022, counties that received contracts from ", input$econ_agency,
      " had an average of ", scales::comma(round(t$estimate[[2]])), " ", label,
      ", compared to ", scales::comma(round(t$estimate[[1]])), " in counties that did not.\n\n",
      "This difference of ", scales::comma(round(t$estimate[[2]] - t$estimate[[1]])),
      " is statistically significant (p < ", formatC(t$p.value, format = "e", digits = 2), ")."
    )
  })
  
  # --- Outputs: Top Counties & Time Trends ---
  
  output$distPlot <- renderPlot({
    req(input$dist_choice_value)
    df <- fedcon %>%
      filter(if (input$dist_type == "Agency") parent_agency == input$dist_choice_value else naics_group == input$dist_choice_value)
    
    top_counties <- df %>%
      group_by(county_fips) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      slice_max(total, n = 10) %>%
      left_join(county_sf %>% select(GEOID, NAME, STATEFP), by = c("county_fips" = "GEOID")) %>%
      left_join(state_sf %>% select(STATEFP, STUSPS), by = "STATEFP") %>%
      mutate(label = paste0(NAME, ", ", STUSPS)) %>%
      mutate(label = ifelse(is.na(label), county_fips, label))
    
    ggplot(top_counties, aes(x = reorder(label, total), y = total)) +
      geom_col(fill = "#1F77B4") +
      coord_flip() +
      labs(title = paste("Top 10 Counties by", input$dist_type), subtitle = input$dist_choice_value, x = "County", y = "Total Obligations ($)") +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  
  output$trendPlot <- renderPlot({
    req(input$agency, input$naics_group, input$minority, input$woman, input$veteran)
    df <- filtered_data() %>%
      filter(
        parent_agency %in% input$agency,
        naics_group %in% input$naics_group,
        is_minority_owned == (input$minority == "Yes" | input$minority == "All"),
        is_woman_owned == (input$woman == "Yes" | input$woman == "All"),
        is_veteran_owned == (input$veteran == "Yes" | input$veteran == "All")
      )
    
    df_national <- df %>%
      group_by(fiscal_year) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      mutate(scope = "National")
    
    df_county <- clicked_county() %>%
      {if (!is.null(.)) df %>% filter(county_fips == .) else NULL} %>%
      group_by(fiscal_year) %>%
      summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      mutate(scope = "Selected County")
    
    bind_rows(df_national, df_county) %>%
      ggplot(aes(x = fiscal_year, y = total, color = scope, group = scope)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(title = "Federal Obligations Over Time", x = "Year", y = "Total Obligations ($)", color = NULL) +
      scale_y_continuous(labels = scales::dollar) +
      scale_color_manual(values = c("National" = "gray60", "Selected County" = "#2C3E50")) +
      theme_minimal()
  })
  
  # --- Outputs: ACS Maps & Business Outcomes ---
  
  output$income_map <- renderLeaflet({
    df <- filtered_acs()
    df_map <- county_data() %>% left_join(df, by = c("GEOID" = "geo_fips")) %>% st_as_sf()
    pal <- colorQuantile("Greens", domain = df$median_household_income, n = 5)
    
    leaflet(df_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(median_household_income),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = ~paste0(NAME, "<br>Median Income: $", scales::comma(median_household_income))
      ) %>%
      addLegend(pal = pal, values = df$median_household_income, title = "Median Income", position = "bottomright")
  })
  
  output$poverty_map <- renderLeaflet({
    df <- filtered_acs()
    df_map <- county_data() %>% left_join(df, by = c("GEOID" = "geo_fips")) %>% st_as_sf()
    pal <- colorQuantile("Purples", domain = df$poverty_rate, n = 5)
    
    leaflet(df_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(poverty_rate),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = ~paste0(NAME, "<br>Poverty Rate: ", scales::percent(poverty_rate / 100, accuracy = 0.1))
      ) %>%
      addLegend(pal = pal, values = df$poverty_rate, title = "Poverty Rate (%)", position = "bottomright")
  })
  
  output$smallbiz_contract_map <- renderLeaflet({
    df <- filtered_data() %>%
      filter(is_small_business == TRUE) %>%
      group_by(county_fips) %>%
      summarise(total_small_contracts = sum(total_obligation, na.rm = TRUE), .groups = "drop")
    
    df_map <- county_data() %>% left_join(df, by = c("GEOID" = "county_fips")) %>% st_as_sf()
    pal <- colorQuantile("YlOrRd", domain = df$total_small_contracts, n = 5)
    
    leaflet(df_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(total_small_contracts),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = ~paste0(NAME, "<br>Small Business Contracts: $", scales::comma(total_small_contracts))
      ) %>%
      addLegend(pal = pal, values = df$total_small_contracts, title = "Small Business Contracts", position = "bottomright")
  })
  
  
  
  ## Tab memory
  observeEvent(clicked_county(), {
    if (!is.null(input$trends_tabs)) {
      updateTabsetPanel(session, "trends_tabs", selected = input$trends_tabs)
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
  ## Close Server
  }
  
  ## Launch App
  shinyApp(ui = ui, server = server)