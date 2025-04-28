# Load Data Reactively
fedcon_data <- reactive({ fedcon })
state_data <- reactive({ state_sf })
county_data <- reactive({ county_sf })
acs_data <- reactive({ acs_summary })
sbcs_data <- reactive({ sbcs })

# Filtered Federal Contract Data
filtered_data <- reactive({
  df <- fedcon_data() %>%
    filter(fiscal_year == input$year)
  
  if (!is.null(input$agency)) df <- df %>% filter(parent_agency %in% input$agency)
  if (!is.null(input$naics_group)) df <- df %>% filter(naics_group %in% input$naics_group)
  if (input$minority != "All") df <- df %>% filter(is_minority_owned == (input$minority == "Yes"))
  if (input$woman != "All") df <- df %>% filter(is_woman_owned == (input$woman == "Yes"))
  if (input$veteran != "All") df <- df %>% filter(is_veteran_owned == (input$veteran == "Yes"))
  
  if (!is.null(clicked_state())) {
    state_abbr <- state_data() %>% filter(STATEFP == clicked_state()) %>% pull(STUSPS)
    df <- df %>% filter(state == state_abbr)
  }
  df
})

# Filtered ACS Data
filtered_acs <- reactive({
  df <- acs_data() %>% filter(year == input$acs_year)
  if (input$acs_state != "All") {
    df <- df %>% filter(state == input$acs_state)
  }
  df
})

# Filtered SBCS Data
filtered_survey <- reactive({
  req(input$survey_year)
  df <- sbcs_data() %>% filter(year == input$survey_year)
  if (input$survey_state != "All") {
    df <- df %>% filter(state == input$survey_state)
  }
  df
})

# State Summary
state_summary <- reactive({
  df <- filtered_data() %>%
    group_by(state) %>%
    summarise(total_obligation = sum(total_obligation, na.rm = TRUE), .groups = "drop")
  
  gdp_year <- state_gdp %>%
    filter(year == input$year) %>%
    mutate(gdp_dollars = gdp * 1e6)
  
  left_join(state_data(), df, by = c("STUSPS" = "state")) %>%
    left_join(gdp_year, by = c("STUSPS" = "state_abbr")) %>%
    mutate(pct_gdp = total_obligation / gdp_dollars)
})

# County Summary
county_summary <- reactive({
  req(clicked_state())
  fips <- clicked_state()
  
  counties <- county_data() %>% filter(STATEFP == fips)
  
  counties %>%
    left_join(
      filtered_data() %>%
        group_by(county_fips) %>%
        summarise(total = sum(total_obligation, na.rm = TRUE), .groups = "drop"),
      by = c("GEOID" = "county_fips")
    ) %>%
    left_join(
      county_gdp %>% filter(year == input$year),
      by = c("GEOID" = "geo_fips")
    ) %>%
    mutate(gdp_dollars = gdp_millions * 1e6, pct_gdp = total / gdp_dollars)
})
