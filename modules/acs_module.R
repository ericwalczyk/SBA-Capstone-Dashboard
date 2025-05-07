## modular ACS code
# --- Module: ACS Overview (acs_module.R) ---

acs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("income_map")),
    leafletOutput(ns("poverty_map")),
    plotOutput(ns("employment_trends"))
  )
}

acs_server <- function(id, county_data, filtered_acs, acs_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$income_map <- renderLeaflet({
      df <- filtered_acs()
      pal <- colorQuantile("Greens", domain = df$median_household_income, n = 5)
      df_map <- county_data() %>% left_join(df, by = c("GEOID" = "geo_fips")) %>% st_as_sf()
      
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
      pal <- colorQuantile("Purples", domain = df$poverty_rate, n = 5)
      df_map <- county_data() %>% left_join(df, by = c("GEOID" = "geo_fips")) %>% st_as_sf()
      
      leaflet(df_map) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(poverty_rate),
          color = "black", weight = 1, fillOpacity = 0.7,
          label = ~paste0(NAME, "<br>Poverty Rate: ", scales::percent(poverty_rate/100, accuracy = 0.1))
        ) %>%
        addLegend(pal = pal, values = df$poverty_rate, title = "Poverty Rate (%)", position = "bottomright")
    })
    
    output$employment_trends <- renderPlot({
      df <- acs_data()
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
  })
}
