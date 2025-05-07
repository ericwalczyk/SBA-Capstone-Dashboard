# --- Module: Small Business Contracts (contract_module.R) ---

contract_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("smallbiz_contract_map")),
    plotOutput(ns("barPlot"))
  )
}

contract_server <- function(id, county_data, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
  })
}