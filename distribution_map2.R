fedcon <- read_csv("data/cleaned/all_fedcon.csv")


counties_sf <- sf::st_read("data/cleaned/cb_2020_us_county_500k/cb_2020_us_county_500k.shp") %>%
  mutate(GEOID = as.character(GEOID))


map_data <- counties_sf %>%
  left_join(fedcon, by = c("GEOID" = "county_fips"))


# Define UI 
ui <- fluidPage(
  titlePanel("Federal Contract Obligations by County"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("fiscal_year", "Year",
                  min = min(fedcon$fiscal_year),
                  max = max(fedcon$fiscal_year),
                  value = min(fedcon$fiscal_year),
                  step = 1,
                  sep = ""),
      selectInput("agency", "Agency", 
                  choices = c("All", sort(unique(fedcon$agency)))),
      selectInput("naics_group", "NAICS Group", 
                  choices = c("All", sort(unique(fedcon$naics_group)))),
      selectInput("is_small_business", "Small Business",
                  choices = c("All", sort(unique(fedcon$is_small_business)))),
      selectInput("is_woman_owned", "Woman Owned", 
                  choices = "All", sort(unique(fedcon$is_woman_owned)))),
    selectInput("is_veteran_owned", "Veteran Owned", 
                choices = "All", sort(unique(fedcon$is_veteran_owned)))
  ),
  mainPanel(
    leafletOutput("map", height = "700px")
  )
)


# Define server logic 
server <- function(input, output, session) {
  fedcon_filtered <- reactive({
    df <- fedcon %>%
      filter(fiscal_year == input$fiscal_year)
    
    if (input$agency != "All") {
      df <- df %>% filter(agency == input$agency)
    }
    
    if (input$naics_group != "All") {
      df <- df %>% filter(naics_group == input$naics_group)
    }
    
    # adding a $250,000 max contract value filter for stability 
    df <- df %>% filter(total_obligation <= 250000)
    
    df %>%
      group_by(county_fips) %>%
      summarize(total_obligation = sum(total_obligation, na.rm = TRUE),
                .groups = "drop")
  })
  map_data <- reactive({
    filtered_df <- fedcon_filtered()  
    counties_sf %>%
      left_join(filtered_df, by = c("GEOID" = "county_fips")) %>%
      mutate(total_obligation = ifelse(is.na(total_obligation), 0, total_obligation))
  })
  
  # rendering the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.58, lat = 39.83, zoom =4) # pinning map to US
  })
  
  observe({
    req(map_data())
    pal <- colorQuantile("YlGnBu", domain = map_data()$total_obligation, n = 5)
    
    leafletProxy("map", data = map_data()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(total_obligation),
        color = "#333333",
        weight = 0.4,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste0(NAME, " (", GEOID, "): $",
                        format(round(total_obligation), big.mark = ",")),
        highlight = highlightOptions(weight = 2, color = "white",
                                     bringToFront = TRUE)
      ) %>% 
      clearControls() %>%
      addLegend(
        pal = pal,
        values = ~total_obligation,
        position = "bottomright",
        title = "Total Obligation ($)", 
        labFormat = labelFormat(prefix = "$", big.mark = ",")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
