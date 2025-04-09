fedcon <- read_csv("data/cleaned/all_fedcon.csv")

fedcon <- fedcon %>%
  mutate(county_fips = sprintf("%05d", as.integer(county_fips)))

str(fedcon$county_fips)

counties_sf <- sf::st_read("data/cleaned/cb_2020_us_county_500k/cb_2020_us_county_500k.shp") %>%
  mutate(GEOID = as.character(GEOID))

fedcon_filtered_test <- fedcon %>%
  filter(
    fiscal_year == 2022,,
    agency == "Department of Defense",,
    naics_group == "Manufacturing"
  ) %>%
  group_by(county_fips) %>%
  summarize(total_obligation = sum(total_obligation, na.rm = TRUE))

map_data <- counties_sf %>%
  left_join(fedcon, by = c("GEOID" = "county_fips"))


# Define UI 
ui <- fluidPage(
  titlePanel("Federal Contract Obligations by County"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year",
                  min = min(fedcon$year),
                  max = max(fedcon$year),
                  value = min(fedcon$year),
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


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  fedcon <- reactive({
    df <- fedcon %>%
      filter(year == input$year)
    
    if (input$agency != "All") {
      df <- df %>% filter(agency == input$agency)
    }
    
    if (input$naics_group != "All") {
      df <- df %>% filter(naics_group == input$naics_group)
    }
    
   if (input$is_small_business != "All") {
     df <- df %>% filter(is_small_business == input$is_small_business)
   }
    if (input$is_woman_owned != "All") {
      df <- df %>% filter(is_woman_owned == input$is_woman_owned)
    }
    if (input$is_veteran_owned != "All") {
      df <- df %>% filter(is_veteran_owned == input$is_veteran_owned)
    }
    df %>%
      group_by(county_fips) %>%
      summarize(total_obligation = sum(total_obligation, na.rm = TRUE),
                .groups = "drop")
  })
  map_data <- reactive({
    counties_sf %>%
      left_join(fedcon(), by = c("GEOID" = "county_fips")) %>%
      mutate(total_obligation = ifelse(is.na(total_obligation),
                                       0, total_obligation))
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
