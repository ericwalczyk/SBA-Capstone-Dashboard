fedcon <- read_csv("data/cleaned/all_fedcon.csv")

fedcon <- fedcon %>%
  mutate(county_fips = sprintf("%05d", as.integer(county_fips)))

str(fedcon$county_fips)

counties_sf <- sf::st_read("data/cleaned/cb_2020_us_county_500k/cb_2020_us_county_500k.shp") %>%
  mutate(GEOID = as.character(GEOID))

counties_sf <- sf::st_read("data/cleaned/cb_2020_us_county_500k/cb_2020_us_county_500k.shp") %>%
  st_transform(crs = 4326) %>%
  mutate(GEOID = as.character(GEOID))

smcon <- fedcon %>%
  filter(total_obligation <= 250000)

map_data <- counties_sf %>%
  left_join(smcon, by = c("GEOID" = "county_fips"))


# Define UI 
ui <- fluidPage(
  titlePanel("Federal Contract Obligations by County"),
  sidebarPanel(
    width = 3,  # reduces the width
    tags$style(HTML("
    .well {
      background-color: #f8f9fa;
      border-radius: 10px;
      padding: 10px;
      box-shadow: 1px 1px 3px rgba(0,0,0,0.1);
    }
    .form-group {
      margin-bottom: 10px;
    }
    label {
      font-weight: 500;
    }
  ")),
    sliderInput("fiscal_year", "Year",
                min = min(smcon$fiscal_year),
                max = max(smcon$fiscal_year),
                value = min(smcon$fiscal_year),
                step = 1, sep = ""),
    selectInput("agency", "Agency", choices = c("All", sort(unique(smcon$agency)))),
    selectInput("naics_group", "NAICS Group", choices = c("All", sort(unique(smcon$naics_group)))),
    selectInput("is_woman_owned", "Woman Owned", choices = c("All", sort(unique(smcon$is_woman_owned)))),
    selectInput("is_veteran_owned", "Veteran Owned", choices = c("All", sort(unique(smcon$is_veteran_owned))))
  ),
  
  mainPanel(
    leafletOutput("map", height = "700px")
  )
)


# Define server logic 
server <- function(input, output, session) {
  smcon_filtered <- reactive({
    df <- smcon %>%
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
    filtered_df <- smcon_filtered()  # force evaluation here
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
    
    df <- map_data()
    vals <- df$total_obligation
    
    # Custom bin breaks:
    # First bin: $0
    # Second bin: $1 to ~20th percentile
    breaks <- c(0, 1, quantile(vals[vals > 0], probs = c(0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE))
    
    # Remove duplicated breaks (can happen if lots of zeros or low spread)
    breaks <- unique(breaks)
    
    pal <- colorBin(
      palette = "YlGnBu",
      domain = vals,
      bins = breaks,
      na.color = "#f0f0f0",
      right = FALSE  # make 0 exclusive from 1+
    )
    
    leafletProxy("map", data = df) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(total_obligation),
        color = "#333333",
        weight = 0.4,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste0(NAME, " (", GEOID, "): $", formatC(total_obligation, 
                                                           format = "f", 
                                                           digits = 0, 
                                                           big.mark = ","))
      ) %>%
      clearControls() %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = vals,
        title = "Total Obligation",
        labFormat = labelFormat(prefix = "$"),
        opacity = 0.7
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
