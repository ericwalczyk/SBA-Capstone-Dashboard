fedcon <- read_csv("data/cleaned/all_fedcon.csv")

fedcon <- fedcon %>%
  mutate(county_fips = sprintf("%05d", as.integer(county_fips)))

## adding in a parent agency column to clean up the huge number of agencies
fedcon <- fedcon %>%
  mutate(parent_agency = case_when(
    str_detect(agency, "Defense") ~ "Department of Defense",
    str_detect(agency, "Energy") ~ "Department of Energy",
    str_detect(agency, "Veterans Affairs") ~ "Department of Veterans Affairs",
    str_detect(agency, "Treasury|IRS|Mint") ~ "Department of the Treasury",
    str_detect(agency, "Agriculture") ~ "Department of Agriculture",
    str_detect(agency, "Commerce") ~ "Department of Commerce",
    str_detect(agency, "State") ~ "Department of State",
    str_detect(agency, "Interior") ~ "Department of the Interior",
    str_detect(agency, "Transportation") ~ "Department of Transportation",
    str_detect(agency, "Education") ~ "Department of Education",
    str_detect(agency, "Justice|FBI") ~ "Department of Justice",
    str_detect(agency, "Labor") ~ "Department of Labor",
    str_detect(agency, "Health|HHS|NIH|CDC") ~ "Department of Health and Human Services",
    TRUE ~ "Independent Agencies"
  ))

str(fedcon$county_fips)
str(fedcon$parent_agency)

counties_sf <- sf::st_read("data/cleaned/cb_2020_us_county_500k/cb_2020_us_county_500k.shp") %>%
  mutate(GEOID = as.character(GEOID))

counties_sf <- sf::st_read("data/cleaned/cb_2020_us_county_500k/cb_2020_us_county_500k.shp") %>%
  st_transform(crs = 4326) %>%
  mutate(GEOID = as.character(GEOID))

smcon <- fedcon %>%
  filter(total_obligation <= 250000)

map_data <- counties_sf %>%
  left_join(smcon, by = c("GEOID" = "county_fips"))




####### Define UI #######


ui <- fluidPage(
  titlePanel("Federal Contract Obligations by County"),
  sidebarPanel(
    width = 3,
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
    selectInput("parent_agency", "Agency",
                choices = c("All", sort(unique(smcon$parent_agency)))),
    selectInput("naics_group", "NAICS Group",
                choices = c("All", sort(unique(smcon$naics_group)))),
    selectInput("is_woman_owned", "Woman Owned",
                choices = c("All", sort(unique(smcon$is_woman_owned)))),
    selectInput("is_veteran_owned", "Veteran Owned",
                choices = c("All", sort(unique(smcon$is_veteran_owned))))
  ),
  mainPanel(
    leafletOutput("map", height = "700px")
  )
)

server <- function(input, output, session) {
  
  smcon_filtered <- reactive({
    req(input$fiscal_year)
    req(input$parent_agency)
    req(input$naics_group)
    req(input$is_woman_owned)
    req(input$is_veteran_owned)
    
    df <- smcon %>% filter(fiscal_year == input$fiscal_year)
    
    if (input$parent_agency != "All") {
      df <- df %>% filter(parent_agency == input$parent_agency)
    }
    
    if (input$naics_group != "All") {
      df <- df %>% filter(naics_group == input$naics_group)
    }
    
    if (input$is_woman_owned != "All") {
      df <- df %>% filter(is_woman_owned == as.logical(input$is_woman_owned))
    }
    
    if (input$is_veteran_owned != "All") {
      df <- df %>% filter(is_veteran_owned == as.logical(input$is_veteran_owned))
    }
    
    return(df)
  })
  
  map_data <- reactive({
    counties_sf %>%
      left_join(
        smcon_filtered() %>%
          group_by(county_fips) %>%
          summarize(total_obligation = sum(total_obligation, na.rm = TRUE),
                    .groups = "drop"),
        by = c("GEOID" = "county_fips")
      ) %>%
      mutate(total_obligation = ifelse(is.na(total_obligation), 0, total_obligation))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.58, lat = 39.83, zoom = 4)
  })
  
  observe({
    req(map_data())
    df <- map_data()
    vals <- df$total_obligation
    
    if (all(vals == 0 | is.na(vals))) {
      breaks <- c(0, 1)
    } else {
      qtiles <- quantile(vals[vals > 0], probs = c(0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
      breaks <- unique(c(0, 1, qtiles))
    }
    
    pal <- colorBin(
      palette = "YlGnBu",
      domain = vals,
      bins = breaks,
      na.color = "#f0f0f0",
      right = FALSE
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

# Launch the app
shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)
