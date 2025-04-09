#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

fedcon <- read_csv("data/cleaned/all_fedcon.csv")

smcon <- fedcon %>%
  filter(award_amount <= 250000)

## I am going to write a function to group naics industries into 10 broad
## categories by pulling the first two digits of the naics_code and using it
## to assign to the broader industry group

add_naics_group <- function(naics_code) {
  sector <- substr(naics_code, 1, 2)
  case_when(
    sector %in% c("11", "21") ~ "Agriculture & Mining",
    sector %in% c("22") ~ "Utilities & Energy",
    sector %in% c("23") ~ "Construction",
    sector %in% c("31", "32", "33") ~ "Manufacturing",
    sector %in% c("42", "44", "45") ~ "Wholesale & Retail Trade",
    sector %in% c("48", "49") ~ "Transportation & Warehousing", 
    sector %in% c("51") ~ "Information & Technology",
    sector %in% c("54") ~ "Professional, Scientific & Technical Services",
    sector %in% c("56", "61", "92") ~ "Public Admin & Support Services",
    sector %in% c("62") ~ "Healthcare & Social Assistance",
    TRUE ~ "Other"
  )
}

fedcon <- fedcon %>%
  mutate(
    naics_code = as.character(naics_code),
    naics_group = add_naics_group(naics_code)
  )

## Now i'm going to check the distribution of all the contracts, then will
## check the distribution of contracts by industry group so I can get an idea
## of the ranges for building the dashboard component
library(dplyr)

summary_stats <- fedcon %>%
  mutate(under_250k = total_obligation <= 250000) %>%  # Create a logical TRUE/FALSE column
  group_by(under_250k) %>%
  summarize(
    count = n(),  # how many contracts
    avg_value = mean(total_obligation, na.rm = TRUE),
    median_value = median(total_obligation, na.rm = TRUE),
    max_value = max(total_obligation, na.rm = TRUE),
    min_value = min(total_obligation, na.rm = TRUE)
  )

print(summary_stats)

## A vast majority of contracts are less than $250k, which is good for our
## project if we want to zone in on small contracts to small businesses

## The function worked so now I will build a distribution map
## that can be filtered by year, agency, and naics group

library(leaflet) # for interactive map
library(sf) # for geography data
library(tigris) # for geography data

## I need to convert the naics code from numeric to character
## and add leading zeros so that it will join correctly with 
## the county map data GEOID 

fedcon <- fedcon %>%
  mutate(county_fips = sprintf("%05d", as.integer(county_fips)))

str(fedcon$county_fips)


## now I'll add the county map data, which I had to download because I was
## getting a server error trying to pull from the package. Investigate later

counties_sf <- sf::st_read("data/cleaned/cb_2020_us_county_500k/cb_2020_us_county_500k.shp") %>%
  mutate(GEOID = as.character(GEOID))

fedcon_filtered_test <- fedcon %>%
  filter(
    year == 2022,,
    agency == "Department of Defense",,
    naics_group == "Manufacturing"
  ) %>%
  group_by(county_fips) %>%
  summarize(total_obligation = sum(total_obligation, na.rm = TRUE))

map_data <- counties_sf %>%
  left_join(fedcon, by = c("GEOID" = "county_fips"))

glimpse(map_data)

## it all worked so now I'm going to put it into a shiny map. I am trying
## to make a heatmap showing the distribution, with a slider input for the 
## year value and drop down menus for the naics and agency

# Define UI
ui <- fluidPage(
  titlePanel("Federal Contract Obligations by County"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year",
                  min = min(fedcon$fiscal_year),
                  max = max(fedcon$fiscal_year),
                  value = min(fedcon$fiscal_year),
                  step = 1,
                  sep = ""),
      selectInput("agency", "Agency", 
                  choices = c("All", sort(unique(fedcon$agency)))),
      selectInput("naics_group", "NAICS Group", 
                  choices = c("All", sort(unique(fedcon$naics_group))))
    ),
    mainPanel(
      leafletOutput("map", height = "700px")
    )
  )
)


# Define server 
server <- function(input, output, session) {
  filtered_data <- reactive({
    df <- fedcon %>%
      filter(year == input$year)
    
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
      summarize(total_obligation = sum(total_obligation, na.rm = TRUE), .groups = "drop")
  })
  
  
  ## COME BACK AND ADD STATE OUTLINES
  
  
  ## joining filtered data with map geometry
  map_data <- reactive({
    counties_sf %>%
      left_join(filtered_data(), by = c("GEOID" = "county_fips")) %>%
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
