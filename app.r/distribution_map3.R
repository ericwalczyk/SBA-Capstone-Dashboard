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
write_csv(fedcon, "data/cleaned/fedcon_preprocessed.csv")

str(fedcon$county_fips)
str(fedcon$parent_agency)

## Adding county and state GDP files. I want to add them such that when I join
## them to the dashboard, when you open the app the state abbreviation, 
## total obligation amount for the given filters, and the percent of GDP those 
## obligations make up for the state to appear as text. 
## Then when you click on a state it zooms in on the county map and opens a 
## separate "info window" that shows the state demographic information, state 
## GDP info, and the percent of GDP contracts make up sorted by department and
## industry. Then, when clicking on a county, it the text box gives you the 
## same county level info.




####################### County GDP ############################

cgdp <- read_csv("data/raw/county_gdp/CAGDP2__ALL_AREAS_2001_2023.csv")

cgdp_clean <- cgdp %>%
  select(
    geo_fips = GeoFIPS,
    geo_name = GeoName,
    industry_code = IndustryClassification,
    description = Description,
    `2017`:`2023`
  )
cgdp_clean <- cgdp_clean %>%
  mutate(across(`2017`:`2023`, ~ as.numeric(.x) / 1000))


## making long for merging later
cgdp_long <- cgdp_clean %>%
  pivot_longer(
    cols = `2017`:`2023`,
    names_to = "year",
    values_to = "gdp_millions"
  )


## The county level data includes the industry grouping so i'm using the same
## function I usedo on the contract data to grab the first two digits of the 
## NAICS code and assigning it to the corresponding NAICS group 
cgdp_long <- cgdp_long %>%
  mutate(
    sector = str_extract(industry_code, "^\\d{2}"),  # extract leading 2 digits
    sector = as.character(sector)  # ensure it's a character for matching
  )

add_naics_group <- function(sector) {
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

cgdp_long <- cgdp_long %>%
  mutate(naics_group = add_naics_group(sector))



####################### State GDP ###########################

sgdp <- read_csv("data/raw/state_gdp/SASUMMARY__ALL_AREAS_1998_2023.csv")

sgdp_filtered <- sgdp %>%
  filter(
    Description %in% c(
      "Real GDP (millions of chained 2017 dollars) 1/",
      "Real per capita personal income 4/",
      "Total employment (number of jobs)"
    )
  ) %>%
  select(GeoFIPS, GeoName, Description, `2017`:`2023`)

## pivot longer to clean format
sgdp_long <- sgdp_filtered %>%
  pivot_longer(
    cols = `2017`:`2023`,
    names_to = "year",
    values_to = "value"
  )

## clean column names
sgdp_long <- sgdp_long %>%
  clean_names()
counties_sf <- sf::st_read("data/cleaned/cb_2020_us_county_500k/cb_2020_us_county_500k.shp") %>%
  st_transform(crs = 4326) %>%
  mutate(GEOID = as.character(GEOID))

## add state abbr by joining with built-in R vectors
state_lookup <- tibble(
  geo_name = state.name,
  state_abbr = state.abb
)

sgdp_final <- sgdp_long %>%
  mutate(geo_name = str_trim(geo_name)) %>%  # ensure spacing matches
  left_join(state_lookup, by = "geo_name")



## adding in a state level map. The idea is that the map will display with 
## state level obligations by default. You can then filter by agency, NAICS,
## woman-owned, etc. If you click on a state, it will open up the county view
## with the county level distribution and same filters. 

states_sf <- st_read("data/cleaned/cb_2020_us_state_500k/cb_2020_us_state_500k.shp") %>%
  st_transform(4326) %>%
  mutate(state = as.character(STUSPS))  # this avoids needing STUSPS directly

smcon <- fedcon %>%
  filter(total_obligation <= 250000) %>%
  select(county_fips, state, total_obligation, parent_agency,
         naics_group, is_woman_owned, is_veteran_owned, everything())

map_data <- counties_sf %>%
  left_join(smcon, by = c("GEOID" = "county_fips"))




############################## Define UI #################################


ui <- fluidPage(
  titlePanel("Federal Contract Obligations by County and State"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("view_mode", "View Mode:", choices = c("State", "County"), selected = "State"),
      actionButton("reset", "Reset View"),
      sliderInput("fiscal_year", "Year", min = 2023, max = 2023, value = 2023, step = 1),
      selectInput("parent_agency", "Agency", choices = c("All")),
      selectInput("naics_group", "NAICS Group", choices = c("All")),
      selectInput("is_woman_owned", "Woman Owned", choices = c("All", "TRUE", "FALSE")),
      selectInput("is_veteran_owned", "Veteran Owned", choices = c("All", "TRUE", "FALSE"))
    ),
    mainPanel(
      leafletOutput("map", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  smcon <- read_csv("data/cleaned/all_fedcon.csv") %>%
    mutate(
      county_fips = sprintf("%05d", as.integer(county_fips)),
      parent_agency = case_when(
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
      )
    )
  
  observe({
    updateSelectInput(session, "parent_agency",
                      choices = c("All", sort(unique(smcon$parent_agency))))
    updateSelectInput(session, "naics_group",
                      choices = c("All", sort(unique(smcon$naics_group))))
    updateSliderInput(session, "fiscal_year", 
                      min = min(smcon$fiscal_year), 
                      max = max(smcon$fiscal_year), 
                      value = min(smcon$fiscal_year))
  })
  
  smcon_filtered <- reactive({
    req(input$fiscal_year)
    df <- smcon %>% filter(fiscal_year == input$fiscal_year)
    if (input$parent_agency != "All") df <- df %>% 
      filter(parent_agency == input$parent_agency)
    if (input$naics_group != "All") df <- df %>%
      filter(naics_group == input$naics_group)
    if (input$is_woman_owned != "All") df <- df %>%
      filter(is_woman_owned == as.logical(input$is_woman_owned))
    if (input$is_veteran_owned != "All") df <- df %>%
      filter(is_veteran_owned == as.logical(input$is_veteran_owned))
    df
  })
  
  state_map_data <- reactive({
    smcon_filtered() %>%
      rename(state_abbr = state) %>%
      group_by(state_abbr) %>%
      summarize(total_obligation = sum(total_obligation, na.rm = TRUE),
                .groups = "drop") %>%
      right_join(states_sf, by = c("state_abbr" = "STUSPS")) %>%
      mutate(total_obligation = ifelse(is.na(total_obligation),
                                       0, total_obligation)) %>%
      st_as_sf()
  })
  
  county_map_data <- reactive({
    counties_sf %>%
      left_join(
        smcon_filtered() %>%
          group_by(county_fips) %>%
          summarize(state = first(state),
                    total_obligation = sum(total_obligation, na.rm = TRUE),
                    .groups = "drop"),
        by = c("GEOID" = "county_fips")
      ) %>%
      mutate(total_obligation = ifelse(is.na(total_obligation),
                                       0, total_obligation)) %>%
      st_as_sf()
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.58, lat = 39.83, zoom = 4)
  })
  
  observe({
    df <- if (input$view_mode == "State") state_map_data() else county_map_data()
    vals <- df$total_obligation
    
    breaks <- if (all(vals == 0 | is.na(vals))) {
      c(0, 1)
    } else {
      unique(c(0, 1, quantile(vals[vals > 0], 
                              probs = seq(0.2, 1, by = 0.2), na.rm = TRUE)))
    }
    
    pal <- colorBin("YlGnBu",
                    domain = vals,
                    bins = breaks,
                    na.color = "#f0f0f0", 
                    right = FALSE)
    
    proxy <- leafletProxy("map", data = df)
    proxy %>% clearShapes() %>% clearControls()
    
    if (input$view_mode == "State") {
      proxy %>%
        addPolygons(
          layerId = ~state_abbr,
          fillColor = ~pal(total_obligation),
          color = "white", weight = 1, opacity = 1, fillOpacity = 0.7,
          label = ~paste0(state_abbr, ": $",
                          formatC(total_obligation,big.mark = ",", 
                                  format = "f", digits = 0)),
          highlightOptions = highlightOptions(color = "black", weight = 2)
        ) %>%
        
        addLegend("bottomright",
                  pal = pal,
                  values = vals,
                  title = "State Obligations")
    
    } else {
      proxy %>%
        addPolygons(
          fillColor = ~pal(total_obligation),
          color = "#333333", weight = 0.4, opacity = 1, fillOpacity = 0.7,
          label = ~paste0(NAME, ": $",
                          formatC(total_obligation,
                                  big.mark = ",",
                                  format = "f",
                                  digits = 0))
        ) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = vals,
                  title = "County Obligations")
    }
  })
  
  observeEvent(input$map_shape_click, {
    if (input$view_mode == "State") {
      clicked_state <- input$map_shape_click$id
      updateRadioButtons(session, "view_mode", selected = "County")
      
      isolate({
        bbox <- st_bbox(states_sf[states_sf$state == clicked_state, ])
        leafletProxy("map") %>%
          flyToBounds(lng1 = bbox$xmin, 
                      lat1 = bbox$ymin, 
                      lng2 = bbox$xmax, 
                      lat2 = bbox$ymax)
      })
    }
  })
  
  observeEvent(input$reset, {
    updateRadioButtons(session, "view_mode", selected = "State")
    leafletProxy("map") %>% setView(lng = -98.58, lat = 39.83, zoom = 4)
  })
}

###### Run App ######
shinyApp(ui, server)



