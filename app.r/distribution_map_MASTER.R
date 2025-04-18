fedcon <- read_csv("data/cleaned/all_fedcon.csv")
cgdp <- read_csv("data/cleaned/county_gdp.csv")
sgdp <- read_csv("data/cleaned/state_gdp.csv")

counties_sf <- sf::st_read("data/cleaned/cb_2020_us_county_500k/cb_2020_us_county_500k.shp") %>%
  st_transform(crs = 4326) %>%
  mutate(GEOID = as.character(GEOID))

states_sf <- st_read("data/cleaned/cb_2020_us_state_500k/cb_2020_us_state_500k.shp") %>%
  st_transform(4326) %>%
  mutate(state = as.character(STUSPS))  # this avoids needing STUSPS directly

smcon <- fedcon %>%
  filter(total_obligation <= 250000) %>%
  select(county_fips, state, total_obligation, parent_agency,
         naics_group, is_minority_owned, is_woman_owned, is_veteran_owned, everything())

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
      selectInput("is_minority_owned", "Minority Owned", choices = c("ALL", "TRUE","FALSE")),
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
    ) %>%
    select(county_fips, state, total_obligation, parent_agency,
           naics_group, is_minority_owned, is_woman_owned, is_veteran_owned, everything())
  
  
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
    if (input$is_minority_owned != "All") df <- df %>%
      filter(is_minority_owned == as.logical(input$is_minority_owned))
    if (input$is_woman_owned != "All") df <- df %>%
      filter(is_woman_owned == as.logical(input$is_woman_owned))
    if (input$is_veteran_owned != "All") df <- df %>%
      filter(is_veteran_owned == as.logical(input$is_veteran_owned))
    df
  })
  
  state_map_data <- reactive({
    df <- smcon_filtered()
    req(nrow(df) > 0)  # ⛑ ensures there’s data to work with
    
    df %>%
      rename(state_abbr = state) %>%
      group_by(state_abbr) %>%
      summarize(total_obligation = sum(total_obligation, na.rm = TRUE),
                .groups = "drop") %>%
      right_join(states_sf, by = c("state_abbr" = "STUSPS")) %>%
      mutate(total_obligation = ifelse(is.na(total_obligation), 0, total_obligation)) %>%
      st_as_sf()
  })
  
  county_map_data <- reactive({
    counties_sf %>%
      left_join(
        smcon_filtered() %>%
          group_by(county_fips, state) %>%
          summarize(
            total_obligation = sum(total_obligation, na.rm = TRUE),
            .groups = "drop"
          ),
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



