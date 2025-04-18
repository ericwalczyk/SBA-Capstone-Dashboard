###############################################################################
################################ DO NOT USE ###################################
###################### FOR DOCUMENTATION PURPOSES ONLY ########################
################################ EW 4/18/25 ###################################
###############################################################################


# ───────────────────────────────────────────────────────────────────────────────
# 1) GLOBAL DATA LOAD + PREP
# ───────────────────────────────────────────────────────────────────────────────
library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(sf)
library(leaflet)
library(scales)

# your original federal contracts
fedcon <- read_csv("data/cleaned/all_fedcon.csv")
fedcon <- fedcon %>%
  filter(fiscal_year <= 2023)

year_range <- sort(unique(fedcon$fiscal_year))

# GDP files (pick the max year present)
state_gdp  <- read_csv("data/cleaned/state_gdp.csv") 
# Trim out the national‐level row and any other non‑state FIPS
state_gdp_cleaned<- state_gdp %>%
  filter(geo_fips != "00000") %>%         # drop the country line
  filter(!is.na(value)) %>%                # just in case there are other NAs
  rename(GDP = value)           # make the numeric column clear


county_gdp <- read_csv("data/cleaned/county_gdp.csv") 

county_gdp_cleaned <- county_gdp %>%
  # 1) dump the national‑level “000000” rows
  filter(geo_fips != "00000") %>%
  rename(
    county_fips = geo_fips,
    county_gdp   = gdp_millions
  )

# shapefiles (your existing ones)
states_sf   <- st_read("data/cleaned/cb_2020_us_state_500k/cb_2020_us_state_500k.shp")  %>% st_transform(4326)
counties_sf <- st_read("data/cleaned/cb_2020_us_county_500k/cb_2020_us_county_500k.shp") %>% st_transform(4326)

# ───────────────────────────────────────────────────────────────────────────────
# 2) UI & SERVER
# ───────────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
    titlePanel("Federal Contract Obligations by County and State"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("view_mode", "View Mode:", choices = c("State", "County"), selected = "State"),
        actionButton("reset", "Reset View"),
        sliderInput(
          "fiscal_year", "Year",
          min   = year_range[1],
          max   = year_range[length(year_range)],
          value = year_range[length(year_range)],  # default to the latest
          step  = 1,
          sep   = ""                                # no thousands sep
        ),
        selectInput(
          "parent_agency", "Agency",
          choices = c("All", sort(unique(fedcon$parent_agency))),
          selected = "All"
        ),
        selectInput(
          "naics_group", "NAICS Group",
          choices = c("All", sort(unique(fedcon$naics_group))),
          selected = "All"
        ),
        selectInput("is_minority_owned", "Minority Owned", choices = c("All", "TRUE", "FALSE")),
        selectInput("is_woman_owned", "Woman Owned", choices = c("All", "TRUE", "FALSE")),
        selectInput("is_veteran_owned", "Veteran Owned", choices = c("All", "TRUE", "FALSE"))
      ),
      mainPanel(
        leafletOutput("map", height = "700px")
      )
    )
  )

server <- function(input, output, session) {
  
  # filter by year/agency/etc.
  smcon <- fedcon
  smcon_filtered <- reactive({
    df <- smcon %>% filter(fiscal_year == input$fiscal_year)
    if (input$parent_agency != "All") df <- df %>% filter(parent_agency == input$parent_agency)
    if (input$naics_group   != "All") df <- df %>% filter(naics_group   == input$naics_group)
    df
  })
  
  # ──────────── STATE MAP DATA ────────────
  # ──────────── STATE MAP DATA ────────────
  state_map_data <- reactive({
    smcon_filtered() %>%
      rename(state_abbr = state) %>%
      group_by(state_abbr) %>%
      summarize(total_obligation = sum(total_obligation, na.rm = TRUE), .groups = "drop") %>%
      right_join(states_sf,    by = c("state_abbr" = "STUSPS")) %>%
      left_join(state_gdp_cleaned,     by = "state_abbr") %>%
      mutate(
        total_obligation = replace_na(total_obligation, 0),
        GDP              = coalesce(GDP, 1),   # ← here’s the swap
        pct_of_gdp       = total_obligation / GDP
      ) %>%
      st_as_sf()
  })
  
  # ──────────── COUNTY MAP DATA ────────────
  county_map_data <- reactive({
    # sum per‐county if you need to collapse multiples
    county_oblig <- smcon_filtered() %>%
      group_by(county_fips) %>%
      summarize(total_obligation = sum(total_obligation, na.rm = TRUE), .groups = "drop")
    
    counties_sf %>%
      left_join(county_oblig,   by = c("GEOID" = "county_fips")) %>%
      left_join(county_gdp_cleaned,     by = c("GEOID" = "county_fips")) %>%
      mutate(
        total_obligation = replace_na(total_obligation, 0),
        county_gdp        = replace_na(county_gdp, 1),
        pct_of_gdp        = total_obligation / county_gdp
      ) %>%
      st_as_sf()
  })
  
  # initial blank map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -98.58, lat = 39.83, zoom = 4)
  })
  
  
  getBins <- function(vals) {
    if (all(vals == 0)) return(c(0, 1))
    unique(c(0, 1, quantile(vals[vals>0], probs = seq(0.2,1,0.2))))
  }
  
  
  # reactive polygon drawing
  # Determine if we're displaying state or county data.
  df <- if (input$view_mode == "State") state_map_data() else county_map_data()
  vals <- df$total_obligation
  
  # Define breaks for the color bins.
  breaks <- getBins(vals)
  
  # Create a color palette function based on the data.
  pal <- colorBin("YlGnBu",
                  domain = vals,
                  bins = breaks,
                  na.color = "#f0f0f0",
                  right = FALSE)
  
  # Update the map using the leaflet proxy.
  proxy <- leafletProxy("map", data = df)
  proxy %>% clearShapes() %>% clearControls()
  
  if (input$view_mode == "State") {
    proxy %>%
      addPolygons(
        layerId = ~state_abbr,
        fillColor = ~pal(total_obligation),
        color = "white", weight = 1, opacity = 1, fillOpacity = 0.7,
        label = ~paste0(state_abbr, ": $",
                        formatC(total_obligation, big.mark = ",", format = "f", digits = 0)),
        highlightOptions = highlightOptions(color = "black", weight = 2)
      ) %>%
      addLegend("bottomright",
                pal = pal,
                values = vals,
                title = "State Obligations")
    
    # Optionally add label markers.
    data_for_labels <- states_sf %>%
      st_centroid() %>%
      left_join(state_overlay_data(), by = c("STUSPS" = "state_abbr"))
    
    if (nrow(data_for_labels) > 0) {
      proxy %>% addLabelOnlyMarkers(
        data = data_for_labels,
        lng = ~st_coordinates(geometry)[, 1],
        lat = ~st_coordinates(geometry)[, 2],
        label = ~paste0(
          STUSPS, "<br>",
          "$", format(round(total_obligation), big.mark = ","), "<br>",
          round(obligation_gdp_pct, 1), "% of GDP"
        ),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "font-size" = "10px", "color" = "#444",
            "text-align" = "center", "background" = "rgba(255,255,255,0.7)",
            "padding" = "2px"
          )
        )
      )
    } else {
      # Fallback in case there's no data for labels.
      proxy %>% addPolygons(
        layerId = ~state_abbr,
        fillColor = ~pal(total_obligation),
        color = "white", weight = 1, opacity = 1, fillOpacity = 0.7,
        label = ~paste0(state_abbr, ": $",
                        formatC(total_obligation, big.mark = ",", format = "f", digits = 0)),
        highlightOptions = highlightOptions(color = "black", weight = 2)
      )
    }
  }
}

# Observe clicks on shapes and react accordingly.
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

# Reset button to return to state-level view.
observeEvent(input$reset, {
  updateRadioButtons(session, "view_mode", selected = "State")
  leafletProxy("map") %>% setView(lng = -98.58, lat = 39.83, zoom = 4)
})


  # ──────────── RESET ALL FILTERS ────────────
  observeEvent(input$reset, {
    # put everything back to “All” (or your defaults)
    updateSelectInput(session, "parent_agency",    selected = "All")
    updateSelectInput(session, "naics_group",      selected = "All")
    updateSelectInput(session, "is_minority_owned",selected = "All")
    updateSelectInput(session, "is_woman_owned",   selected = "All")
    updateSelectInput(session, "is_veteran_owned", selected = "All")
    
    # reset year slider to latest available
    updateSliderInput(session, "fiscal_year",
                      value = max(fedcon$fiscal_year, na.rm = TRUE))
    
    # reset map view mode to “State”
    updateRadioButtons(session, "view_mode", selected = "State")
  })
  
  # optional: click drills down
  observeEvent(input$map_shape_click, {
    if (input$view_mode == "State") {
      updateRadioButtons(session, "view_mode", selected = "County")
    }
  })


shinyApp(ui, server)