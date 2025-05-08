> business_apps <- readRDS("business_apps.rds")
> 
  > apps_long <- business_apps %>%
    +     # rename State → state
    +     rename(state = State) %>%
    +     # pivot 2005–2023 columns into a (year, applications) pair
    +     pivot_longer(
      +         cols      = `2005`:`2023`,
      +         names_to  = "year",
      +         values_to = "applications"
      +     ) %>%
    +     mutate(
      +         year = as.integer(year)
      +     )
  > 
    > # 2. LOAD & TIDY POPULATION
    > acs_summary <- readRDS("acs_summary.rds")
    > 
      > pop_long <- acs_summary %>%
        +     # already has lowercase state + year; just rename total_population → population
        +     rename(
          +         population = total_population
          +     ) %>%
        +     # keep only the three columns we need
        +     select(state, year, population) %>%
        +     mutate(year = as.integer(year))
      > 
        > # 3. UI
        > ui <- fluidPage(
          +     titlePanel("Business Applications & Population by State"),
          +     sidebarLayout(
            +         sidebarPanel(
              +             selectInput(
                +                 "variable", "Select variable:",
                +                 choices  = c("Applications", "Population"),
                +                 selected = "Applications"
                +             ),
              +             sliderInput(
                +                 "year", "Year:",
                +                 min   = min(c(apps_long$year, pop_long$year), na.rm = TRUE),
                +                 max   = max(c(apps_long$year, pop_long$year), na.rm = TRUE),
                +                 value = max(c(apps_long$year, pop_long$year), na.rm = TRUE),
                +                 step  = 1, sep = ""
                +             )
              +         ),
            +         mainPanel(
              +             plotlyOutput("barPlot", height = "600px")
              +         )
            +     )
          + )
        > 
          > # 4. SERVER
          > server <- function(input, output, session) {
            +     filtered <- reactive({
              +         if (input$variable == "Applications") {
                +             apps_long %>%
                  +                 filter(year == input$year) %>%
                  +                 group_by(state) %>%
                  +                 summarise(value = sum(applications, na.rm = TRUE), .groups = "drop")
                +         } else {
                  +             pop_long %>%
                    +                 filter(year == input$year) %>%
                    +                 group_by(state) %>%
                    +                 summarise(value = sum(population,   na.rm = TRUE), .groups = "drop")
                  +         }
              +     })
            +     
              +     output$barPlot <- renderPlotly({
                +         df <- filtered()
                +         plot_ly(
                  +             df,
                  +             x      = ~reorder(state, value),
                  +             y      = ~value,
                  +             type   = "bar",
                  +             marker = list(
                    +                 color = if (input$variable == "Applications") "royalblue" else "seagreen"
                    +             )
                  +         ) %>%
                  +             layout(
                    +                 title = paste(input$variable, "by State in", input$year),
                    +                 xaxis = list(title = "State", tickangle = -45),
                    +                 yaxis = list(title = input$variable)
                    +             )
                +     })
              + }
          > 
            > # 5. RUN THE APP
            > shinyApp(ui, server)
          