# --- Module: Economic Impact Comparison (impact_module.R) ---

impact_ui <- function(id) {
  ns <- NS(id)
  tagList(
    valueBoxOutput(ns("econ_valuebox")),
    plotlyOutput(ns("econ_compare_plot")),
    verbatimTextOutput(ns("econ_t_test"))
  )
}

impact_server <- function(id, econ_compare_data, input_outcome, input_agency) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    label_map <- c(
      total_emp = "jobs",
      total_est = "businesses",
      total_ap  = "in payroll ($)"
    )
    
    output$econ_valuebox <- renderValueBox({
      df <- econ_compare_data()
      t <- t.test(df[[input_outcome()]] ~ df$received_contract)
      treated <- t$estimate[[2]]
      untreated <- t$estimate[[1]]
      diff <- treated - untreated
      label <- label_map[[input_outcome()]] %||% input_outcome()
      
      valueBox(
        value = paste0("+", scales::comma(round(diff))),
        subtitle = paste("Avg. Gain in", label, "\nfrom", input_agency(), "Contracts"),
        icon = icon("chart-line"),
        color = if (diff > 0) "green" else "red"
      )
    })
    
    output$econ_compare_plot <- renderPlotly({
      label <- label_map[[input_outcome()]] %||% input_outcome()
      df <- econ_compare_data() %>%
        mutate(contract_group = ifelse(received_contract, "Received Contract", "No Contract"))
      
      plot <- ggplot(df, aes(x = contract_group, y = .data[[input_outcome()]])) +
        geom_boxplot(fill = "#2C3E50") +
        labs(
          title = paste("Economic Impact of", input_agency(), "Contracts (2022)"),
          x = "", y = label
        ) +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal(base_size = 14)
      
      ggplotly(plot)
    })
    
    output$econ_t_test <- renderText({
      df <- econ_compare_data()
      t <- t.test(df[[input_outcome()]] ~ df$received_contract)
      treated_mean <- t$estimate[[2]]
      untreated_mean <- t$estimate[[1]]
      diff <- treated_mean - untreated_mean
      label <- label_map[[input_outcome()]] %||% input_outcome()
      
      paste0(
        "In 2022, counties that received contracts from ", input_agency(),
        " had an average of ", scales::comma(round(treated_mean)), " ", label,
        ", compared to ", scales::comma(round(untreated_mean)), " in counties that did not.\n\n",
        "This difference of ", scales::comma(round(diff)), " is statistically significant (p < ",
        formatC(t$p.value, format = "e", digits = 2), ")."
      )
    })
  })
}
