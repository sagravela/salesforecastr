# SERVER LOGIC

## Server Logic
server <- function(input, output, session) {
  # Observe change in store_id and update upc_id options accordingly
  observeEvent(input$store_id, {
    # Filter UPC IDs based on selected store_id
    filtered_upc_ids <- transaction %>%
      filter(store_id == input$store_id) %>%
      distinct(upc_id) %>%
      pull(upc_id) %>%
      sort()

    # Update the upc_id selectInput choices
    updateSelectInput(
      session, "upc_id",
      choices = filtered_upc_ids
    )
  })

  # Filter dataset based on input
  ts <- function(data) {
    reactive({
      req(input$upc_id, input$store_id)
      data |>
        filter(
          upc_id == as.numeric(input$upc_id),
          store_id == as.numeric(input$store_id)
        )
    })
  }
  rtransaction <- ts(transaction)
  rfbl_arima <- ts(fbl_arima)
  rfbl_stl <- ts(fbl_stl)
  rres <- ts(residuals)

  # Render Products and Store data tables
  output$product <- DT::renderDataTable(product)
  output$store <- DT::renderDataTable(store)

  # Render Forecast plot
  output$forecast <- plotly::renderPlotly({
    v_feature <- as.numeric(input$feature)
    v_display <- as.numeric(input$display)
    v_tpr_only <- as.numeric(input$tpr_only)

    # Check if input is a valid combination
    if (v_tpr_only == 1 && (v_display == 1 || v_feature == 1)) {
      stop("Not valid combination.")
    }

    # Filter fable for input
    f_rfbl_arima <- rfbl_arima() |>
      filter(
        feature == v_feature,
        display == v_display,
        tpr_only == v_tpr_only
      )

    p <- rtransaction() |>
      ggplot() +
      # Side feature plot
      geom_line(aes(week, !!sym(input$column)), color = "gray") +
      # Historic data plot
      geom_line(aes(week, units)) +
      # ARIMA forecast plot
      geom_line(aes(week, units, color = model), data = f_rfbl_arima) +
      # CI 95% and 80%
      geom_ribbon(
        aes(week, ymin = low95, ymax = up95, fill = model),
        alpha = 0.3, show.legend = FALSE,
        data = f_rfbl_arima
      ) +
      geom_ribbon(
        aes(week, ymin = low80, ymax = up80, fill = model),
        alpha = 0.3, show.legend = FALSE,
        data = f_rfbl_arima
      ) +
      # STL forecast plot
      geom_line(aes(week, units, color = model), data = rfbl_stl()) +
      # CI 95% and 80%
      geom_ribbon(
        aes(week, ymin = low95, ymax = up95, fill = model),
        alpha = 0.3, show.legend = FALSE,
        data = rfbl_stl()
      ) +
      geom_ribbon(
        aes(week, ymin = low80, ymax = up80, fill = model),
        alpha = 0.3, show.legend = FALSE,
        data = rfbl_stl()
      ) +
      # Scale, labs and theme
      tsibble::scale_x_yearweek(breaks = "1 month") +
      labs(color = "Forecast Model:", x = "Week", y = "Units sold") +
      theme(axis.text.x = element_text(angle = 45))
    plt <- plotly::ggplotly(p)

    # Clean model names in legend
    for (i in seq_len(length(plt$x$data))) {
      value <- plt$x$data[[i]]$name
      if (!is.null(value)) {
        plt$x$data[[i]]$name <- stringr::str_match(value, "[A-Z]+")[[1]]
      }
    }
    plt
  })

  # Residuals autocorrelation plot
  output$res_acf <- plotly::renderPlotly({
    plt <- rres() |>
      feasts::ACF(.innov, lag_max = 55) |>
      autoplot() +
      scale_x_continuous(breaks = c(13, 26, 39, 52)) +
      labs(x = "Lag [weeks]", y = "ACF")
    plotly::ggplotly(plt)
  })

  # Residuals distribution plot
  output$res_dist <- plotly::renderPlotly({
    plt <- rres() |>
      ggplot() +
      geom_histogram(aes(x = .innov, fill = .model), bins = 50) +
      facet_wrap(~.model, scales = "free") +
      scale_x_continuous(breaks = scales::extended_breaks(10)) +
      labs(x = "Residuals", y = "Count") +
      theme(legend.position = "none")

    plotly::ggplotly(plt)
  })
}
