#' Sample TS for debugging
#'
#' @param ts A `tsibble` containing the time series.
#' @param n Number of samples to select.
sample_for_debugging <- function(ts, n = 10) {
  ts_keys <- ts |>
    dplyr::distinct(.data$store_id, .data$upc_id) |>
    dplyr::sample_n(n)
  return(
    ts |>
      dplyr::semi_join(ts_keys, by = c("store_id", "upc_id"))
  )
}

#' Decorate function to add a message after saving data
#'
#' @param data Data frame to be written to parquet.
#' @param path Path
write_to_parquet <- function(data, path) {
  # Write data to parquet file
  arrow::write_parquet(data, path)
  message("Data saved to: ", path)
}

#' Data Loading and Preparation
#'
#' If raw data path is provided, apply the processing steps. Otherwise, use the default datasets.
#' Save them to the output directory.
#'
#' @param raw_data_path Optional path to the raw data file location.
#' @param data_path Directory where the output files will be saved.
#' @return A list containing the processed data frames: store, product, and transaction.
#' @export
data_loading <- function(raw_data_path, data_path) {
  if (!is.null(raw_data_path)) {
    # Check if input_path exists
    if (!file.exists(raw_data_path)) {
      stop("The specified input_path does not exist: ", raw_data_path)
    }
    message("\nLoading data from ", raw_data_path)
    data <- load_raw_data(raw_data_path) |> process_data()
  } else {
    message("\nNo raw data path provided. Using default datasets.")

    # Load from internal data without polluting global env
    data_env <- new.env()
    utils::data("store", package = "salesforecastr", envir = data_env)
    utils::data("product", package = "salesforecastr", envir = data_env)
    utils::data("transaction", package = "salesforecastr", envir = data_env)

    data <- list(
      store = data_env$store,
      product = data_env$product,
      transaction = data_env$transaction
    )
  }

  message("\nData loaded successfully.")
  write_to_parquet(data$store, file.path(data_path, "store.parquet"))
  write_to_parquet(data$product, file.path(data_path, "product.parquet"))
  write_to_parquet(data$transaction, file.path(data_path, "transaction.parquet"))

  data
}

#' Render Quarto Report
#'
#' Render the Quarto report and save the output to the output directory.
#'
#' @param report Name of the report to render (without .qmd extension).
#' @param data_path Path to the data directory.
#' @param output_path Path to the output directory where the report will be saved.
render_quarto_report <- function(report, data_path, output_path) {
  # Data analysis report
  quarto::quarto_render(
    system.file("reports", glue::glue("{report}.qmd"), package = "salesforecastr"),
    output_format = "all",
    execute_params = list(
      path = data_path
    )
  )
  # Now move the output to the output path and remove the original output
  files <- list.files(
    system.file("reports", package = "salesforecastr"),
    full.names = TRUE, recursive = TRUE, pattern = report
  )
  dirs <- list.dirs(
    system.file("reports", package = "salesforecastr"),
    full.names = TRUE, recursive = FALSE
  )
  # Copy the entire reports directory to the specified reports_path
  file.copy(from = files, to = output_path, recursive = TRUE, overwrite = TRUE)
  file.copy(from = dirs, to = output_path, recursive = TRUE, overwrite = TRUE)

  # Remove all files and folders in reports except *.qmd files
  unlink(c(dirs, files[!grepl(".qmd", files)]), recursive = TRUE, force = TRUE)

  message("Report rendered and saved to: ", output_path)
}

#' Forecasting
#'
#' @description
#' Models:
#' * **ARIMA**:
#'   * `arima_def = fable::ARIMA(sqrt(units))`
#'   * `arima_lagged = fable::ARIMA(sqrt(units) ~ feature + display + tpr_only +
#'      dplyr::lag(feature) + dplyr::lag(display) + dplyr::lag(tpr_only))`
#'  (Lagged ARIMA model is preferred over the default ARIMA model (if it isn't null))
#'
#' * **STL + ETS model**:
#' After interpolating missing values using the selected ARIMA model,
#' an STL decomposition followed by ETS modeling is performed.
#'
#' @param steps Number of steps to forecast ahead. Defaults to 2.
#' @param raw_data_path Optional path to the raw data file location.
#' @param output_dir Directory where the output files will be saved. Defaults to the current working directory.
#' @param batch_size The size of the batch for forecasting. Defaults to 8.
#' @seealso [`fable::ARIMA`] [`feasts::STL`] [`fable::ETS`]
#' @export
dh_forecasting <- function(
    steps = 2,
    raw_data_path = NULL,
    output_dir = getwd(),
    batch_size = 8) {
  # Setup paths
  data_path <- file.path(output_dir, "data")
  model_path <- file.path(output_dir, "model")
  forecast_path <- file.path(output_dir, "forecast")
  mbl_path <- file.path(model_path, "mbl.rds")
  residuals_path <- file.path(forecast_path, "residuals.parquet")
  fbl_stl_path <- file.path(forecast_path, "fbl_stl.parquet")
  fbl_arima_path <- file.path(forecast_path, "fbl_arima.parquet")

  dir.create(data_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(model_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(forecast_path, recursive = TRUE, showWarnings = FALSE)

  # Data loading
  data <- data_loading(raw_data_path, data_path)

  # Data processing
  message("\nData Processing")
  tsb <- filter_ts(data$transaction) |> tsibble::fill_gaps()

  # Training
  message("\nTraining")
  message("\n- ARIMA")
  mbl_arima <- train_model(
    tsb,
    mods = list(
      arima_def = models$arima_def,
      arima_lagged = models$arima_lagged
    )
  ) |>
    dplyr::mutate(arima = dplyr::if_else(
      fabletools::is_null_model(.data$arima_lagged),
      .data$arima_def,
      .data$arima_lagged
    )) |>
    dplyr::select(
      -.data$arima_def,
      -.data$arima_lagged
    ) |>
    dplyr::filter(!fabletools::is_null_model(.data$arima))

  message("\n- Interpolating and training STL+ETS model.")
  mbl_stl <- mbl_arima |>
    generics::interpolate(
      dplyr::semi_join(tsb, mbl_arima, by = c("store_id", "upc_id"))
    ) |>
    train_model(mods = list(stl = models$stl))
  mbl <- dplyr::bind_cols((mbl_arima), mbl_stl["stl"])
  saveRDS(mbl, mbl_path)
  message("\nFitted model saved to: ", mbl_path)

  # Compute residuals for evaluation
  mbl |>
    generics::augment() |>
    write_to_parquet(residuals_path)

  # Forecast STL decomposition model with ETS errors
  message("\nForecasting STL+ETS model")
  get_forecast(
    mbl |> dplyr::select(-.data$arima),
    tsibble::new_data(tsb, n = steps),
    batch_size
  ) |>
    add_ci("units") |>
    dplyr::select(-.data$.model, -.data$units) |>
    dplyr::rename(units = .data$.mean) |>
    write_to_parquet(fbl_stl_path)

  # Forecast ARIMA model with regressors
  message("\nForecasting ARIMA model")
  # Combinations of predictors in ARIMA model
  combinations <- list(
    list(feature = 0, display = 0, tpr_only = 0),
    list(feature = 1, display = 0, tpr_only = 0),
    list(feature = 0, display = 1, tpr_only = 0),
    list(feature = 0, display = 0, tpr_only = 1),
    list(feature = 1, display = 1, tpr_only = 0)
  )

  # Batching and forecast
  fbl_arima <- purrr::map_dfr(
    seq_along(combinations), function(i) {
      message(
        "\n\nFeature = ", combinations[[i]]$feature,
        "| Display = ", combinations[[i]]$display,
        "| TPR = ", combinations[[i]]$tpr_only,
        "\n"
      )
      ds <- tsibble::new_data(tsb, n = steps) |>
        dplyr::mutate(!!!combinations[[i]])
      fbl <- get_forecast(
        dplyr::select(mbl, -.data$stl),
        ds,
        batch_size
      ) |>
        dplyr::mutate(fbl_ind = i)
      # Save checkpoint just in case
      # saveRDS(fbl, glue::glue("output/model/fbl_arima_{i}.rds"))

      # Return as tibble to avoid problems binding later
      tibble::as_tibble(fbl)
    }
  )

  fbl_arima |>
    tsibble::as_tsibble(
      index = .data$week,
      key = c(
        .data$store_id,
        .data$upc_id,
        .data$.model,
        .data$fbl_ind
      )
    ) |>
    add_ci("units") |>
    dplyr::select(-.data$.model, -.data$units) |>
    dplyr::rename(units = .data$.mean) |>
    write_to_parquet(fbl_arima_path)
}

#' Validation Pipeline
#'
#' @description
#' This function performs the following steps:
#' * Generates a data analysis report in Quarto.
#' * Samples time series for validation:
#'     * One time series sample randomly for each product category and store.
#'     * Less than 25% missing values.
#' * Null imputation with `fable::ARIMA(sqrt(units))` and regressors set to 0.
#' * Splits the dataset into training and testing sets based on `test_size`.
#' * Trains the following models:
#'     * `arima_def`: ARIMA model on `sqrt(units)` with default settings.
#'     * `arima_base`: ARIMA model on `sqrt(units)` with predictors.
#'     * `arima_lagged`: ARIMA model on `sqrt(units)` with predictors and their lagged values.
#'     * `arima_seasonal`: ARIMA model with seasonal terms using Fourier series (`K = 6`) and predictors.
#'     * `arima_seasonal_lagged`: ARIMA model with seasonal terms, predictors, and their lagged values.
#'     * `stl`: Seasonal decomposition model using STL and ETS on seasonally adjusted data.
#'     * `nnetar`: Neural network autoregression model with predictors.
#'     * `prophet`: Prophet model with predictors.
#' * Selects the best ARIMA model based on AICc.
#' * Calculates RMSE for the selected ARIMA model and STL+ETS model.
#' * Renders a validation report.
#'
#' All models forecast the square root of the `units` variable to help stabilize variance.
#' Predictor variables are `feature`, `display`, and `tpr_only`.
#'
#' @details
#' Needs write permissions in the installed package location to render Quarto reports.
#'
#' @param raw_data_path Optional path to the raw data file location.
#' @param output_dir Directory where the output files will be saved. Defaults to the current working directory.
#' @param test_size Proportion of the dataset to be used for testing.
#' @param batch_size The size of the batch for forecasting. Defaults to 8.
#' @seealso [`fable::ARIMA`] [`fable::NNETAR`] [`fable.prophet::prophet`] [`feasts::STL`] [`fable::ETS`]
#'
#' @export
dh_validation <- function(
    raw_data_path = NULL,
    output_dir = getwd(),
    test_size = 0.2,
    batch_size = 8) {
  # Set up paths
  data_path <- file.path(output_dir, "data")
  validation_path <- file.path(output_dir, "validation")
  reports_path <- file.path(output_dir, "reports")
  mbl_path <- file.path(validation_path, "mbl.rds")
  results_path <- file.path(validation_path, "results.csv")

  dir.create(validation_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(data_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(reports_path, recursive = TRUE, showWarnings = FALSE)

  # Data loading
  data <- data_loading(raw_data_path, data_path)

  # Data Analysis report
  render_quarto_report(
    report = "data_analysis",
    data_path = normalizePath(data_path),
    output_path = reports_path
  )

  # Data processing
  message("\n\nData Processing")
  tsb <- missing_transaction(data$transaction) |>
    sample_ts(data$product) |>
    impute_ts()
  index <- tsibble::index_var(tsb)
  keys <- tsibble::key_vars(tsb)

  # Split data
  splitted_data <- split_dataset(tsb, test_size)
  train <- splitted_data$train
  test <- splitted_data$test

  # Validation
  message("\n\nStarting validation")
  # Training
  message("\nTraining")
  mbl <- train_model(train = train, mods = models)
  saveRDS(mbl, mbl_path)
  message("\nFitted model saved to: ", mbl_path)

  # Calculate AICc for arima models
  message("\nEvaluating ARIMA models for AICc")
  aicc <- mbl |>
    dplyr::select(tidyselect::all_of(keys), tidyselect::matches("arima")) |>
    generics::glance() |>
    dplyr::select(tidyselect::all_of(keys), ".model", "AICc") |>
    tidyr::pivot_wider(names_from = ".model", values_from = "AICc")

  # Calculate RMSE for selected ARIMA model and STL+ETS model
  message("\nEvaluating RMSE in test set on selected ARIMA and STL+ETS model")
  rmse <- dplyr::select(mbl, .data$arima_lagged, .data$stl) |>
    get_forecast(test, batch_size) |>
    calculate_rmse(test, "units") |>
    tidyr::pivot_wider(
      names_from = ".model",
      values_from = "RMSE"
    )

  results <- dplyr::bind_rows(aicc = aicc, rmse = rmse, .id = "metric")
  arrow::write_csv_arrow(results, results_path)
  message("\nResults saved to: ", results_path)

  # Render validation report
  render_quarto_report(
    report = "validation",
    data_path = normalizePath(validation_path),
    output_path = reports_path
  )

  message("\nValidation process completed successfully.")
}
