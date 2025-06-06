#' @export
forecast_train_dataset <- function(
    models,
    raw_data_path = NULL,
    output_dir = getwd(),
    batch_size = 8) {
  # Data Loading and preparation
  # If path is provided, apply the processing steps. Otherwise, use the default datasets.
  if (raw_data_path) {
    message("\nLoading data from ", raw_data_path)
    data <- load_raw_data(raw_data_path) |> clean_data()
  } else {
    message("\nLoading the default data.")
    data <- list(store = store, product = product, transaction = transaction)
  }
  message("\nData loaded succesfully.")

  # Saving
  data_path <- file.path(output_dir, "output", "data")
  dir.create(data_path, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(data$store, file.path(data_path, "store.parquet"))
  arrow::write_parquet(data$product, file.path(data_path, "product.parquet"))
  arrow::write_parquet(data$transaction, file.path(data_path, "transaction.parquet"))
  message("Cleaned data saved to: ", data_path)

  # Data processing
  message("\nProcessing data...")
  train_data <- filter_ts(data$transaction) |>
    split_dataset()
  full_data <- dplyr::bind_rows(train_data$train, train_data$test)

  # Save the data
  arrow::write_parquet(train_data$train, file.path(data_path, "train.parquet"))
  arrow::write_parquet(train_data$test, file.path(data_path, "test.parquet"))
  arrow::write_parquet(full_data, file.path(data_path, "full.parquet"))
  message("\nProcessed data saved to: ", data_path)

  # Modelling
  # Sample TS for debugging
  ts_keys <- train_data$train |>
    dplyr::distinct(store_id, upc_id) |>
    dplyr::sample_n(10)
  train_data$train <- train_data$train |>
    dplyr::semi_join(ts_keys, by = c("store_id", "upc_id"))

  # Setup path
  model_path <- file.path(output_dir, "output", "model")
  dir.create(model_path, recursive = TRUE, showWarnings = FALSE)

  # Train in train set
  message("\nTraining in train set...")
  mbl_train_path <- file.path(model_path, "mbl_train.rds")
  mbl <- train_arima_stl(train_data$train)
  saveRDS(mbl, mbl_train_path)
  message("\nFitted model in train dataset saved to: ", mbl_train_path)

  # Forecast
  message("\nForecasting in Train Dataset...")
  fbl_train_path <- file.path(model_path, "fbl_train.parquet")
  get_forecast(mbl, train_data$test, batch_size) |>
    add_ci() |>
    arrow::write_parquet(fbl_train_path)
  message("\nSuccessful. Forecasts saved to: ", fbl_train_path)
}


#' @export
forecast_full_dataset <- function(
    models,
    raw_data_path = NULL,
    output_dir = getwd(),
    batch_size = 8) {
  # Train in full dataset
  message("\nTraining in full dataset...")
  mbl_full_path <- file.path(model_path, "mbl_full.rds")
  mbl <- train_arima_stl(full_data)
  saveRDS(mbl, mbl_full_path)
  message("\nFitted model in full dataset saved to: ", mbl_full_path)

  # Compute residuals for evaluation
  residuals_path <- file.path(model_path, "full_residuals.parquet")
  mbl |>
    generics::augment() |>
    arrow::write_parquet(residuals_path)
  message("\nResiduals for full dataset saved to: ", residuals_path)

  # Forecast STL decomposition model with ETS errors
  message("\nForecasting STL+ETS model...")
  fbl_full_stl_path <- file.path(model_path, "fbl_full_stl.parquet")
  get_forecast(
    fit |> dplyr::select(-arima),
    tsibble::new_data(full_data, n = 2),
    batch_size
  ) |>
    add_ci() |>
    arrow::write_parquet(fbl_full_stl_path)
  message("\nSuccessful. Forecasts saved to: ", fbl_full_stl_path)

  # Forecast ARIMA model with regressors
  message("\nForecasting ARIMA model...")
  fbl_full_arima_path <- file.path(model_path, "fbl_full_arima.parquet")
  forecast_arima(fit, full_data, batch_size) |> arrow::write_parquet(fc_arima_path)
  message("\nSuccessful. Forecasts saved to: ", fc_arima_path)
}
