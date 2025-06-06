#' Model Validation Pipeline
#'
#' This function validates the models by training them on the provided data,
#' calculating AICc values, and forecasting to evaluate RMSE.
#'
#' @param models A list of models to be trained and validated.
#' @param raw_data_path Optional path to the raw data file. If provided, the function will load and process this data.
#' @param output_dir Directory where the output files will be saved. Defaults to the current working directory.
#' @param batch_size The size of the batch for forecasting. Defaults to 8.
#' @return NULL
#' @export
model_validation <- function(
    models = models,
    raw_data_path = NULL,
    output_dir = getwd(),
    batch_size = 8) {
  # Data Loading and preparation
  # If path is provided, apply the processing steps. Otherwise, use the default datasets.
  if (raw_data_path) {
    message("\n\nLoading data from ", raw_data_path)
    data <- load_raw_data(raw_data_path) |> clean_data()
  } else {
    message("\n\nLoading the default data.")
    data <- list(store = store, product = product, transaction = transaction)
  }
  message("\n\nData loaded succesfully.")

  # Saving
  data_path <- file.path(output_dir, "output", "data")
  dir.create(data_path, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(data$store, file.path(data_path, "store.parquet"))
  arrow::write_parquet(data$product, file.path(data_path, "product.parquet"))
  arrow::write_parquet(data$transaction, file.path(data_path, "transaction.parquet"))
  message("\nCleaned data saved to: ", data_path)

  # Data processing
  message("\n\nProcessing data...")
  sampled_data <- missing_transaction(data$transaction) |>
    sample_ts(data$product) |>
    impute_ts() |>
    split_sample_dataset()

  # Save the data
  arrow::write_parquet(sampled_data$sample_train, file.path(output_path, "sample_train.parquet"))
  arrow::write_parquet(sampled_data$sample_val, file.path(output_path, "sample_val.parquet"))
  message("\nProcessed data saved to: ", data_path)

  # Modelling
  # Setup path
  val_path <- file.path(output_dir, "output", "validation")
  dir.create(val_path, recursive = TRUE, showWarnings = FALSE)

  # Training
  message("\n\nTraining models...")
  mbl_path <- file.path(validation_path, "mbl.rds")
  mbl <- train_model(train = data$train, models = models)
  saveRDS(mbl, mbl_path)
  message("\nModels trained and saved to: ", mbl_path)

  # Calculate AICc values for each model
  aicc_path <- file.path(validation_path, "aicc.csv")
  aicc <- mbl |>
    dplyr::select(contains("arima")) |>
    generics::glance() |>
    dplyr::select(upc_id, store_id, .model, AICc) |>
    tidyr::pivot_wider(
      names_from = .model,
      values_from = AICc
    )
  arrow::write_csv_arrow(aicc_path)
  message("\nAICc values saved to: ", aicc_path)

  # The best model among arima variations is the lagged version of ARIMA.
  # Prophet model has more than 60% of null models so I will remove it from the mable.
  # Because NNETAR is very slow at predicting, I will remove it from the mable.
  # Also, is not performing well in the validation set due to lack of historic data.
  mbl <- dplyr::select(mbl, -arima_base, -arima_seasonal, -arima_seasonal_lagged, -prophet, -nnetar)

  # Forecast and evaluate the models with RMSE
  message("\nForecasting...")
  rmse_path <- file.path(validation_path, "rmse.csv")
  rmse <- get_forecast(mbl, data$test, batch_size) |>
    tibble::as_tibble() |>
    dplyr::left_join(
      data$test,
      by = c("week", "store_id", "upc_id"),
      suffix = c("", "")
    ) |>
    dplyr::group_by(store_id, upc_id, .model) |>
    dplyr::summarise(
      RMSE = sqrt(mean((units - .mean)^2)),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = .model,
      values_from = RMSE
    )
  arrow::write_csv_arrow(rmse, rmse_path)
  message("\nRMSE values saved to: ", rmse_path)
  message("\nValidation process completed successfully.")
}
