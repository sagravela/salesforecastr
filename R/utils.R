# Make `.data` global
utils::globalVariables(".data")

#' Decorate function to add a message after saving data
#'
#' @param data Data frame to be written to parquet.
#' @param path Path
write_to_parquet <- function(data, path) {
  # Write data to parquet file
  arrow::write_parquet(data, path)
  message("Data saved to: ", path)
}

#' Load raw data
#'
#' Load raw data from Excel and clean their column names.
#' It has to be in the same format as the original data.
#'
#' @param raw_data_path A `character` with the path to the raw data Excel file.
#' @returns A `list` of dataframes for store, product and transaction datasets.
#' @export
load_raw_data <- function(raw_data_path) {
  store <- readxl::read_excel(
    raw_data_path,
    sheet = "dh Store Lookup", skip = 1
  ) |>
    janitor::clean_names()

  product <- readxl::read_excel(
    raw_data_path,
    sheet = "dh Products Lookup", skip = 1
  ) |>
    janitor::clean_names() |>
    dplyr::rename(upc_id = "upc")

  transaction <- readxl::read_excel(
    raw_data_path,
    sheet = "dh Transaction Data", skip = 1
  ) |>
    janitor::clean_names() |>
    dplyr::rename(upc_id = "upc", store_id = "store_num")
  list(store = store, product = product, transaction = transaction)
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

#' Convert product size to volume (ml) and mass (oz)
#'
#' @param x A `character` containing the product size.
#' @returns A `number` of the converted product size.
converter <- function(x) {
  split_values <- unlist(strsplit(x, " "))
  unit <- tolower(split_values[2])
  result <- measurements::conv_unit(
    as.numeric(split_values[1]),
    ifelse(unit == "lt", "l",
      ifelse(unit == "ct", "carat", unit)
    ),
    to = ifelse(unit %in% c("lt", "ml"), "ml", "oz")
  )
  return(result)
}

#' Parse tibble to tsibble with defined index and keys
#'
#' @param tbl A `tibble`
#' @returns A `tsibble`
to_tsibble <- function(tbl) {
  return(
    tsibble::as_tsibble(
      tbl,
      index = .data$week,
      key = c("store_id", "upc_id")
    )
  )
}

#' Process data
#'
#' Prepare data to comsumption. Steps for each dataset:
#' * Store -> Unify repeated store with distinct `seg_value_name`.
#' * Product -> Convert product size to volume (ml) and mass (oz).
#' * Transaction -> Parse to `tsibble` format.
#'
#' @param data A `list` of dataframes for store, product and transaction.
#' @returns A `list` of processed dataframes.
#' @export
process_data <- function(data) {
  message("\nProcessing data...")
  # Process store data
  # Handle repeated store
  repeated_store <- data$store |>
    dplyr::group_by(.data$store_id) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(.data$n > 1) |>
    dplyr::left_join(data$store, by = "store_id")

  # Set the `seg_value_name` as both "MAINSTREAM" and "UPSCALE"
  data$store <- data$store |>
    dplyr::mutate(
      seg_value_name = ifelse(
        .data$store_id %in% repeated_store$store_id,
        "MAINSTREAM_UPSCALE",
        .data$seg_value_name
      )
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(store_id = as.character(.data$store_id))

  # Process product data
  data$product <- data$product |>
    dplyr::mutate(
      upc_id = as.character(.data$upc_id),
      volume_ml = ifelse(
        grepl("LT|ML", .data$product_size),
        sapply(.data$product_size, converter), NA
      ),
      mass_oz = ifelse(
        grepl("OZ|CT", .data$product_size),
        sapply(.data$product_size, converter), NA
      )
    )

  # Process transaction data
  data$transaction <- data$transaction |>
    dplyr::mutate(
      week = tsibble::yearweek(.data$week_end_date),
      upc_id = as.character(.data$upc_id),
      store_id = as.character(.data$store_id)
    ) |>
    dplyr::relocate(.data$week) |>
    to_tsibble()

  message("Data processed successfully.")
  return(data)
}

#' Missing transaction
#'
#' Aggregate missing values accross key groups.
#'
#' @param tsb A `tsibble` containing the transaction data.
#' @returns A `tsibble` with the number of missing transaction by store and product.
missing_transaction <- function(tsb) {
  # Add a column with the number of missing values.
  return(
    tsb |>
      tsibble::fill_gaps() |>
      tibble::as_tibble() |>
      dplyr::group_by(.data$store_id, .data$upc_id) |>
      dplyr::mutate(n_na = sum(is.na(units))) |>
      dplyr::ungroup() |>
      tsibble::as_tsibble(
        index = .data$week,
        key = c(.data$store_id, .data$upc_id)
      )
  )
}

#' Sample TS
#'
#' In order to validate the model, I will select at least one TS sample randomly for each product category and store.
#' Also, because many TS are incomplete, I will use only those with less than 25% of missing values.
#'
#' @param transaction A `tsibble` with the number of missing transaction by store and product.
#' @param product A `tibble` containing the product data.
#' @returns A `tsibble` with the sampled TS.
sample_ts <- function(transaction, product) {
  # Set seed for reproducibility
  set.seed(42)
  ts_samples <- transaction |>
    tibble::as_tibble() |>
    dplyr::left_join(product[c("upc_id", "category")], by = "upc_id") |>
    dplyr::group_by(.data$store_id, .data$upc_id) |>
    dplyr::mutate(missingness = .data$n_na / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$store_id, .data$category) |>
    dplyr::filter(.data$missingness < 0.25) |>
    dplyr::distinct(
      .data$upc_id,
      .data$store_id,
      .data$category,
      .data$missingness
    ) |>
    dplyr::slice_sample(n = 1) |>
    dplyr::ungroup()

  sampled_transaction <- ts_samples |>
    dplyr::inner_join(
      transaction,
      by = c("store_id", "upc_id"),
      suffix = c("", "")
    ) |>
    to_tsibble()
  message("TS sampled successfully.")
  return(sampled_transaction)
}

#' Imputation
#'
#' Nulls imputation with ARIMA because some models require non-nulls.
#' Imputation of feature, display and tpr_only with 0.
#'
#' @param sampled_transaction A `tsibble` with sampled time series.
#' @returns A `tsibble` with the imputed time series.
impute_ts <- function(sampled_transaction) {
  transaction_na <- sampled_transaction |>
    dplyr::filter(is.na(.data$units)) |>
    tibble::as_tibble() |>
    dplyr::distinct(.data$upc_id, .data$store_id, .data$n_na) |>
    dplyr::left_join(sampled_transaction, by = c("store_id", "upc_id")) |>
    to_tsibble()

  # Trasform units to square root to avoid negative values.
  # I don't use regressors because in time gaps they aren't known.
  message("Imputing with ARIMA...")
  model_imp <- progressr::with_progress(
    fabletools::model(transaction_na, imp = fable::ARIMA(sqrt(units))),
    enable = TRUE
  )

  # I will impute with the model in those TS if the model is not null.
  # Otherwise, I will impute them with the last value.
  sample_transaction_imp <- model_imp |>
    dplyr::filter(!fabletools::is_null_model(.data$imp)) |>
    generics::interpolate(transaction_na)
  message("Nulls imputed successfully.")
  return(
    sampled_transaction |>
      dplyr::left_join(sample_transaction_imp, by = c("week", "store_id", "upc_id")) |>
      dplyr::mutate(units = dplyr::if_else(
        is.na(.data$units.x),
        .data$units.y,
        .data$units.x
      )) |>
      dplyr::select(-.data$units.x, -.data$units.y, -.data$n_na) |>
      tsibble::group_by_key() |>
      tidyr::fill(units, .direction = "down") |>
      dplyr::relocate(.data$week, .before = 1) |>
      dplyr::relocate(units, .after = .data$store_id) |>
      dplyr::ungroup()
  )
}

#' Filter TS
#'
#' Conserve those TS with more than 12 observations (3 months at least).
#'
#' @param transaction A `tsibble` containing the transaction data.
#' @returns A `tsibble` with the filtered time series.
filter_ts <- function(transaction) {
  message("Filtering TS...")
  return(
    transaction |>
      tsibble::group_by_key() |>
      dplyr::filter(dplyr::n() > 12) |>
      dplyr::ungroup()
  )
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
  file.copy(from = files[!grepl(".qmd", files)], to = output_path, recursive = TRUE, overwrite = TRUE)
  file.copy(from = dirs, to = output_path, recursive = TRUE, overwrite = TRUE)

  # Remove all files and folders in reports except *.qmd files
  unlink(c(dirs, files[!grepl(".qmd", files)]), recursive = TRUE, force = TRUE)

  message("Report rendered and saved to: ", output_path)
}
