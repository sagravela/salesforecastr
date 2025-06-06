#' Data loading
#' Load raw data from Excel and clean their column names
#' @returns A `list` of dataframes.
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
  return(list(store = store, product = product, transaction = transaction))
}

#' Convert product size to volume (ml) and mass (oz)
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

#' Clean and save data
#' @param raw_data_path A `character` with the path to the raw data Excel file.
#' @returns Cleaned data.
#' @export
clean_data <- function(data) {
  message("\nCleaning data...")
  # Process store data
  # Handle repeated store
  repeated_store <- data$store |>
    dplyr::group_by(store_id) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(n > 1) |>
    dplyr::left_join(data$store, by = dplyr::join_by(store_id))

  # Set the `seg_value_name` as both "MAINSTREAM" and "UPSCALE"
  data$store <- data$store |>
    dplyr::mutate(
      seg_value_name = ifelse(
        store_id %in% repeated_store$store_id,
        "MAINSTREAM_UPSCALE",
        seg_value_name
      )
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(store_id = as.character(store_id))

  # Process product data
  data$product <- data$product |>
    dplyr::mutate(
      upc_id = as.character(upc_id),
      volume_ml = ifelse(
        grepl("LT|ML", product_size),
        sapply(product_size, converter), NA
      ),
      mass_oz = ifelse(
        grepl("OZ|CT", product_size),
        sapply(product_size, converter), NA
      )
    )

  # Process transaction data
  data$transaction <- data$transaction |>
    dplyr::mutate(
      week = tsibble::yearweek(week_end_date),
      upc_id = as.character(upc_id),
      store_id = as.character(store_id)
    ) |>
    dplyr::relocate(week) |>
    tsibble::as_tsibble(index = week, key = c(store_id, upc_id))

  message("Data cleaned successfully.")
  return(data)
}


#' Missing transaction
#'
#' Aggregate missing values accross key groups.
#' @param transaction A `tsibble` containing the transaction data.
#' @returns A `tsibble` with the number of missing transaction by store and product.
missing_transaction <- function(transaction) {
  # Add a column with the number of missing values.
  return(
    transaction |>
      tsibble::fill_gaps() |>
      tibble::as_tibble() |>
      dplyr::group_by(store_id, upc_id) |>
      dplyr::mutate(n_na = sum(is.na(units))) |>
      dplyr::ungroup() |>
      tsibble::as_tsibble(index = week, key = c(store_id, upc_id))
  )
}

#' Sample TS
#'
#' In order to validate the model, I will select at least one TS sample randomly for each product category and store.
#' Also, because many TS are incomplete, I will select those with less 50 nulls (almost one year of data).
#' If any TS has nulls, I will imput them with an ARIMA model. That's because some models require non-nulls.
#' @param transaction A `tsibble` with the number of missing transaction by store and product.
#' @param product A `tibble` containing the product data.
#' @returns A `tsibble` with the sampled TS.
sample_ts <- function(transaction, product) {
  # Set seed for reproducibility
  set.seed(42)
  ts_samples <- transaction |>
    tibble::as_tibble() |>
    dplyr::left_join(product[c("upc_id", "category")], by = "upc_id") |>
    dplyr::group_by(store_id, upc_id) |>
    dplyr::mutate(missingness = n_na / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(store_id, category) |>
    dplyr::filter(missingness < 0.25) |>
    dplyr::distinct(upc_id, store_id, category, missingness) |>
    dplyr::slice_sample(n = 1) |>
    dplyr::ungroup()

  sampled_transaction <- ts_samples |>
    dplyr::inner_join(
      transaction,
      by = c("store_id", "upc_id"),
      suffix = c("", "")
    ) |>
    tsibble::as_tsibble(index = week, key = c(store_id, upc_id))
  message("TS sampled successfully.")
  return(sampled_transaction)
}

#' Imputation
#'
#' @description
#' Nulls imputation with ARIMA because some models require non-nulls.
#' Also, I impute feature, display and tpr_only with 0.
#' @param sampled_transaction A `tsibble` with sampled time series.
#' @returns A `tsibble` with the imputed time series.
impute_ts <- function(sampled_transaction) {
  transaction_na <- sampled_transaction |>
    dplyr::filter(is.na(units)) |>
    tibble::as_tibble() |>
    dplyr::distinct(upc_id, store_id, n_na) |>
    dplyr::left_join(sampled_transaction, by = c("store_id", "upc_id")) |>
    tsibble::as_tsibble(index = week, key = c(store_id, upc_id))

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
    dplyr::filter(!fabletools::is_null_model(imp)) |>
    generics::interpolate(transaction_na)
  message("Nulls imputed successfully.")
  return(
    sampled_transaction |>
      dplyr::left_join(sample_transaction_imp, by = c("week", "store_id", "upc_id")) |>
      dplyr::mutate(units = dplyr::if_else(is.na(units.x), units.y, units.x)) |>
      dplyr::select(-units.x, -units.y, -n_na) |>
      tsibble::group_by_key() |>
      tidyr::fill(units, .direction = "down") |>
      dplyr::relocate(week, .before = 1) |>
      dplyr::relocate(units, , .after = store_id)
  )
}

#' Split sampled dataset
#'
#' Because crossvalidation is time consuming, split them into train
#' and validation sets. Last month is used for validation.
#' @param sampled_transaction A `tsibble` with sampled time series.
#' @returns A `list` with sampled train and validation sets.
split_sample_dataset <- function(sampled_transaction) {
  sampled_train <- sampled_transaction |>
    dplyr::group_by(store_id, upc_id) |>
    dplyr::filter(week <= tsibble::yearweek(max(as.Date(week)) - months(1))) |>
    dplyr::ungroup()

  sampled_val <- sampled_transaction |>
    dplyr::group_by(store_id, upc_id) |>
    dplyr::filter(week > tsibble::yearweek(max(as.Date(week)) - months(1))) |>
    dplyr::ungroup()
  message("Sampled TS splitted successfully.")
  return(list(sample_train = sampled_train, sample_val = sampled_val))
}

#' Filter TS
#'
#' @description
#' Conserve those TS with more than 12 observations (3 months at least). Also,
#' fill implicit gaps with explicit ones.
#' @param transaction A `tsibble` containing the transaction data.
#' @returns A `tsibble` with the filtered time series.
filter_ts <- function(transaction) {
  message("Filtering TS...")
  return(
    transaction |>
      tsibble::group_by_key() |>
      dplyr::filter(n() > 12) |>
      dplyr::ungroup() |>
      tsibble::fill_gaps() # convert implicit gaps as explicit
  )
}

#' Split full dataset
#'
#' Split the dataset into train and test sets.
#' @param transaction A `tsibble` containing the transaction data.
#' @returns A `list` with train and test sets.
#' @export
split_dataset <- function(transaction) {
  # To test the dataset, I will use the last month of available data.
  train <- transaction |>
    tsibble::group_by_key() |>
    dplyr::slice_head(n = -4) |> # retrive all steps except the last 4
    dplyr::ungroup()

  test <- tsibble::new_data(train, n = 4) |>
    dplyr::left_join(transaction, by = dplyr::join_by(week, store_id, upc_id))
  message("Full TS splitted successfully.")
  return(list(train = train, test = test))
}
