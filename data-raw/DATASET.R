## code to prepare `DATASET` dataset goes here
# This will work after calling devtools::load_all() in development
raw_data_path <- system.file("extdata", "dunnhumby - Breakfast at the Frat.xlsx", package = "salesforecastr")
data <- salesforecastr::load_raw_data(raw_data_path) |>
  salesforecastr::clean_data()
store <- data$store
product <- data$product
transaction <- data$transaction

usethis::use_data(store, product, transaction, overwrite = TRUE)
