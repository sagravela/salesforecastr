## code to prepare `DATASET` dataset goes here
# This will work after calling devtools::load_all() in development
data <- salesforecastr::load_raw_data("dunnhumby - Breakfast at the Frat.xlsx") |>
  salesforecastr::process_data()
store <- data$store
product <- data$product
transaction <- data$transaction

usethis::use_data(store, product, transaction, overwrite = TRUE)
