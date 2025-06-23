# Main Dependencies
library(dplyr)
library(shiny)
library(ggplot2)

# Data
transaction <- arrow::read_parquet(file.path("data", "transaction.parquet"))
store <- arrow::read_parquet(file.path("data", "store.parquet"))
product <- arrow::read_parquet(file.path("data", "product.parquet"))
fbl_arima <- arrow::read_parquet(file.path("forecast", "fbl_arima.parquet")) |>
  mutate(model = "ARIMA")
fbl_stl <- arrow::read_parquet(file.path("forecast", "fbl_stl.parquet")) |>
  mutate(model = "STL")
residuals <- arrow::read_parquet(file.path("forecast", "residuals.parquet")) |>
  mutate(.model = toupper(.model))
