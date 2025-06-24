# Set up progress bars
# progressr::handlers(global = TRUE)
progressr::handlers(list(
  progressr::handler_progress(
    format   = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta",
    width    = 80,
    clear    = FALSE,
    enable   = TRUE
  )
))

# MODELS
models <- list(
  # ARIMA default
  arima_def = fable::ARIMA(sqrt(units)),
  # ARIMA default with predictors
  arima_base = fable::ARIMA(sqrt(units) ~ feature + display + tpr_only),
  # ARIMA with lagged predictors
  arima_lagged = fable::ARIMA(
    sqrt(units) ~ feature + display + tpr_only +
      dplyr::lag(feature) + dplyr::lag(display) + dplyr::lag(tpr_only)
  ),
  # ARIMA with seasonal predictors
  arima_seasonal = fable::ARIMA(
    sqrt(units) ~ PDQ(0, 0, 0) + fourier(K = 6) +
      feature + display + tpr_only
  ),
  # ARIMA with seasonal and lagged predictors
  arima_seasonal_lagged = fable::ARIMA(
    sqrt(units) ~ PDQ(0, 0, 0) + pdq(d = 0) +
      fourier(K = 6) + feature + display + tpr_only +
      dplyr::lag(feature) + dplyr::lag(display) + dplyr::lag(tpr_only)
  ),
  # Seasonal decomposition model with ETS errors.
  stl = fabletools::decomposition_model(
    feasts::STL(sqrt(units)),
    fable::ETS(season_adjust ~ season("N"))
  ),
  # Default Neural Network Model with predictors.
  nnetar = fable::NNETAR(sqrt(units) ~ feature + display + tpr_only),
  # Default prophet model with predictors.
  prophet = fable.prophet::prophet(
    sqrt(units) ~ feature + display + tpr_only
  )
)

#' Split dataset into train and test sets
#'
#' @param tsb A `tsibble` with the time series data.
#' @param test_size The proportion of data to use for testing.
#' @returns A `list` with train and test sets.
#' @export
split_dataset <- function(tsb, test_size) {
  index <- tsibble::index_var(tsb)
  keys <- tsibble::key_vars(tsb)
  # Split data
  message("\nSplitting data into train and test sets")
  test <- tsb |>
    tsibble::group_by_key() |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::mutate(n_total = dplyr::n()) |>
    dplyr::filter(.data$row_id > .data$n_total * (1 - test_size)) |>
    dplyr::select(-.data$row_id, -.data$n_total) |>
    dplyr::ungroup()

  train <- dplyr::anti_join(tsb, test, by = c(index, keys))
  return(list(train = train, test = test))
}

#' Forecast and evaluate the models with RMSE
#'
#' @param fbl A `fable` with predictions.
#' @param test A `tsibble` with test data.
#' @param target A `character` indicating the target variable.
#' @returns A `tibble` with RMSE for each model.
#' @export
calculate_rmse <- function(fbl, test, target) {
  index <- tsibble::index_var(test)
  keys <- tsibble::key_vars(test)
  fbl |>
    tibble::as_tibble() |>
    dplyr::left_join(
      test,
      by = c(index, keys),
      suffix = rep("", length(keys))
    ) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(keys, ".model")))) |>
    dplyr::summarise(
      RMSE = sqrt(mean(
        (.data[[target]] - .data$.mean)^2,
        na.rm = TRUE
      )),
      .groups = "drop"
    )
}


#' Train Models
#'
#' @param train A `tsibble` with training data.
#' @param mods A named `list` of models to train.
#' @returns A `mable`.
#' @export
train_model <- function(train, mods) {
  mb <- progressr::with_progress(fabletools::model(train, !!!mods), enable = TRUE)
  return(mb)
}

#' Forecasting function
#'
#' @param mbl A `mable` with fitted models.
#' @param test A `tsibble` with test data.
#' @param bs An `integer` with batch size.
#' @returns A `fable` with forecasts for the test data.
#' @export
get_forecast <- function(mbl, test, bs) {
  batchs <- seq(1, nrow(mbl), by = bs)
  progressr::with_progress(
    {
      p <- progressr::progressor(along = batchs)
      purrr::map_dfr(
        batchs,
        function(x) {
          end <- min(x + bs - 1, nrow(mbl))
          # p(sprintf("%s/%s", end, nrow(mbl)))
          fabletools::forecast(mbl[x:end, ], test)
        }
      )
    },
    enable = TRUE
  )
}

#' Helper function to calculate lower and upper bounds
#'
#' @param fbl A `fable` object with forecasts.
#' @param target A `character` indicating the target variable.
#' @param level A `numeric` with the confidence level.
#' @param pos A `character` with the position of the bound (lower or upper).
#' @returns A `numeric` with the bound or NA if there is an error.
hilo_bound <- function(fbl, target, level, pos) {
  purrr::map_dbl(
    fbl[[target]],
    ~ tryCatch(
      {
        if (pos == "upper") {
          fabletools::hilo(.x, level)$upper
        } else {
          fabletools::hilo(.x, level)$lower
        }
      },
      error = function(e) {
        NA
      }
    )
  )
}

#' Helper function to add CI to fables
#'
#' Adds 80% and 95% CI bounds to a fable object.
#'
#' @param fbl A `fable` object with forecasts.
#' @param target A `character` indicating the target variable.
#' @returns A `tsibble` with lower and upper bounds.
#' @export
add_ci <- function(fbl, target) {
  # Add lower and upper bounds
  fbl$low80 <- fbl |> hilo_bound(target, 80, "lower")
  fbl$up80 <- fbl |> hilo_bound(target, 80, "upper")
  fbl$low95 <- fbl |> hilo_bound(target, 95, "lower")
  fbl$up95 <- fbl |> hilo_bound(target, 95, "upper")

  return(tsibble::as_tsibble(fbl))
}
