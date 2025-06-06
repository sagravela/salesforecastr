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

#' Train Model
#'
#' @param train A `tsibble`.
#' @param models A `list` of models to train.
#' @returns A `mable`.
#' @export
train_model <- function(train, models = models) {
  message("Training model...")
  mb <- progressr::with_progress(fabletools::model(train, !!!models), enable = TRUE)
  message("Model trained.")
  return(mb)
}

#' Forecast with ARIMA and STL+ETS
#'
#' This function forecast on two types of models:
#'
#' **ARIMA model** selected from:
#' - \code{arima_def = fable::ARIMA(sqrt(units))}
#' - \code{arima_lagged = fable::ARIMA(sqrt(units) ~ feature + display + tpr_only +
#'  dplyr::lag(feature) + dplyr::lag(display) + dplyr::lag(tpr_only))}
#
#' Lagged ARIMA model is selected over the default ARIMA model (if available).
#'
#' **STL + ETS model**:
#' After interpolating missing values using the selected ARIMA model,
#' an STL decomposition followed by ETS modeling is performed.
#'
#' @param train A [`tsibble`][tsibble::tsibble] containing the training data.
#'
#' @return A [`mable`][fabletools::mable] containing the trained ARIMA and STL+ETS models.
train_arima_stl <- function(train) {
  message("\nTraining ARIMA...")
  mbl_arima <- train_model(
    train,
    models = list(
      arima_def = models$arima_def,
      arima_lagged = models$arima_lagged
    )
  ) |>
    mutate(arima = if_else(
      is_null_model(arima_lagged), arima_def, arima_lagged
    )) |>
    select(-arima_def, -arima_lagged) |>
    filter(!is_null_model(arima))

  message("\nInterpolating and training STL+ETS model ...")
  mbl_stl <- mbl_arima |>
    generics::interpolate(
      semi_join(train, mbl_arima, by = c("store_id", "upc_id"))
    ) |>
    train_model(models = models$stl)
  return(dplyr::bind_cols((mbl_arima), mbl_stl["stl"]))
}

#' Forecasting function
#'
#' @param fit A `mable` with fitted models.
#' @param test A `tsibble` with test data.
#' @param bs An `integer` with batch size.
#' @returns A `fable` with forecasts for the test data.
#' @export
get_forecast <- function(fit, test, bs) {
  batchs <- seq(1, nrow(fit), by = bs)
  progressr::with_progress(
    {
      p <- progressr::progressor(along = batchs)
      purrr::map_dfr(
        batchs,
        function(x) {
          end <- min(x + bs - 1, nrow(fit))
          # p(sprintf("%s/%s", end, nrow(fit)))
          fabletools::forecast(fit[x:end, ], test)
        }
      )
    },
    enable = TRUE
  )
}

#' Helper function to calculate lower and upper bounds
#'
#' @param fc A `fable` object with forecasts.
#' @param level A `numeric` with the confidence level.
#' @param pos A `character` with the position of the bound (lower or upper).
#' @returns A `numeric` with the bound or NA if there::here is an error.
hilo_bound <- function(fc, level, pos) {
  purrr::map_dbl(
    fc$units,
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
#' @param fc A `fable` object with forecasts.
#' @returns A `tsibble` with lower and upper bounds.
#' @export
add_ci <- function(fc) {
  # Add lower and upper bounds
  fc$low80 <- fc |> hilo_bound(80, "lower")
  fc$up80 <- fc |> hilo_bound(80, "upper")
  fc$low95 <- fc |> hilo_bound(95, "lower")
  fc$up95 <- fc |> hilo_bound(95, "upper")

  return(
    fc |>
      tsibble::as_tsibble() |>
      dplyr::select(-units) |>
      dplyr::rename(p_units = ".mean")
  )
}

#' Forecasting function for ARIMA
#'
#' @description
#' Forecast for Dinamyc Regression Model with ARIMA errors.
#' Because this model needs regressors, we will forecast in all possible combination of
#' regressors (feature, display, tpr_only).
#' @param fit A `mable` with fitted models.
#' @param data A `tsibble` with time series data.
#' @param bs An `integer` with batch size.
#' @returns A `fable` with forecasts for the test data.
#' @export
forecast_arima <- function(fit, data, bs) {
  combinations <- list(
    list(feature = 0, display = 0, tpr_only = 0),
    list(feature = 1, display = 0, tpr_only = 0),
    list(feature = 0, display = 1, tpr_only = 0),
    list(feature = 0, display = 0, tpr_only = 1),
    list(feature = 1, display = 1, tpr_only = 0)
  )

  fc_arima <- purrr::map_dfr(
    seq_along(combinations), function(i) {
      message(
        "\n\nFeature = ", combinations[[i]]$feature,
        "| Display = ", combinations[[i]]$display,
        "| TPR = ", combinations[[i]]$tpr_only,
        "\n"
      )
      ds <- tsibble::new_data(data, n = 2) |>
        dplyr::mutate(!!!combinations[[i]])
      fc <- get_forecast(dplyr::select(fit, -stl), ds, bs) |>
        dplyr::mutate(fc_ind = i)
      # Save checkpoint just in case
      # saveRDS(fc, glue::glue("output/model/fc_arima_{i}.rds"))

      # Return as tibble to avoid problems binding later
      return(tibble::as_tibble(fc))
    }
  )
  return(
    fc_arima |>
      tsibble::as_tsibble(index = week, key = c(store_id, upc_id, .model, fc_ind)) |>
      add_ci()
  )
}
