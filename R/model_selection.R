#' Extract model fit
#'
#' @param models list of models
#' @param param model parameter to be extracted and added
#'
#' @importFrom purrr map_dbl
#' @return vector of doubles

extract_model_fit <- function(models, param) {
        tmp <- map_dbl(
                models,
                ~ .x[[param]]
        )

        sum(tmp)
}

# TODO how to use this object
safe_arima <- purrr::safely(forecast::Arima)

#' Fit ARIMA model
#'
#' @param df data.frame with nested list column of time-series
#' @param model_order ARIMA model parameters
#' @param drift ARIMA model drift parameter
#' @importFrom dplyr  mutate pull .data
#' @importFrom magrittr %>%
#' @importFrom purrr map keep pluck
#'
#' @return original list of successfully fitted models
#'
calculate_model_fit <- function(df,
                                model_order,
                                drift = FALSE) {
        df %>%
                mutate(new_model = map(
                        .data$ts,
                        ~ safe_arima(.x,
                                order = model_order,
                                include.drift = drift
                        )
                )) %>%
                pull(.data$new_model) %>%
                keep(function(model) is.null(model$error)) %>%
                map(~ pluck(.x, "result"))
}

#' Forecast following year
#'
#' @param tbl data frame including specific nested list columns
#' @param order parameters to define ARIMA model
#' @param drift drift parameter for ARIMA model
#' @param lambda 	Box-Cox transformation parameter. If lambda="auto",
#' then a transformation is automatically selected using BoxCox.lambda.
#' The transformation is ignored if NULL.
#' Otherwise, data transformed before model is estimated.
#' @param log_transform logical to log transform count time series
#'
#' @importFrom purrr map map_dfc
#' @importFrom forecast forecast

forecast_next_year <- function(tbl,
                               order,
                               drift,
                               lambda = NULL,
                               log_transform = FALSE) {
        if (log_transform) {
                tbl$ts <- purrr::map(tbl$ts, log1p)
        }

        tbl$model <- map(tbl$ts, function(ts) {
                model <- safe_arima(ts,
                        order = order,
                        include.drift = drift
                )
                if (is.null(model$error)) {
                        return(model$result)
                } else {
                        warning("Error fitting model")
                        return(NULL)
                }
        })

        tbl$forecast <- map(
                tbl$model,
                function(m) {
                        pred <- forecast(m,
                                h = 1,
                                lambda = lambda,
                                level = 95
                        )
                        df <- data.frame(
                                point = pred$mean,
                                lower95 = as.numeric(pred$lower),
                                upper95 = as.numeric(pred$upper)
                        )

                        if (log_transform) {
                                df <- purrr::map_dfc(df, expm1)
                        }

                        df
                }
        )

        tbl
}

#' Prepare observed notifications
#'
#' Combine cleaned data frames, restrict to specific year and merge with
#' training data sets
#'
#' @param original_data list of data frames
#' @param year numeric to define which year to restrict to
#' @param training_data training data set
#'
#' @importFrom dplyr bind_rows left_join
#'
#'
prepare_actual_notifications <- function(original_data,
                                         year = 2019,
                                         training_data) {
        full_df <- dplyr::bind_rows(original_data)
        filtered_df <- full_df[which(full_df$year == year), ]
        full_df$year <- NULL
        joined <- left_join(
                filtered_df,
                training_data,
                by = c("location" = "location", "age_group" = "age_group")
        )

        joined <- joined[, c("location", "age_group", "cases")]
        names(joined) <- c("location", "age_group", "observed")
        joined
}

#' Evaluate model prediction
#'
#' @param observed data frame of observed notifications
#' @param predicted data frame of predicted notifications
#' @param fn function to compare values
#' @importFrom dplyr left_join

compare_observed_predicted <- function(observed, predicted, fn) {
        predicted <- predicted[, c("location", "age_group", "forecast")]

        df <- left_join(observed,
                predicted,
                by = c("location", "age_group")
        )

        fn(df$observed, df$forecast)
}

#' Predict and compare model predictions
#'
#' @param observed_data data frame of observed notifications
#' @param order model specification
#' @param drift model drift parameter
#' @param evaluation_function function to evaluate observed and predicted
#'   data - requires tow arguments (observed and predicted)
#' @importFrom purrr map_dbl

evaluate_predictions <- function(observed_data = observed,
                                 order = c(0, 1, 0),
                                 drift = FALSE,
                                 evaluation_function) {
        predicted <- forecast_next_year(
                training_ts,
                order, drift
        )

        predicted$forecast <- purrr::map_dbl(
                predicted$forecast,
                ~ as.numeric(.x$mean)
        )

        compare_observed_predicted(
                observed_data, predicted,
                evaluation_function
        )
}
