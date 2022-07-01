extract_model_fit <- function(models, param) {
        map_dbl(
                models,
                ~ .x[[param]]
        ) %>% sum()
}

safe_arima <- safely(forecast::Arima)

calculate_model_fit <- function(df,
                                model_order,
                                drift = FALSE,
                                fit_param = "aic") {
        df %>%
                mutate(new_model = map(
                        ts,
                        ~ safe_arima(.x,
                                order = model_order,
                                include.drift = drift
                        )
                )) %>%
                pull(new_model) %>%
                keep(function(model) is.null(model$error)) %>%
                map(~ pluck(.x, "result"))
}

forecast_next_year <- function(tbl, order, drift, lambda = NULL, log_transform = FALSE) {
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

prepare_actual_notifications <- function(original_data, training_data) {
        full_df <- bind_rows(original_data)
        filtered_df <- filter(full_df, year == 2019)
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

compare_observed_predicted <- function(observed, predicted, fn) {
        predicted <- predicted[, c("location", "age_group", "forecast")]

        df <- left_join(observed,
                predicted,
                by = c("location", "age_group")
        )

        fn(df$observed, df$forecast)
}

evaluate_predictions <- function(observed_data = observed,
                                 order = c(0, 1, 0),
                                 drift = FALSE,
                                 evaluation_function = function(observed, predicted) {
                                         diff <- predicted - observed
                                         mean(diff, na.rm = TRUE)
                                 }) {
        predicted <- forecast_next_year(
                training_ts,
                order, drift
        )

        predicted$forecast <- map_dbl(predicted$forecast, ~ as.numeric(.x$mean))

        compare_observed_predicted(
                observed_data, predicted,
                evaluation_function
        )
}