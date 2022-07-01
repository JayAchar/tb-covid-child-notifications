#' Gruoped predictions
#'
#' Aggregate predictions across grouping variables
#'
#' @param prediction_df data frame containing prediction point
#'   estimates and standard deviations
#' @param ... grouping variables
#'
#'
#' @examples grouped_predictions(df, type, age_group)
grouped_predictions <- function(prediction_df, ...) {
        ssum <- function(x) sqrt(sum(x))

        prediction_df %>%
                dplyr::group_by(...) %>%
                dplyr::summarise(
                        point = sum(point),
                        sd = ssum(variance),
                        .groups = "drop"
                )
}