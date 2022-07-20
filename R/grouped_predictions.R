#' Gruoped predictions
#'
#' Aggregate predictions across grouping variables
#'
#' @param prediction_df data frame containing prediction point
#'   estimates and standard deviations
#' @param ... grouping variables
#' 
#' @importFrom dplyr group_by summarise
#' @examples 
#' \dontrun{
#' grouped_predictions(df, type, age_group)
#' }
grouped_predictions <- function(prediction_df, ...) {
        ssum <- function(var) sqrt(sum(var))

        prediction_df %>%
                dplyr::group_by(...) %>%
                dplyr::summarise(
                        point = sum(point),
                        sd = ssum(variance),
                        .groups = "drop"
                )
}
