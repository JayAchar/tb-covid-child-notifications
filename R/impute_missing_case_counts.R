#' Impute missing counts
#'
#' @param df data.frame
#'
#' @importFrom dplyr group_by mutate
#' @importFrom imputeTS na_interpolation
#' @return data.frame
#'
impute_missing_case_counts <- function(df) {
        df$cases <- as.integer(df$cases)

        group_by(df, iso3, sex, age_group) %>%
                mutate(cases = as.integer(imputeTS::na_interpolation(cases)))
}
