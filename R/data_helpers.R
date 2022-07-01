read_data <- function(file = c("notifications")) {
        file <- match.arg(file)

        name <- dplyr::case_when(
                file == "notifications" ~ "TB_notifications.csv",
                TRUE ~ ""
        )
        readr::read_csv(here::here("data", name))
}

prepare_data <- function(raw, const = constants()) {
        wide <- list(
                country = raw[raw$year > 2012, const$raw_vars_keep]
        )

        wide$region <- wide$country %>%
                select(-country, -iso3) %>%
                group_by(g_whoregion, year) %>%
                summarise(across(starts_with("newrel_"), ~ sum(.x, na.rm = TRUE)),
                        .groups = "drop"
                )

        long <- list(
                country = pivot_longer(
                        data = wide$country,
                        cols = starts_with("newrel_"),
                        values_to = "cases",
                        names_to = "group"
                ) %>%
                        mutate(group = str_remove(
                                group,
                                "^newrel_"
                        )) %>%
                        separate(
                                col = "group",
                                into = c("sex", "age_group"),
                                sep = 1
                        )
        )

        long$region <- long$country %>%
                select(-country, -iso3) %>%
                group_by(g_whoregion, year, sex, age_group) %>%
                summarise(
                        cases = sum(cases, na.rm = TRUE),
                        .groups = "drop"
                )

        long$global <- long$region %>%
                group_by(year, sex, age_group) %>%
                summarise(cases = sum(cases), .groups = "drop") %>%
                mutate(id = "Global") %>%
                select(id, everything())

        return(list(
                wide = wide,
                long = long
        ))
}

prepare_long_data <- function(lst, const) {
        raw_countries <- lst$long$country %>%
                filter(
                        iso3 %in% const$high_burden,
                        year >= 2014,
                        age_group != "014"
                ) %>%
                mutate(age_group = factor(
                        age_group,
                        levels = c("04", "514", "15plus"),
                        labels = c("0-4 years", "5-14 years", "15+ years"),
                        ordered = TRUE
                ))

        hbc_df <- raw_countries %>%
                impute_missing_case_counts() %>%
                select(location = iso3, year, sex, age_group, cases)

        message("Missing values have been imputed into HBC data")

        region_df <- lst$long$region %>%
                filter(
                        year >= 2014,
                        age_group != "014"
                ) %>%
                mutate(age_group = factor(age_group,
                        levels = c("04", "514", "15plus"),
                        labels = c("0-4 years", "5-14 years", "15+ years"),
                        ordered = TRUE
                )) %>%
                select(location = g_whoregion, year, sex, age_group, cases)

        global_df <- lst$long$global %>%
                filter(
                        year >= 2014,
                        age_group != "014"
                ) %>%
                mutate(age_group = factor(age_group,
                        levels = c("04", "514", "15plus"),
                        labels = c("0-4 years", "5-14 years", "15+ years"),
                        ordered = TRUE
                )) %>%
                select(location = id, year, sex, age_group, cases)

        list(
                region = region_df,
                raw_hbc = raw_countries,
                imputed_hbc = hbc_df,
                global = global_df
        )
}