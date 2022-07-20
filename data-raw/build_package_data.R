# prepare raw data as external package data

tb_notifications <- read.csv(here::here("data-raw", "TB_notifications.csv"),
                             encoding = "UTF-8")

stringency_index <- read.csv(here::here("data-raw", "stringency_index.csv"))


usethis::use_data(
  tb_notifications,
  stringency_index,
  internal = FALSE,
  overwrite = TRUE
)
