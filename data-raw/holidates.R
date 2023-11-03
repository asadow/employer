# Prepare UofG holidays and dates.
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)

years <- 2014:2100

# In hdays_uog, we exclude Boxing Day
# since Christmas Day is pushed to a business day

hdays_uog <- timeDate::listHolidays("CA") |>
  c("GoodFriday", "ChristmasDay", "DENewYearsEve", "NewYearsDay")

hdates_rough <- expand_grid(hday = hdays_uog, year = years) |>
  mutate(
    date = map2_vec(
      hday,
      year,
      ~ as.Date(eval(parse(text = glue::glue("timeDate::{.x}({.y})"))))
    ),
    wday = wday(date, label = TRUE)
  ) |>
  select(-year)

# UofG December break: 23 to NYD ------------------------------------------

# We don't account for it as this level of accuracy is unnecessary
# And the purpose of this data is for counting sick runs
# which reset every year_half, every January and July

# Corrections -------------------------------------------------------------

observe_on_bnday <- c("NewYearsDay", "CACanadaDay", "ChristmasDay")

holidates <- hdates_rough |>
  mutate(
    date = case_when(
      # Adjusting CanadaDay as per UofG memo February 1, 2023
      hday == "CACanadaDay" & year(date) == 2023 ~ date - 1,
      # Stat. holidays are typically observed on next business day
      hday %in% observe_on_bnday & wday == "Sat" ~ date + 2,
      hday %in% observe_on_bnday & wday == "Sun" ~ date + 1,
      .default = date
    )
  ) |>
  select(-wday) |>
  arrange(date)

usethis::use_data(holidates, overwrite = TRUE)
