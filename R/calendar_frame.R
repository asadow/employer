#' Create a calendar data frame
#'
#' @description
#' `calendar_frame()` takes a range of dates and creates a data frame with the
#' necessary variables for plotting a yearly calendar.
#'
#' @param .date_range A range of dates.
#' @returns A nested tibble.
#'
#' @export

calendar_frame <- function(.date_range) {
  ## Consider tidyr::complete() instead!

  first_year_start <- lubridate::floor_date(.date_range[1], "years")
  # - 1 subtracts one day
  last_year_end <- lubridate::ceiling_date(.date_range[2], "years") - 1

  dates <- seq(first_year_start, last_year_end, by = "day")

  tibble::tibble(date = dates) |>
    dplyr::mutate(
      month = lubridate::month(date, label = TRUE, abbr = FALSE),
      wday = lubridate::wday(date, label = TRUE),
      day = lubridate::mday(date),
      week = week_of_month(date),
      year = lubridate::year(date)
    )
}
