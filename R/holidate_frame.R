#' Create tibble of dates and holidays
#'
#' `holidate_frame()` takes a vector of dates
#' and returns a tibble with every date and holiday inside its range.
#'
#' @param date A vector of class `Date`.
#' @param holidates A data frame with a `date` column for holiday dates.
#' @export
holidate_frame <- function(date, holidates) {
  tibble::tibble(date = seq(range(date)[1], range(date)[2], by = "day")) |>
    dplyr::left_join(holidates, by = "date")
}
