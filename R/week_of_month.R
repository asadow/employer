#' Get week of month where weeks start on Sunday after the first week
#'
#' @description
#' `week_of_month()` calculates the week of the month.
#'
#' @param x A date.
#' @returns A numeric.
#'
#' @export

week_of_month <- function(x) {
  month_floor <- lubridate::floor_date(x, "months")
  wday_of_month_floor <- lubridate::wday(month_floor)

  (wday_of_month_floor + lubridate::mday(x) + 5) %/% 7

  # Explanation:
  # Say our month starts on a Sunday,
  # and our month-day is 8, the second Sunday.
  # If we add FWD + MD we get 9.
  # We know this is week 2, so we add 5 to get 14, as 14/7 = 2.
}
