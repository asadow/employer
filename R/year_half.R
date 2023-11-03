#' Get year-half
#'
#' @description
#' `year_half()` gives the January to June or July to December year-half.
#'
#' @param date A vector of class date.
#' @returns A factor vector.
#'
#' @export
year_half <- function(date) {
  dplyr::case_when(
    lubridate::month(date) <= 6 ~ "Jan:Jun",
    lubridate::month(date) >= 7 ~ "July:Dec"
  ) |>
    factor()
}
