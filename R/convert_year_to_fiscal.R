#' Convert calendar year to fiscal
#' @param .data A data frame.
#'
#' @export

convert_year_to_fiscal <- function(.data) {
  year <- NULL
  
  .data |>
    dplyr::mutate(
      year = dplyr::case_when(
        lubridate::month(date) > 4 ~ year + 1,
        .default = year
      )
    )
}
