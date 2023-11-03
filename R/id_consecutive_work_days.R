#' Create id column identifying sets of continuous working days
#'
#' @param .data A data frame with a `date` column of class `Date`.
#' @param x <[`tidy-select`][dplyr::dplyr_tidy_select]>
#' An event variable of interest.
#' @param work_days <[`data-masked`][dplyr::dplyr_data_masking]>
#' Numeric vector of working days of the week (1 is Sunday).
#' @param holidates A data frame with a `date` column for holiday dates.
#' @return A data frame with a new `id` column of class `integer`.
#' @export
id_consecutive_work_days <- function(.data, x, work_days = 2:6, holidates) {
  hday <- NULL

  .data |>
    dplyr::full_join(
      holidate_frame(.data$date, holidates),
      by = "date",
      multiple = "all"
    ) |>
    dplyr::arrange(date) |>
    # To count across days off, keep non-holiday, working days of the week
    dplyr::filter(is.na(hday) & lubridate::wday(date) %in% {{ work_days }}) |>
    dplyr::mutate(row = dplyr::row_number()) |>
    dplyr::mutate(
      id = cumsum(c(1, diff(row) > 1)),
      .by = {{ x }}
    ) |>
    ## Remove unnecessary
    ## 1) days from holidate_frame()
    ## 2) "row" and "holiday" column
    dplyr::filter(!is.na({{ x }})) |>
    dplyr::select(!c(row, hday))
}
