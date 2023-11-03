#' Pivot wide transaction summaries by year
#'
#' @param .data A data frame.
#'
#' @export
tr_pivot_years_wider <- function(.data) {
  employee_no <- tr_code <- year <- days <- NULL

  .data |>
    tidyr::pivot_wider(
      id_cols = c(employee_no, tr_code),
      names_from = year,
      values_from = days,
      names_sort = TRUE
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        ~ round(.x, 1)
      )
    )
}
