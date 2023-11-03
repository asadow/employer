#' Matricize with totals of both rows and columns
#'
#' @description
#' `tr_matrix()` creates a matrix summary out of summarized transactions.
#'
#' @param .data A data frame.
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select]>
#' A variable to summarise by in addition to `days`.
#' @returns A tibble.
#'
#' @export
tr_matrix <- function(.data, by) {
  days <- n <- NULL

  .data |>
    dplyr::summarise(n = dplyr::n(), .by = c(days, {{ by }})) |>
    tidyr::pivot_wider(names_from = {{ by }}, values_from = n) |>
    dplyr::arrange(days) |>
    ## Round and summ. so runs with 1.25 and 1.3 days get lumped into 1 day
    dplyr::mutate(days = round(days, 0)) |>
    dplyr::summarise(
      .by = days,
      dplyr::across(dplyr::everything(), \(x) sum(x, na.rm = TRUE))
    ) |>
    dplyr::mutate(
      Instances = rowSums(
        dplyr::pick(dplyr::where(is.numeric), -days),
        na.rm = TRUE
      )
    ) |>
    janitor::adorn_totals(name = "Instances")
}
