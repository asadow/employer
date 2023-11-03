#' Nest employees, add transaction runs, and unnest
#'
#' @description
#' `tr_runs()` nests by employee, year-half, and tr_code to calculate
#' transaction runs (id's for transactions that continue across
#' working days).
#'
#' @param .data A data frame.
#' @param holidates A data frame with a `date` column for holiday dates.
#' @returns A tibble.
#'
#' @export
tr_runs <- function(.data, holidates) {
  tr_code <- tr_no <- employee_no <- data <- id <- NULL

  .data <- .data |>
    dplyr::mutate(year_half = year_half(date))

  ## NB runs will not have days of the week over than the work_days
  runs <- .data |>
    tidyr::nest(
      data = c(year_half, tr_code, date, tr_no),
      .by = c(employee_no, year_half, tr_code)
    ) |>
    dplyr::mutate(
      data = purrr::map(
        data,
        \(x) x |>
          id_consecutive_work_days(tr_code, work_days = 2:6, holidates) |>
          dplyr::select(-c(year_half, tr_code)),
        .progress = " Adding transaction runs"
      )
    ) |>
    tidyr::unnest(data) |>
    dplyr::rename(run = id)

  ## Hence we left join to the original .data
  .data |>
    dplyr::left_join(
      runs,
      by = dplyr::join_by(employee_no, year_half, tr_code, date, tr_no)
    )
}
