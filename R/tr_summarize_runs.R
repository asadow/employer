#' Summarize transaction runs: how many days per run
#'
#' @description
#' `tr_summarize_runs()` creates nested summaries by runs.
#'
#' @param .data A data frame.
#' @returns A tibble.
#'
#' @export
tr_summarize_runs <- function(.data) {
  tr_code <- hours_day <- employee_no <- runs <- run <- days <-
    summ_by_run <- dept <- data <- hours <- month <- NULL

  .data |>
    tidyr::nest(.by = tr_code) |>
    dplyr::mutate(
      summ_over_run = purrr::map(
        data,
        \(x) x |>
          # We have NA's in col "run" from tr_runs()
          # as currently weekend transactions are not considered.
          # For now, 0 is used in place of NA to avoid NA in max() and sum()
          dplyr::mutate(
            run = dplyr::if_else(is.na(run), 0, run)
          ) |>
          dplyr::summarize(
            runs = max(run),
            days = sum(hours / hours_day),
            .by = c(employee_no, year_half)
          ) |>
          dplyr::summarize(
            runs = sum(runs),
            days = sum(days),
            .by = employee_no
          )
      ),
      summ_by_run = purrr::map(
        data,
        \(x) x |>
          dplyr::summarise(
            days = sum(hours / hours_day),
            month = lubridate::month(min(date), label = TRUE, abbr = TRUE),
            dept = unique(dept),
            .by = c(employee_no, run, year_half)
          ),
        .progress = " Summarizing runs"
      )
    ) |>
    dplyr::select(!data) |>
    dplyr::mutate(
      summ_by_month = purrr::map(
        summ_by_run, \(x) x |> employer::tr_matrix(month),
        .progress = " Summarizing month runs"
      ),
      summ_by_dept = purrr::map(
        summ_by_run, \(x) x |> employer::tr_matrix(dept),
        .progress = " Summarizing department runs"
      )
    )
}
