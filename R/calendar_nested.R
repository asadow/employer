#' Create nested, yearly calendars
#'
#' @description
#' `calendar_nested()` nests data by year and group given by `x` and adds a
#' list of yearly calendars of `y`.
#'
#' @param .data A data frame.
#' @param nest_by <[`tidy-select`][dplyr::dplyr_tidy_select]> 
#' A grouping variable other than year.
#' @inheritParams calendar
#' @returns A nested tibble.
#'
#' @export
calendar_nested <- function(.data, nest_by, event) {
  year <- data <- NULL

  .data |>
    tidyr::nest(.by = c({{ nest_by }}, year)) |>
    dplyr::mutate(
      data = purrr::map(
        data,
        \(x) calendar_frame(range(x$date)) |>
          dplyr::left_join(x, by = "date", suffix = c("", "_ignore"))
      )
    ) |> 
    dplyr::mutate(
      calendar = purrr::map(
        data,
        \(x) calendar(x, {{ event }}) |>
          ggplot2::ggplotGrob() |> 
          ggpubr::as_ggplot(),
        .progress = " Plotting calendars..."
      ),
      .keep = "unused"
    )
}
