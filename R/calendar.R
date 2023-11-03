#' Plot a calendar
#'
#' @description
#' `calendar()` plots a yearly calendar.
#' @param .data A data frame.
#' @param event <[`data-masked`][dplyr::dplyr_data_masking]>
#' A variable to calendar.
#' @returns A ggplot.
#'
#' @export
calendar <- function(.data, event) {
  wday <- week <- year <- day <- NULL

  .data |>
    ggplot2::ggplot(ggplot2::aes(wday, week)) +
    ggplot2::geom_tile(
      alpha = 0.8,
      ggplot2::aes(fill = {{ event }}, group = year),
      color = "black"
    ) +
    ggplot2::facet_wrap(~month, scales = "free_x", ncol = 3) +
    ggplot2::geom_text(ggplot2::aes(label = day)) +
    ggplot2::scale_y_reverse(breaks = NULL) +
    ggplot2::labs(fill = "", x = "", y = "") +
    ggplot2::scale_fill_manual(
      na.value = "white",
      values = c(
        CSICK = "#440154FF",
        CV = "#74D055FF",
        DOCK3 = "#39558CFF",
        DOCK4 = "#32648EFF",
        DOCK5 = "#2D718EFF",
        DOCK6 = "#287D8EFF",
        SICK = "#FDE725FF",
        NOPAY = "beige"
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 14),
      strip.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(15, 15, 15, 15),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(1.5, "lines"),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.spacing.x = ggplot2::unit(0.1, "cm"),
    )
}
