#' Creates a PR-prototypical DT.
#'
#' @description `formatted_dt()` creates an HTML widget as per the `DT` package.
#'
#' @param .data A data frame.
#' @param pane_false Indexes representing columns to not have search panes.
#' @param visible_false Indexes representing columns to be invisible 
#' in the table.
#' @param ... More arguments to `DT::datatable(options = ...)`.
#' @export
formatted_dt <- function(.data, pane_false, visible_false, ...) {
  DT::datatable(
    .data,
    rownames = FALSE,
    extensions = c("Select", "RowGroup", "Buttons", "SearchPanes"),
    options = list(
      pageLength = 10,
      dom = "PBfrtip",
      buttons = c("copy", "excel", "pdf", "print"),
      columnDefs = list(
        list(searchPanes = list(show = FALSE), targets = pane_false - 1),
        list(visible = FALSE, targets = visible_false - 1)
      )
    ),
    selection = "none",
    ...
  )
}
