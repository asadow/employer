test_that("calendar_nested() creates a nested plot column", {
  df <- tibble::tibble(
    employee_no = rep("1005", 3L),
    tr_code = rep("SICK", 3L),
    year = rep(2023, 3),
    date = as.Date(c("2023-02-07", "2023-02-08", "2023-02-09"))
  )

  nested_df <- df |>
    calendar_nested(nest_by = employee_no, event = tr_code)

  expect_named(nested_df, c("employee_no", "year", "calendar"))
  expect_s3_class(nested_df$calendar[[1]], c("ggplot"))
})
