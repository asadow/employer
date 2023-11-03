test_that("tr_pivot_years_wider() works by ignoring runs", {
  df <- tibble::tibble(
    employee_no = c("1010", "1005", "1005", "1005", "1010", "1010"),
    tr_code = c("SICK", "SICK", "VAC", "SICK", "SICK", "SICK"),
    year = c(2022, 2020, 2019, 2019, 2020, 2019),
    runs = c(1, 2, 12, 2, 1, 2),
    days = c(4, 9, 32, 4, 5, 13.3125),
  )

  expected_df <- tibble::tibble(
    employee_no = c("1010", "1005", "1005"),
    tr_code = c("SICK", "SICK", "VAC"),
    `2019` = c(13.3, 4, 32),
    `2020` = c(5, 9, NA),
    `2022` = c(4, NA, NA),
  )

  expect_equal(df |> tr_pivot_years_wider(), expected_df)
})
