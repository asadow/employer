test_that("convert_year_to_fiscal() adds 1 to year if month(date) > 4", {
  date_tbl <- tibble::tibble(
    date = c("2022-09-08", "2023-01-01", "2024-03-29")
  ) |>
    dplyr::mutate(
      year = lubridate::year(date)
    ) |>
    convert_year_to_fiscal()

  expect_equal(date_tbl$year, c(2023, 2023, 2024))
})
