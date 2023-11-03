test_that("year_half() gives correct year half for April 9th, 2023", {
  # April 9th, 2023

  date <- lubridate::as_date("2023-04-09")
  expect_equal(year_half(date), factor("Jan:Jun"))
})
