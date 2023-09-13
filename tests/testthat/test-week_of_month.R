test_that("week_of_month() gives correct week for April 9th, 2023", {

  # April 9th, 2023 is a Sunday

  date <- lubridate::as_date("2023-04-09")
  expect_equal(week_of_month(date), 3)
})
