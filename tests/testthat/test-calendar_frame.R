
test_that("calendar_frame() creates a year's worth of dates in a data frame", {

  df <- tibble::tibble(
    employee_no = rep("1005", 3L),
    tr_code = rep("SICK", 3L),
    date = as.Date(c("2023-02-07", "2023-02-08", "2023-02-09"))
  )

  whole_year <- calendar_frame(range(df$date))

  whole_year$date[1]

  expect_named(whole_year, c("date", "month", "wday", "day", "week", "year"))
  expect_equal(
    whole_year$date,
    seq.Date(
      as.Date("2023-01-01"),
      as.Date("2023-12-31"),
      by = "day"
      )
    )
})
