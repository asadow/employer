test_that("get tibble with date input", {
  d <- as.Date("2022-02-02")
  expect_s3_class(
    holidate_frame(c(d, d + 2), employer::holidates),
    class(tibble::tibble())
  )
})

test_that("inputs are dates", {
  d <- "2022-02-02"
  expect_error(
    holidate_frame(c(d, d + 2), employer::holidates),
    "non-numeric argument"
  )
})
