test_that("tr_runs() does not duplicate repeat dates", {
  dates <- c("2018-08-03", "2018-08-03", "2018-08-08") |> as.Date()

  df <- tibble::tibble(
    employee_no = c("1234"),
    date = dates,
    tr_no = 1:3,
    code = rep("SICK", 3)
  )

  expected_df <- df |>
    dplyr::mutate(
      year_half = factor("July:Dec"),
      tr_no = 1:3,
      run = c(1, 1, 2)
    )

  expect_equal(
    df |> tr_runs(employer::holidates),
    expected_df
  )
})
