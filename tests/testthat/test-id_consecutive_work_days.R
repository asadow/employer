test_that("id_consecutive_work_days() keeps work_days = 2:6", {

  ## August 4-5 and 18-19 are weekend days
  ## August 6 is a holiday

  dates <- c("2018-08-03", "2018-08-07", "2018-08-08",
             "2018-08-17", "2018-08-20") |> as.Date()

  df <- tibble::tibble(date = dates, tr_code = rep("SICK", 5))

  expected_df <- df |> dplyr::mutate(id = c(1, 1, 1, 2, 2))

  expect_equal(
    df |> id_consecutive_work_days(tr_code, work_days = 2:6, employer::holidates),
    expected_df
    )

})

test_that("id_consecutive_work_days() keeps non-holidays if work_days = 1:7", {

  ## August 4-5 and 18-19 are weekend days
  ## August 6 is a holiday

  dates <- c("2018-08-03", "2018-08-05", "2018-08-07", "2018-08-10") |>
    as.Date()

  df <- tibble::tibble(date = dates, tr_code = rep("SICK", 4))

  expected_df <- df |> dplyr::mutate(id = c(1, 2, 2, 3))

  expect_equal(
    df |> id_consecutive_work_days(tr_code, work_days = 1:7, employer::holidates),
    expected_df
  )

})
