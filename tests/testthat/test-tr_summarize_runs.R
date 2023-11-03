test_that("tr_summarize_by_runs() gives right columns", {
  df <- tibble::tibble(
    employee_no = c("101769", "101252", "101252", "100340", "101128"),
    tr_code = c("SICK", "FRTU", "WETU", "PB", "PB"),
    date = as.Date(c(
      "2022-12-15", "2022-07-27", "2022-07-27",
      "2022-12-20", "2022-12-20"
    )),
    hours = c(7, 1, 7, 5, 5),
    hours_day = rep(c(7, 8), c(1L, 4L)),
    year_half = factor(rep("July:Dec", 5L), levels = c("Jan:Jun", "July:Dec")),
    run = c(2, 1, 1, 6, 9),
    dept = c("0808XX", "0862XX", "0862XX", "0846XX", "0846XX"),
  )

  expected <- c("tr_code", "summ_over_run", "summ_by_run", "summ_by_month", "summ_by_dept")

  expect_named(df |> tr_summarize_runs(), expected)
})


# test_that(
#   "summ_over_run gets max run per half-year before summing",
#   {
#     df <- tibble::tibble(
#       employee_no = c(rep("1010", 3), rep("1005", 3)),
#       tr_code = c(rep("SICK", 3), "VAC", "VAC", "SICK"),
#       year = rep(2023, 6),
#       hours = rep(8, 6L),
#       hours_day = c(rep(7, 3L), rep(8, 3L)),
#       year_half = factor(
#         c(rep("July:Dec", 1L), rep("Jan:Jun", 5L)),
#         levels = c("Jan:Jun", "July:Dec")
#       ),
#       run = c(1, 1, 2, 1, 1, 1),
#     )
#
#     expected_df <- tibble::tibble(
#       employee_no = c("1010", "1005", "1005"),
#       tr_code = c("SICK", "VAC", "SICK"),
#       year = rep(2023, 3L),
#       runs = c(3, 1, 1),
#       days = c(3.428571428571428381105, 2, 1),
#     )
#
#     expect_equal(df |> ????(), expected_df)
#   })
