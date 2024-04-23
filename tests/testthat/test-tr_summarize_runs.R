test_that("tr_summarize_by_runs() gives right columns", {
  df <- tibble::tibble(
    employee_no = c("101769", "101252", "101252", "100340", "101128"),
    code = c("SICK", "FRTU", "WETU", "PB", "PB"),
    date = as.Date(c(
      "2022-12-15", "2022-07-27", "2022-07-27",
      "2022-12-20", "2022-12-20"
    )),
    tot_hrs = c(7, 1, 7, 5, 5),
    hours_day = rep(c(7, 8), c(1L, 4L)),
    year_half = factor(rep("July:Dec", 5L), levels = c("Jan:Jun", "July:Dec")),
    run = c(2, 1, 1, 6, 9),
    dept = c("0808XX", "0862XX", "0862XX", "0846XX", "0846XX"),
  )

  expected <- c("code", "summ_over_run", 
                "summ_by_run", "summ_by_month", "summ_by_dept")

  expect_named(df |> tr_summarize_runs(), expected)
})
