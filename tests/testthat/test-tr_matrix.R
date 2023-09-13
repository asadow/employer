test_that("tr_matrix() rounds and summarises", {
  df <- tibble::tibble(
    instance = c(1, 1, 1),
    days = c(16.2, 1.25, 1.3),
    month = c("Jan", "Mar", "Mar")
  )

  expected_df <- tibble::tibble(
    days = c(1, 16, "Instances"),
    Jan = c(0, 1, 1),
    Mar = c(2, 0, 2),
    Instances = c(2, 1, 3)
  )

  df <- tr_matrix(df, month) |> tibble::as_tibble()

  expect_equal(df, expected_df, ignore_attr = TRUE)

})
