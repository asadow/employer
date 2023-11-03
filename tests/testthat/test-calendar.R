test_that("calendar() creates ggplot with certain characteristics", {
  df <- tibble::tibble(
    employee_no = rep("1005", 3L),
    tr_code = rep("SICK", 3L),
    date = as.Date(c("2023-02-07", "2023-02-08", "2023-02-09"))
  )

  whole_year <- calendar_frame(range(df$date)) |>
    dplyr::left_join(df, by = "date", suffix = c("", "_ignore"))

  p <- whole_year |> calendar(tr_code)
  p$layers[[1]]$geom |> class()
  expect_s3_class(p$layers[[1]], "ggproto")
  expect_contains(p$layers[[1]]$geom |> class(), "GeomTile")
  expect_contains(p$layers[[1]]$stat |> class(), "StatIdentity")
  expect_identical(p$labels$label, "day")
  expect_identical(p$labels$group, "year")
  expect_null(p$scales$scales[[1]]$range$range)
})
