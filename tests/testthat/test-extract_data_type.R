# tests/testthat/utils_helpers.R
test_that("extract_data_type works correctly", {
  df <- tibble::tibble(
    int_col = 1L,
    num_col = 1.5,
    chr_col = "a",
    fac_col = factor("a"),
    log_col = TRUE,
    date_col = as.Date("2020-01-01"),
    dt_col = as.POSIXct("2020-01-01 00:00:00")
  )

  expect_equal(extract_data_type(df, "int_col"), "integer")
  expect_equal(extract_data_type(df, "num_col"), "numeric")
  expect_equal(extract_data_type(df, "chr_col"), "character")
  expect_equal(extract_data_type(df, "fac_col"), "factor")
  expect_equal(extract_data_type(df, "log_col"), "logical")
  expect_equal(extract_data_type(df, "date_col"), "Date")
  expect_equal(extract_data_type(df, "dt_col"), "POSIXt")
})
