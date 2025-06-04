test_that("get_files_from_path reads both csv and xlsx files", {
  test_dir <- system.file("testdata", package = "autodict")

  result <- get_files_from_path(test_dir)

  expect_type(result, "list")
  expect_true("test-file.csv" %in% names(result))
  expect_true("test-file.xlsx" %in% names(result))

  expect_s3_class(result[["test-file.csv"]], "data.frame")
  expect_s3_class(result[["test-file.xlsx"]], "data.frame")

  expect_true(all(c("Name", "Age") %in% names(result[["test-file.csv"]])))
  expect_true(all(c("Product", "Price") %in% names(result[["test-file.xlsx"]])))
})

