test_that("get_example returns first non-NA value as string", {
  df <- tibble::tibble(
    a = c(NA, "apple", "banana"),
    b = c(1, 2, 3),
    c = c(NA, NA, NA)
  )

  expect_equal(get_example(df, "a"), "apple")
  expect_equal(get_example(df, "b"), "1")  # coerced to character
  expect_equal(get_example(df, "c"), "All NA")
})
