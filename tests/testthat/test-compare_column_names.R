test_that("compare_column_names creates a dataframe", {
  test_df_1 <- tibble::tibble(
    nameL = c("Bob", "Joe", "Anne"),
    nameF = c("Billy", "Johnny", "Raggedy"),
    age = c(16, 17, 18)
  )
  test_df_2 <- tibble::tibble(
    LastName = c("Jon", "Jill", "Jack"),
    FirstName = c("Bo", "Bo", "Bo"),
    age = c(19, 20, 21)
  )
  test_df_3 <- tibble::tibble(
    FirstName = c("Tab", "Adam", "Jon"),
    nameL = c("Gabay", "gabay", "Gabby"),
    age = c(45, 31, 28)
  )

  expected <- data.frame(
    Variable = c("nameL", "nameF", "age", "LastName", "FirstName"),
    df1 = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    df2 = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    df3 = c(TRUE, FALSE,TRUE,FALSE,TRUE),
    stringsAsFactors = FALSE
  )
  dfs <- list(test_df_1, test_df_2, test_df_3)
  dfs
  expect_equal(compare_column_names(dfs), expected)

})
