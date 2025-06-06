test_that("compare_two creates expected table", {
  test_df_1 <- tibble::tibble(
    nameL = c("Bob", "Gabay", "Anne"),
    nameF = c("Billy", "Jon", "Raggedy"),
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

  expected <- list(
    dataset1_name = "Dataset1",
    dataset2_name = "Dataset2",
    unique_in_dataset1 = 3,
    unique_in_dataset2 = 3,
    common_items = 1,
    only_in_dataset1 = 2,
    only_in_dataset2 = 2,
    jaccard_similarity = 0.2,
    overlap_pct_dataset1 = 33.3333333,
    overlap_pct_dataset2 = 33.3333333,
    sample_common = "Gabay",
    sample_only_1 = c("Bob", "Anne"),
    sample_only_2 = c("gabay", "Gabby")
  )

  expect_equal(compare_two(test_df_1$nameL, test_df_3$nameL), expected)
})
