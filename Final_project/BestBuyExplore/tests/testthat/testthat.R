library(testthat)
library(BestBuyExplore)

testthat::test_that("Find information of the TV brand", {
  expect_equal(dim(Search_Store(10028,20))[1], 44)
  expect_equal(dim(Search_Store(10028,20))[2], 7)
})
