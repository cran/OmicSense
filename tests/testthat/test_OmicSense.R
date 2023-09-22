test_that("sort", {

  y <- runif(30)
  x <- matrix(runif(30 * 30), 30, 30)

  expect_equal(nrow(os.sort(x, y)), 30)
  expect_equal(ncol(os.sort(x, y)), 30)
})
