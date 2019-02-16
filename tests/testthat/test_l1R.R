context('l1 regularization')

test_that("l1 norm satisfies nonnegativity", {
  x1 <- matrix(1, 10, 1)
  checkmate::expect_double(.l1R(x1), lower = 0)
  x2 <- -1 * matrix(1, 10, 1)
  checkmate::expect_double(.l1R(x2), lower = 0)
  x3 <- matrix(0, 10, 1)
  checkmate::expect_double(.l1R(x3), lower = 0)
})
