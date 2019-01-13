context('l1 regularization')

test_that("l1 norm satisfies nonnegativity", {
  x <- matrix(1, 10, 1)
  checkmate::expect_double(.l1R(x), lower = 0)
})
