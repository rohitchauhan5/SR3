context('Regularizer and prox operator')

test_that("reg_prox returns list with correct names", {
  p <- NULL
  expect_equal(names(reg_prox(p)), c("R", "Rprox"))
})
