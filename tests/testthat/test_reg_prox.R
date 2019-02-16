context('Regularizer and prox operator')

p <- NULL
alpha <- 1
Rprox <- reg_prox(p, alpha)$Rprox
Rfunc <- reg_prox(p, alpha)$R

test_that("reg_prox returns list with correct names", {
  expect_equal(names(reg_prox(p, alpha)), c("R", "Rprox"))
})

test_that("reg_prox returns functions", {
  checkmate::expect_function(Rprox)
  checkmate::expect_function(Rfunc)
})
