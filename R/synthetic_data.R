# Create synthetic data for testing purposes
set.seed(42)

a1 <- runif(1000, -5, 5)
a2 <- runif(1000, -5, 5)
a3 <- runif(1000, -5, 5)
b <- a1 + rnorm(1000) + 3

# add a column of 1's for the intercept coefficient
A <- cbind(1, matrix(a1), matrix(a2), matrix(a3))


two_dimensional <- function() {
  a <- runif(1000, -5, 5)
  b <- a1 + rnorm(1000) + 3
  A <- cbind(1, matrix(a))
  print("Returning data and labels")
  list(A, b)
}