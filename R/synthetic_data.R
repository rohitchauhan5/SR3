# Create synthetic data for testing purposes

set.seed(42)

a <- runif(1000, -5, 5)
b <- a + rnorm(1000) + 3

# add a column of 1's for the intercept coefficient
A <- cbind(1, matrix(a))