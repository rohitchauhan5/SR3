# Modified from http://digitheadslabnotebook.blogspot.com/2012/07/linear-regression-by-gradient-descent.html
set.seed(42)

a <- runif(1000, -5, 5)
b <- a + rnorm(1000) + 3

built_in_regression <- function() {
  linreg <- lm( b ~ a )
  print(linreg)
  plot(a, b)
  abline(linreg, col='red')
}
built_in_regression()

R <- function(x) {
  lambda*sum(abs(x))
}

cost <- function(A, b, x) {
  sum( (A %*% x - b)^2 ) / (2 * length(b)) + lambda*R(x)
}

Hk <- function(A, kappa) {
  # C is the identity I matrix
  t(A)%*%A + kappa
}

# Hyperparameters
eta <- 0.01
num_iters <- 1000
lambda <- 0.001
kappa = 1000

# add a column of 1's for the intercept coefficient
A <- cbind(1, matrix(a))

HkInv <- solve(Hk(A, kappa))

F_kappa <- function(A, HkInv, kappa) {
  I <- diag(nrow(HkInv))
  r1 <- kappa*A%*%HkInv
  r2 <- sqrt(kappa)*(I - kappa*HkInv)
  rbind(r1, r2)
}

G_kappa <- function(A, HkInv, kappa) {
  m <- nrow(A)
  I <- diag(m)
  r1 <- I - A %*% HkInv %*% t(A)
  r2 <- sqrt(kappa) * HkInv %*% t(A)
  rbind(r1, r2)
}

Fk <- F_kappa(A, HkInv, kappa)
Rprof("performance_Gk")
replicate(n=100, G_kapp(A, HkInc, kappa))
Gk <- G_kappa(A, HkInv, kappa)
Rprof(NULL)
gk <- Gk %*% b


# History
x_cost_history <- double(num_iters)
w_cost_history <- double(num_iters)
x_history <- list(num_iters)
w_history <- list(num_iters)

# Initialize weights
x <- matrix(c(0, 0), nrow=2)
w <- matrix(c(0, 0), nrow=2)


soft_threshold <- function(x, eta) {
  for (i in 1:length(x)) {
    x[i] <- sign(x[i])*max(0.0, abs(x[i]) - lambda*eta);
  }
  x
}

# gradient descent
# for (i in 1:num_iters) {
#   error <- (A %*% x - b)
#   delta <- t(A) %*% error / length(b)
#   # print(delta)
#   z <- x - eta * delta
#   x <- soft_threshold(z, eta)
#   cost_history[i] <- cost(A, b, x)
#   x_history[[i]] <- x
# }

# Rprof("performance_gradient_descent")
for (i in 1:num_iters) {
  error <- (Fk %*% w - gk)
  delta <- t(Fk) %*% error / length(gk)
  # print(delta)
  z <- w - eta * delta
  w <- soft_threshold(z, eta)
  
  x <- HkInv %*% (t(A) %*% b + kappa * w)
  
  w_cost_history[i] <- cost(Fk, gk, w)
  x_cost_history[i] <- cost(A, b, x)
  w_history[[i]] <- w
  x_history[[i]] <- x
}
# Rprof(NULL)


print(w)
print(x)

# plot data and converging fit
plot(a,b, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=x_history[[i]], col=rgb(0.8,0.0,0,0.3))
}
abline(coef=x, col='blue')

summaryRprof("performance_gradient_descent")
summaryRprof("performance_Fk")
summaryRprof("performance_Gk")
summaryRprof("performance_Hk")