# Modified from http://digitheadslabnotebook.blogspot.com/2012/07/linear-regression-by-gradient-descent.html
source("/Users/zarak/Courses/SR3/R/synthetic_data.R")
source("/Users/zarak/Courses/SR3/R/utility.R")
source("/Users/zarak/Courses/SR3/R/SR3.R")

built_in_regression(a1, b)

# Hyperparameters
eta <- 0.01
num_iters <- 1000
lambda <- 0.01
kappa <- 10000

HkInv <- solve(Hk(A, kappa))

Fk <- F_kappa(A, HkInv, kappa)
Gk <- G_kappa(A, HkInv, kappa)
gk <- Gk %*% b


# History
x_cost_history <- double(num_iters)
w_cost_history <- double(num_iters)
x_history <- list(num_iters)
w_history <- list(num_iters)

# Initialize weights
x <- matrix(0, nrow=4, ncol=1)
w <- matrix(0, nrow=4, ncol=1)

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

# Proximal gradient descent
for (i in 1:num_iters) {
  error <- (Fk %*% w - gk)
  delta <- t(Fk) %*% error / length(gk)
  # print(delta)
  z <- w - eta * delta
  w <- soft_threshold(z, eta)
  
  x <- HkInv %*% (t(A) %*% b + kappa * w)
  
  w_cost_history[i] <- cost(Fk, gk, w, lambda)
  x_cost_history[i] <- cost(A, b, x, lambda)
  w_history[[i]] <- w
  x_history[[i]] <- x
}

print(w)
print(x)

# make_plots(a, b, x, x_history)