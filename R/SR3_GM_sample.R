library(ggplot2)
library(xts)
library(zoo)

GM1 <- read.csv("data/R-Project data set (raw 12-04-18) - price to global auto sales.csv",
                skip = 2)
head(GM1)

GM1$Quarter.ended <- as.Date( GM1$Quarter.ended, '%m/%d/%Y')

base <- ggplot(GM1, aes(Quarter.ended, SHARE.PRICE)) + geom_line()
base

GM2 <- read.zoo(GM1, format = "%Y-%m-%d")
plot(GM2)

source("/Users/zarak/Courses/SR3/R/SR3.R")

# Hyperparameters
eta <- 0.0001         # Learning rate
num_iters <- 1000   # Fixed number of iterations
lambda <- 0.01      # Regularization strength
kappa <- 50      # Relaxation parameter

A <- as.matrix(GM1[,c(2,4)])
b <- A[,1]
A[,1] <- 1

# SR3 and Value Function Optimization
# See page 2, section II-A of paper
HkInv <- solve(Hk(A, kappa))

Fk <- F_kappa(A, HkInv, kappa)
Gk <- G_kappa(A, HkInv, kappa)
gk <- Gk %*% b

# Store parameter history
x_cost_history <- double(num_iters)
w_cost_history <- double(num_iters)
x_history <- list(num_iters)
w_history <- list(num_iters)

# Initialize weights
x <- matrix(0, nrow=ncol(A), ncol=1)
w <- matrix(0, nrow=ncol(A), ncol=1)

# Proximal gradient descent
for (i in 1:num_iters) {
  # Compute residuals
  error <- (Fk %*% w - gk)
  # Compute gradient
  delta <- t(Fk) %*% error / length(gk)
  # Take one gradient step
  z <- w - eta * delta
  # Proximal L1 operator
  w <- soft_threshold(z, eta)
  # Recover original weights
  x <- HkInv %*% (t(A) %*% b + kappa * w)
  
  w_cost_history[i] <- cost(Fk, gk, w, lambda)
  x_cost_history[i] <- cost(A, b, x, lambda)
  w_history[[i]] <- w
  x_history[[i]] <- x
}

print(w)
print(x)

make_plots(A[,2], b, x, x_history)
