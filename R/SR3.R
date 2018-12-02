#' Compute L1 regularization
#' 
#' @param x A vector of weights
#' @param lambda Parameter to control the amount of regularization
#' @return The L1 norm of \code{x} weighed uniformly by \code{lambda}.
R <- function(x, lambda = 1) {
  lambda * sum(abs(x))
}


#' Cost function described in equation (2) of SR3 paper
#' 
#' @param A m x d real valued matrix representing
#' linear data generating mechanism
#' @param b m dimensional vector of observations
#' @param x d dimensional vector of unknown signal
#' @param lambda Scalar to control strength of regularization
#' @return Loss of least squares problem regularized by
#' R with strength lambda
cost <- function(A, b, x, lambda = 1) {
  sum( (A %*% x - b)^2 ) / (2 * length(b)) + lambda*R(x)
}


#' Function to compute d x d invertible matrix H_k
#' 
#' @param A m x d real valued matrix representing
#' linear data generating mechanism
#' @param kappa Relaxation parameter to control gap
#' between x and w
#' @return d x d matrix
Hk <- function(A, kappa) {
  # C is the identity I matrix
  t(A)%*%A + kappa
}

#' Function to compute matrix F_k
#' 
#' @param A m x d real valued matrix representing
#' linear data generating mechanism
#' @param HkInv Inverse of matrix Hk
#' @param kappa Relaxation parameter to control gap
#' between x and w
#' @return Matrix with shape (m + d) x d 
F_kappa <- function(A, HkInv, kappa) {
  I <- diag(nrow(HkInv))
  r1 <- kappa*A%*%HkInv
  r2 <- sqrt(kappa)*(I - kappa*HkInv)
  rbind(r1, r2)
}

#' Function to compute matrix G_k
#' 
#' @param A m x d real valued matrix representing
#' linear data generating mechanism
#' @param HkInv Inverse of matrix Hk
#' @param kappa Relaxation parameter to control gap
#' between x and w
#' @return Matrix with shape (m + d) x m
G_kappa <- function(A, HkInv, kappa) {
  m <- nrow(A)
  I <- diag(m)
  r1 <- I - A %*% HkInv %*% t(A)
  r2 <- sqrt(kappa) * HkInv %*% t(A)
  rbind(r1, r2)
}

#' Proximal operator for L1 regularized least squares
#' 
#' @param x A vector of weights
#' @param eta Learning rate
#' @param lambda Parameter to control the amount of regularization
#' @return Matrix with shape (m + d) x d 
soft_threshold <- function(x, eta, lambda=1) {
  for (i in 1:length(x)) {
    x[i] <- sign(x[i])*max(0.0, abs(x[i]) - lambda*eta);
  }
  x
}