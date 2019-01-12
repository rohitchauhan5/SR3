#' SR3_PARSE_INPUT parse the input to SR3
#'
#' Sets default values and checks types (within reason)
#'
#'
sr3_parse_input <- function(A, b, m, n, ...) {
  l1R <- function(x) sum(abs(x))
  l1Rprox <- function(x, alpha) sign(x) %*% (abs(x) - alpha) %*% (abs(x) > alpha)
}

#' SR3
#'
#' SR3 Relaxed pursuit method for regularized least squares problems
#' of the form:
#'    0.5*norm(A*x-b,2)^2 + lam*R(w) + 0.5*kap*norm(C*x-w,2)^2
#' over x and w. The output w represents a regularized solution of
#' the least squares problem described by A and b.
#'
#' @param A double precision real or complex matrix (dimension, say, MxN)
#' @param b double precision real or complex vector (length M)
sr3 <- function(A, b, ...) {
  print("hi")
}
