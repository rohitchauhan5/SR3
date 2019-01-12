#' SR3_PARSE_INPUT parse the input to SR3
#'
#' Sets default values and checks types (within reason)
#'
#'
sr3_parse_input <- function(A, b, m, n, ...) {

  defaultx0 <- matrix(0, n, 1)
  defaultw0 <- matrix(0, n, 1)
  defaultC <- diag(n) # TODO: Convert this to sparse
  defaultlam <- 1.0
  defaultkap <- 1.0
  defaultitm <- 100
  defaulttol <- 1e-6
  defaultptf <- 0
  defaultmode <- '1'
  defaultl0w <- 0.0
  defaultl1w <- 0.0
  defaultl2w <- 0.0
  defaultR <- .l1R
  defaultRprox <- .l1Rprox
  defaultifusenormal <- 0
  defaultifuselsqr <- 0

  # isfunhandle = @(x) isa(x,'function_handle')


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
  m <- dim(A)[1]
  n <- dim(A)[2]

}

### helper functions

.l1R <- function(x) sum(abs(x))
.l1Rprox <- function(x, alpha) sign(x) %*% (abs(x) - alpha) %*% (abs(x) > alpha)

.isdouble <- function(x) checkmate::checkDouble(x)
.isdoublep <- function(x) checkmate::checkDouble(x, lower = 0) && all(x > 0)
.isdoublepp <- function(x) checkmate::checkDouble(x, lower = 0)
.isdoublem <- function(x) checkmate::checkDouble(x, lower = 0, len = m)
.isdoublem <- function(x) checkmate::checkDouble(x, lower = 0, len = n)
.isnumericp <- function(x) checkmate::checkNumeric(x) && x > 0
.isnumericp <- function(x) checkmate::checkNumeric(x, lower = 0)
