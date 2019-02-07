# Helper functions
.l1R <- function(x) sum(abs(x))
.l1Rprox <- function(x, alpha) sign(x) * (abs(x) - alpha) * (abs(x) > alpha)


#' Parse the input to SR3
#'
#' Sets default values and checks types (within reason)
#'
#' @param A double precision real or complex matrix (dimension, say, MxN)
#' @param b double precision real or complex vector (length M)
#' @param m number of rows of A
#' @param n number of columns of A
#' @param ... optional arguments
#' @return List of \code{R} and \code{Rprox}
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

  isdouble <- function(x) checkmate::expect_double(x)
  isdoublep <- function(x) checkmate::expect_double(x, lower = 0) && all(x > 0)
  isdoublepp <- function(x) checkmate::expect_double(x, lower = 0)
  isdoublem <- function(x) checkmate::expect_double(x, len = m)
  isdoublen <- function(x) checkmate::expect_double(x, len = n)
  isnumericp <- function(x) checkmate::expect_numeric(x) && x > 0
  isnumericpp <- function(x) checkmate::expect_numeric(x, lower = 0)
  isfunhandle <- function(x) checkmate::expect_function(x)

  isdouble(A)
  isdoublem(b)
  isdoublen(defaultx0) # x0
  isdouble(defaultw0)# w0
  isdouble(defaultC)
  isdoublep(defaultlam)
  isdoublep(defaultkap)
  isnumericp(defaultitm)
  isdoublep(defaulttol)
  isnumericpp(defaultptf)

  # TODO: Check if default mode is char
  isdoublepp(defaultl0w)
  isdoublepp(defaultl1w)
  isdoublepp(defaultl2w)
  isfunhandle(defaultR)
  isfunhandle(defaultRprox)

  # TODO: Check if these variables are numeric
  # defaultifusenormal
  # defaultifuselsqr

  parse <- list(A, b, defaultx0, defaultw0, defaultC, defaultlam,
                defaultkap, defaultitm, defaulttol, defaultptf,
                defaultmode, defaultl0w, defaultl1w, defaultl2w,
                defaultR, defaultRprox, defaultifusenormal, defaultifuselsqr)
  names(parse) <- c("A", "b", "x0", "w0", "C", "lam", "kap",
                    "itm", "tol", "ptf", "mode", "l0w", "l1w",
                    "l2w", "R", "Rprox", "ifusenormal", "ifuselsqr")
  return(parse)
}


#' Prox operators
#'
#' Function to return prox operator and regularization function
#'
#' @param p The argument to \code{sr3_parse_input} which is a list that
#' contains the mode variable for regularization
#' @param alpha Penalty term for R (\code{lam}) divided by penalty term for
#'  relaxed expression (\code{kap}).
#' @return A list of \code{R} and \code{Rprox}
reg_prox <- function(p, alpha) {
  l1w <- p$l1w
  R <- function(x) l1w*sum(abs(x))
  Rprox <- function(x, alpha) {
    alpha1 = l1w*alpha
    sign(x) * (abs(x) - alpha1) * (abs(x) > alpha1)
  }

  return(list(R = R, Rprox = Rprox))
}


#' SR3
#'
#' SR3 Relaxed pursuit method for regularized least squares problems
#' of the form:
#'    \code{0.5*norm(A*x-b,2)^2 + lam*R(w) + 0.5*kap*norm(C*x-w,2)^2}
#' over x and w. The output w represents a regularized solution of
#' the least squares problem described by A and b.
#'
#' @param A double precision real or complex matrix (dimension, say, MxN)
#' @param b double precision real or complex vector (length M)
#' @param ... Optional arguments
sr3 <- function(A, b, ...) {
  m <- dim(A)[1]
  n <- dim(A)[2]
  
  parsed <- sr3_parse_input(A, b, m, n)
  
  
  x <- parsed$x0
  w <- parsed$w0
  C <- parsed$C
  lam <- parsed$lam
  kap <- parsed$kap
  itm <- parsed$itm
  tol <- parsed$tol
  ptf <- parsed$ptf
  ifusenormal <- parsed$ifusenormal
  ifuselsqr <- parsed$ifuselsqr
  
  md <- nrow(C)
  if (md != n) w <- matrix(0L, nrow = md, ncol = 1)
  
  rootkap <- sqrt(kap)
  alpha <- lam/kap
  
  Rfunc <- parsed$R
  Rprox <- parsed$Rprox
  # Use this to override defaults
  # results <- reg_prox(parsed, alpha)
  # Rfunc <- results$R
  # Rprox <- results$Rprox
  
  # TODO: use the normal equations and Cholesky factorization
  
  # Least squares
  sys <- rbind(A, rootkap * C)
  u <- rbind(b, rootkap * w)
  # x = lsqr(sys,u,tol/2,100,[],[], x); 
  x <- solve(sys, u, tol/2)
  
  # TODO: QR
  wm <- w
  err <- 2.0 * tol
  noi <- 0
  
  normb <- norm(b, type = '2')
  
  while (err >= tol) {
    if (ifuselsqr) {
      u <- rbind(b, rootkap * w)
      x <- solve(sys, u, tol/2) 
    }
    
    y <- C %*% x
    
    w <- Rprox(y, alpha)
    
    # TODO: write obj
    # obj = 0.5*sum((A*x-b).^2) + lam*Rfunc(w) + 0.5*kap*sum((y-w).^2);
    obj <- 0.5 * sum((A %*% x - b)^2) + lam * Rfunc(w) + 0.5 * kap * sum((y - w)^2)
    err <- sqrt(sum((w - wm)^2)) / normb
    wm <- w
    
    noi <- noi + 1
    if (noi %% ptf == 0) {
      print('iter'); print(noi)
      print('obj'); print(obj)
      print('err'); print(err)
    }
    if (noi >= itm) {
      break
    }
  }
}
