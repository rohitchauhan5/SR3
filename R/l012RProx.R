#' Generate the L0,L1 or L2 prox utility function for the respective L0, L1 or L2 penalties
#' The solution of the prox problem (if prox):
#' argmin_z alpha*(l0w* nnz(z) + l1w*\|z\|_1 + l2w*0.5\|z\|_2^2) + 0.5*\|x-z\|_2^2
#' If ~ifprox, then 
#' l0w*nnz(x) + l1w*\|x\|_1 + l2w*0.5*\|x\|_2^2 is returned
#' 
#' @param x vector
#' @param alpha weight
#' @param mode string
#' @param ifprox boolean variable, can take TRUE/FALSE values
#' @export

mode <- (l0w != 0)*1 + (l1w != 0)*2 + (l2w != 0)*4

# we use mode to avoid unnecessary computation
if(mode == 1){
  if(ifprox = TRUE){
    alpha <- l0w*alpha
    z <- x * (abs(x) > sqrt(2*alpha0))
  }
  else{
    z <- l0w*
  }
}