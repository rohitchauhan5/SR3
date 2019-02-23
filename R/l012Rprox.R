#' Return either the value of the penalty or the solution of the prox problem (if ifprox)
#'
#' @param x Vector on which prox operator is run
#' @param alpha weight
#' @param mode string, '0' -> p = 0, etc
#' @param ifprox flag

norm_vec <- function(x) sqrt(sum(x^2))

l012Rprox <- function(x,alpha,l0w,l1w,l2w,ifprox) {
  
  mode <- (l0w != 0)*1 + (l1w != 0)*2 + (l2w != 0)*4;#(l0w != 0)*1;(l1w != 0)*2
  
  if(mode == 1){
    if ( ifprox == TRUE ) {
      alpha0 <- l0w * alpha # Ques : alpha is scalar?
      z <- x * (abs(x) > sqrt(2*alpha0))
    } else {
      z <- l0w * Matrix::nnzero(x)
    }
  } else if (mode == 2){
    if ( ifprox == TRUE ) {
      alpha1 <- l1w * alpha
      z <- sign(x) * (abs(x) - alpha1) * (abs(x) > alpha1)
    } else {
      z <- l1w * sum(abs(x))
    }
  } else if (mode == 3) {
    if ( ifprox == TRUE ) {
      alpha0 <- l0w * alpha
      alpha1 <- l1w * alpha
      z <- sign(x) * (abs(x) - alpha1) * (abs(x) > alpha1)
      fz <- (abs(z) != 0) * alpha0 + abs(z) * alpha1 + 0.5 * abs(z - x)^2
      z <- z * (fz < 0.5 * abs(x)^2)
    } else {
      z <- l0w * Matrix::nnzero(x) + l1w * sum(abs(x))
    }
  } else if (mode == 4) {
    if ( ifprox == TRUE) {
      alpha2 <- l2w * alpha
      z <- x / (1.0 + alpha)
    } else {
      z <- l2w * 05 * sum(abs(x)^2)
    }
  } else if(mode == 5) {
    if (ifprox == TRUE) {
      alpha0 <- alpha * l0w
      alpha2 <- alpha * l2w
      z <- x / (1+alpha2) 
      fz <- (abs(z) != 0) * alpha0 + 0.5*alpha2*(abs(z)^2)+0.5*(abs(z-x)^2)
      z <- z * (fz < 0.5 * (abs(x)^2))
    } else {
      z <- l0w * Matrix::nnzero(x) + l2w * 0.5 * sum(abs(x)^2)
    }
  } else if(mode == 6) {
    if (ifprox == TRUE) {
      alpha1 <- alpha * l1w
      alpha2 <- alpha * l2w
      z <- sign(x)*(abs(x)-alpha1)*(abs(x)>alpha1) / ( 1 + alpha2)
    } else {
      z <- l1w * sum(abs(x)) + l2w * 0.5 * sum((abs(x)^2))
    }
  } else if (mode == 7) {
      if ( ifprox == TRUE) {
        alpha0 = alpha*l0w
        alpha1 = alpha*l1w
        alpha2 = alpha*l2w
        z = sign(x)*(abs(x)-alpha1)*(abs(x)>alpha1)/(1+alpha2)
        fz = (abs(z)!=0)*alpha0+abs(z)*alpha1+0.5*alpha2*abs(z)^2+0.5*abs(z-x)^2
        z = z*(fz < 0.5*abs(x)^2)
      } else {
        z = l0w*nnzero(x)+l1w*sum(abs(x))+l2w*0.5*sum(abs(x)^2)
    }
  } else {
      if ( ifprox == TRUE){
        z <- x
      }
  }
  
  return(z)
}
