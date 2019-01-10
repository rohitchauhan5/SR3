# Use R internal library functions to validate results
built_in_regression <- function(a, b) {
  linreg <- lm( b ~ a )
  print(linreg)
  plot(a, b)
  abline(linreg, col='red')
}

# plot data and converging fit
make_plots <- function(a, b, x, x_history) {
  plot(a,b, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
  for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
    abline(coef=x_history[[i]], col=rgb(0.8,0.0,0,0.3))
  }
  abline(coef=x, col='blue')
}