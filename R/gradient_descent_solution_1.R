# Understanding the implementation of the one of the most simple optimization problems: Gradient Descent
# Link : https://www.youtube.com/watch?v=Bg7XtyEHDbI

# Install the "Deriv" package to help us evaluate the derivatives
# install.packages("Deriv")
# library(Deriv)

# We will try to get the optimal coefficient and intercept
# regressing the following:

# gradient desc : 
x = iris$Sepal.Length
t = iris$Petal.Width

plot(x,t)
lm(t~x)

# Call:
#   lm(formula = t ~ x)
# 
# Coefficients:
#   (Intercept)            x  
# -3.2002       0.7529  

# Our goal would be to get these values: 
# b = -0.3631 where b is the intercept
# w = 0.4158 where w is the ratio of weight/coefficient

# The following function is the function to calculate w and b using gradient descent
gd = function(t, epoch, lr){
  browser()
  set.seed(1)
  
  # we set initial w and b randomly from a uniform distribution
  w <- runif(1)
  b <- runif(1)
  # here we define the loss function
  # We will use ordinary least squares here for defining the loss function, but our loss function gives us flexibility to use any other function with ease
  
  loss = function(t, w, b, deg = 2) sum((t - (w*x - b))^2)^(1/2)

  # Here we calculate the derviatives with respect to w and b
  dw = Deriv::Deriv(loss, x = "w")
  db = Deriv::Deriv(loss, x = "b")
  
  # The following is just a list to store the values as out gradient descent progresses
  l = list(w = numeric(epoch), b = numeric(epoch), l = numeric(epoch))
  
  for(i in 1:epoch){
    # we update w and b as seen below till we reach desired epoch values
    w <- w - lr*dw(t, w, b)
    b <- b - lr*db(t, w, b)
    
    # print(paste(w, dw(x, w, b), b, dw(x, w, b)))
    
    l[['l']][i] = loss(t, w, b)
    l[['w']][i] = w
    l[['b']][i] = b
    {
      par(mfrow = c(2,2))
      plot(x,t)
      lines(x, l[['w']][length(l[['w']])] * x + l[['b']][length(l[['b']])])
      plot(l[['w']])
      plot(l[['b']])
      plot(l[['l']])
    }
  }
  l
}

# We need to bve careful with the learning rate (0.02), as if we set it too high, out algo might not converge at all
l = gd(t, 100, 0.002)


{
  par(mfrow = c(2,2))
  plot(x,t)
  lines(x, l[['w']][length(l[['w']])] * x + l[['b']][length(l[['b']])])
  plot(l[['w']])
  plot(l[['b']])
  plot(l[['l']])
}

# The output we get from the above is as follows
# The top left graph shows the line plotted using the output of gradient descent implemented manually
# The rest of the graphs showed how the algorithm progressed
