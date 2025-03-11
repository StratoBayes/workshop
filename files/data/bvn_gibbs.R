# function to draw samples from a standard bivariate normal distribution
# using Gibbs sampling
bvn_gibbs<-function (
    n, # number of iterations
    rho, # correlation of the bivariate normal
    inits = c(0, 0) # initial values for the chain, defaults to the origin
    ) 
{
  # create an object to hold output
  output <- data.frame(x = rep(NA_real_, n), y = rep(NA_real_, n))
  # first iteration is the initial values
  x <- inits[1]
  y <- inits[2]
  output[1, ] <- c(x, y)
  # iterate to generate samples
  for (i in 2:n) {
    # draw a new x from the univariate normal conditioned on the current y
    x <- rnorm(1, rho * y, sqrt(1 - rho^2))
    # draw a new y from the univariate normal conditioned on the current x 
    y <- rnorm(1, rho * x, sqrt(1 - rho^2))
    # store this iteration
    output[i, ] <- c(x, y)
  }
  return(output)
}