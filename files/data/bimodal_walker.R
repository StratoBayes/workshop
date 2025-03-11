# function to draw samples from a bimodal distribution
# combining normal distributions with sd 1 centred at 0 and b 
# using a Metropolis-Hastings algorithm
bimodal_walker<-function (
    n, # number of iterations
    b = 5, # correlation of the bivariate normal
    prop_sd = 1, # standard deviation of proposal distribution
    inits = 0  # initial value for the chain, defaults to zero
) 
{
  # define probability density function
  dens <- function(v, b)
  {
    (dnorm(v) + dnorm(v, b)) / 2 
  }
  # create an object to hold output and the Hastings ratios
  output <- data.frame(x = rep(NA_real_, n), HR = rep(NA_real_, n))
  # first iteration is the initial values
  x <- inits
  output[1,] <- c(x, NA)
  # iterate to generate samples
  for (i in 2:n) {
    # propose a new x centred on current x
    prop <- rnorm(1, x, prop_sd)
    # calculate the Hastings ratio
    HR = dens(prop, b)/dens(x, b)
    # accept the proposal?
    if (HR > runif(1)) x <- prop
    # store this iteration
    output[i, ] <- c(x, HR)
  }
  return(output)
}