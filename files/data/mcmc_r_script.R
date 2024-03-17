mcmc_run <- bvn_gibbs(n = 2000, rho = 0.5)
hist(mcmc_run$x,breaks = 25)
hist(mcmc_run$y,breaks = 25)
cor(simple_run)
plot(mcmc_run$x,type='l')
plot(mcmc_run$y,type='l')
plot(mcmc_run, type = 'l')
mcmc_acf <- acf(mcmc_run$x)
length(mcmc_run$x) / (2*sum(mcmc_acf$acf) - 1)

mcmc_run <- bvn_gibbs(n = 2000, rho = 0.5, inits = c(10,10))

plot(mcmc_run$x[101:2000],type='l')
plot(mcmc_run$y[101:2000],type='l')
plot(mcmc_run[101:2000,], type = 'l')
mcmc_acf <- acf(mcmc_run$x[101:2000])

mcmc_run <- bvn_gibbs(n = 2000, rho = 0.998, inits = c(10,10))

mcmc_run <- bvn_gibbs(n = 20000, rho = 0.99, inits = c(10,10))

plot(mcmc_run$x[5001:20000],type='l')
plot(mcmc_run$y[5001:20000],type='l')
plot(mcmc_run[5001:20000,], type = 'l')
mcmc_acf <- acf(mcmc_run$x[5001:20000])

mcmc_thinned <- mcmc_run[seq(5001,20000,20), ]
plot(mcmc_thinned$x,type='l')
plot(mcmc_thinned$y,type='l')
plot(mcmc_thinned, type = 'l')
mcmc_acf <- acf(mcmc_thinned$x)
length(mcmc_thinned$x) / (2*sum(mcmc_acf$acf) - 1)


mcmc_bw <- bimodal_walker(n = 20000, b = 5, prop_sd = 1)

plot(mcmc_bw$x,type='l')
acf(mcmc_bw$x)
hist(mcmc_bw$x,breaks=25)

mcmc_bw <- bimodal_walker(n = 20000, b = 15, prop_sd = 1)

plot(log(mcmc_bw$HR), type='l')
mean(mcmc_bw$x[-1]!=mcmc_bw$x[-20000])