# blood-pressure dataset from "Teager and Hunt (2014) Statistical Hypothesis Testing with SAS and R"
blood_pressure <- data.frame(status = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 
                                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                            mmhg = c(120, 115, 94, 118, 111, 102, 102, 
                                     131, 104, 107, 115, 139, 115, 113,
                                     114, 105, 115, 134, 109, 109, 93,
                                     118, 109, 106, 125, 150, 142, 119,
                                     127, 141, 149, 144, 142, 149, 161,
                                     143, 140, 148, 149, 141, 146, 159,
                                     152, 135, 134, 161, 130, 125, 141,
                                     148, 153, 145, 137, 147, 169))


# We need to implement one sample Z-test manually, 
# because there is no ready-to-use function in base R.
z.test <- function(sample, 
                   mu = 0, 
                   sigma = 1, 
                   alternative = "two.sided"){
  xbar <- mean(sample)
  n <- length(sample)
  z <- sqrt(n) * (xbar - mu) / sigma
  if(alternative == "two.sided"){p_value <- 2 * pnorm(-abs(z))}
  if(alternative == "less"){p_value <- pnorm(z)}
  if(alternative == "greater"){p_value <- 1- pnorm(z)}
  return(p_value)
  }

# Then create a 'sim' function to compare the performance of Z and t-test
# in terms of Monte-Carlo simulations under the some conditions.
# sim() function returns the performance of the test
# if you set mu = mu0, it returns the Type I error
# or mu != mu0, it returns the power of the test
sim <- function(n,                 # sample size 
                mu = 0,            # population mean of the generated sample
                mu0 = 0,           # value of the population mean want to be tested
                sig = 1,           # population standard deviation of the generated sample
                sig.s = "unknown", # knowledge about the population sample size
                run                # run time of the simulation
                ){ 
  pval_z <- NULL
  pval_t <- NULL
  for(i in 1:run){
    samp <- rnorm(n, mean = mu, sd = sig)
    pval_z[i] <- z.test(samp, mu = mu0, sigma = ifelse(sig.s == "unknown", sd(samp), sig))
    pval_t[i] <- t.test(samp, mu = mu0)$p.value
  }
  per_z <- mean(pval_z < 0.05)
  per_t <- mean(pval_t < 0.05)
  return(list(z = per_z, t = per_t))
}

# Let see the Type I error probability of the tests when 
# the population standard deviation is unknown
sim(n = 5, sig.s = "unknown", run = 10000)

# Compare the previous results with the condition 
# the population standard deviation is known
sim(n = 5, sig.s = "known", run = 10000)

# You can check the results for rule of thumb: n > 30
sim(n = 30, sig.s = "unknown", run = 10000)
sim(n = 30, sig.s = "known", run = 10000)
