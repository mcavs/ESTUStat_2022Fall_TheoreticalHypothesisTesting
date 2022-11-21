#################################################################
##                  1. Binomial test application               ##
#################################################################
# To test the hypothesis that the proportion of defective 
# (0: non-defective, 1:defective) workpieces of a machine equals 
# 50%. The available dataset contains 40 observations.

# dataset
dataset <- tibble(company = c(rep("A", 20), rep("B", 20)),
                  malfunction = c(1, 1, 0, 1, 0, 1, 0, 1, 0, 1,
                                  0, 0, 1, 0, 0, 1, 1, 1, 0, 1,
                                  0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                  1, 0, 1, 1, 0, 0, 0, 0, 0, 0))

# number of observations
n <- length(dataset$malfunction)

# number of interested events
d <- length(dataset$malfunction[dataset$malfunction == 1])

# proportion of interested events
p <- d / n

# condition for small sample size
ifelse(n * p * (1 - p) < 9, "use prop.test()", "use binom.test()")

# proportion to test
p0 <- 0.5

# binom test
binom.test(d, n, p0, alternative = "two.sided")
prop.test(d, n, p0, alternative = "two.sided")

#################################################################
##            2. Two-sample Binomial test application          ##
#################################################################
# To test the hypothesis that the proportion of defective 
# (0: non-defective, 1:defective) workpieces of machines in 
# company A is more than proportion of the defective machines in 
# company B. The available dataset contains 20 observations for 
# each company.

#################################################################
# 2.1. unpooled variance
#################################################################
# number of observations
nA <- length(dataset$malfunction[dataset$company == "A"])
nB <- length(dataset$malfunction[dataset$company == "B"])

# number of success
sA <- length(dataset$malfunction[dataset$company == "A" &
                                   dataset$malfunction == 1])
sB <- length(dataset$malfunction[dataset$company == "B" &
                                   dataset$malfunction == 1])

# proportion to test
pA <- sA / nA
pB <- sB / nB
# difference of the populations 
d <- pA - pB
d0 <- 0.10
# test statistic
z = (d - d0) / sqrt((pA * (1 - pA) / nA) + (pB * (1 - pB) / nB))
pvalue = 2 * pnorm(-abs(z))
pvalue

#################################################################
# 2.2. pooled variance
#################################################################
# test statistic
p = (pA * nA + pB * nB) / (nA + nB)
z = (pA - pB) / sqrt((p * (1 - p)) * (1 / nA + 1 / nB))
pvalue = 2 * pnorm(-abs(z))
pvalue

#################################################################
##            3. k-sample Binomial test application            ##
#################################################################
# The proportions of male carp in three ponds are tested for
# equality. The observed relative frequency of male carp in pond 
# one is 10/19, in pond two 12/20, and in pond three 14/21. 
x1 <- matrix(c(10, 12, 14, 9, 8, 7), ncol = 2)
x1

chisq.test(x1)

##################################################################
##  4. Simulation study for comparison of binom and prop test   ##
##################################################################

comp1 <- function(n = 100, p = 0.2, p0 = 0.2){
  
  pval.b <- NULL
  pval.p <- NULL
  
  for(i in 1:1000){
    samp1 <- rbinom(n, 1, p)
    d <- sum(samp1)
    pval.b[i] <- binom.test(d, n, p0)$p.value
    pval.p[i] <- prop.test(d, n, p0)$p.value
  }
  return(list(binom = mean(pval.b < 0.05),
              prop = mean(pval.p < 0.05),
              kosul = ifelse(n * p * (1 - p) < 9, 
                             "use prop.test()", 
                             "use binom.test()")))
}

comp1()








