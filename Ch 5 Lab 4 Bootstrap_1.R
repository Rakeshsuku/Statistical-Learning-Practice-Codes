############################ The Bootstrap ################################
library (ISLR) ## Portfolio data set.
library (boot) ## For boot () function.

?Portfolio
#### For performing a bootstrap in R:
##  1. Create a function that compute the statistic of interest.
##  2. Use boot () function to perform the bootstrap by repeatedly sampling observations from the data set with replacement.

##Here we create a function aplha.fn which takes as input the matrix (X,Y) and a vector indicating which observations should be used to estimate the statistic.

alpha.fn = function (data, index) {
        X = data$X[index]
        Y = data$Y[index]
        return ((var (Y)-cov (X,Y))/(var (X)+var (Y)-2*cov (X,Y)))
}
## The function returns an estimate of alpha based on applying the formula to observations indexed by index argument.

set.seed (1)
alpha.fn (Portfolio, sample (100, 100, replace=TRUE))

## We can implement a bootstrap by performing this command many times, recoding all of the estimates of alpha and computing the resulting standard deviation.
## However, boot () function automates this for us. Here we produce 1000 bootstarp estimates.

names(boot (Portfolio, alpha.fn, R=1000))
##See boot(Portfolio, alpha.fn, R=1000)$t and sd(boot(Portfolio, alpha.fn, R=1000)$t).
class (boot(Portfolio, alpha.fn, R=1000)) ## An object of class boot returned.
sd(boot(Portfolio, alpha.fn, R=1000)$t)

boot (Portfolio, alpha.fn, R=1000)

rm(list=ls())
