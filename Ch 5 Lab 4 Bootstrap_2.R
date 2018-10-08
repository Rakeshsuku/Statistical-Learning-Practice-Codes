################The Bootstrap - Estimating the accuracy of Linear Regression Model###############
library (ISLR) ## Portfolio data set.
library (boot) ## For boot () function.

#### For performing a bootstrap in R:
##  1. Create a function that compute the statistic of interest.
##  2. Use boot () function to perform the bootstrap by repeatedly sampling observations from the data set with replacement.

##Here we create a function boot.fn which takes as input the Auto dataset and a set of indices for the observations and 
##returns the intercept and slope estimate for the linear regression model.

boot.fn = function (data, index) {
        return (coef (lm(mpg~horsepower, data=data, subset=index)))
}
boot.fn (Auto, 1:392) ##Checks the function.

set.seed (1)
boot.fn (Auto, sample (392, 392, replace=T))  ## creates a bootstrap estimate
boot.fn (Auto, sample (392, 392, replace=T))  ## creates another bootstrap estimate to replicate as in textbook.

boot (Auto, boot.fn, 1000)
## This indicates that the bootstrap estimate for standard error for beta0 is 0.86 and that for beta1 is 0.0074
## We now compute the standard errors using summary () and lm () functions.
summary (lm (mpg~horsepower, data=Auto))$coef
## The standard errors obtained from summary () function is different from the bootstrap estimate. See text page 196 for the reason. 

## Next, we repeat the process for a quadratic model.
boot.fn = function (data, index) {
        coefficients (lm(mpg~horsepower+I(horsepower^2), data=data, subset=index)) ##Coefficients () is an alias to coef () function.
}

set.seed (1)
boot (Auto, boot.fn, 1000)

summary (lm (mpg~horsepower+I(horsepower^2), data=Auto))$coef

## Note that since there is better fit of the data to the quadratic model, there is a better correspondence between the bootstrap estimates
## and the standard estimates of the standard errors

rm(list = ls())
