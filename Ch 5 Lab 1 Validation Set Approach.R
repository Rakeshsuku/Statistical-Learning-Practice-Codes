############################ Validation Set Approach ################################
library (ISLR)
set.seed (1)
## It is generally a good idea to set a random seed when performing an analysis such as 
## cross-validation that contains a set of randomness so that the results can be reproduced later.

train=sample (392, 196) ## Select 196 indices randomly. See content train.
## We use sample () function to split the set of observations into two halves, 
## by selecting random set of 196 observations from original 392 observations.

lm.fit = lm(mpg~horsepower, data=Auto, subset=train)
attach (Auto)
mean ((mpg-predict (lm.fit, Auto))[-train]^2)
## Equivalent to the command: mean( (mpg[-train] - predict(lm.fit, Auto[-train,]))^2 )
## Gives the estimated test MSE for linear regression fit  as 26.14

## We can use a poly () function to estimate the fit for polynomial regressions.
lm.fit2 = lm(mpg~poly (horsepower, 2), data=Auto, subset=train)
mean ((mpg-predict (lm.fit2, Auto))[-train]^2) ## MSE = 19.82

lm.fit3 = lm(mpg~poly (horsepower, 3), data=Auto, subset=train)
mean ((mpg-predict (lm.fit3, Auto))[-train]^2) ## MSE = 19.78

## If we use a different training set we may get a different MSE as shown below:
set.seed (2)
train = sample (392, 196)
lm.fit = lm(mpg~horsepower, subset=train)
mean ((mpg-predict (lm.fit, Auto))[-train]^2) ## MSE =23.29

lm.fit2 = lm(mpg~poly (horsepower, 2), data=Auto, subset=train)
mean ((mpg-predict (lm.fit2, Auto))[-train]^2) ## MSE = 18.90

lm.fit3 = lm(mpg~poly (horsepower, 3), data=Auto, subset=train)
mean ((mpg-predict (lm.fit3, Auto))[-train]^2) ## MSE = 19.25

detach(Auto)
rm(list=ls())
