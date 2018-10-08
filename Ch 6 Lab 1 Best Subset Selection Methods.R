#################### Best Subset Selection Methods ###########################

## Here we wish to predict a baseball player's Salary on the basis of various statistics
## associated with performance in the previous year.

library (ISLR)
library (leaps)  ## For regsubsets () function

dim(Hitters)
summary(Hitters)
str(Hitters)
fix (Hitters) ##The fix () function invokes edit on Hitters data set.
names (Hitters)

sum(is.na(Hitters$Salary))  ## Indicates that for 59 observations previous year Salary data is not available.
Hitters = na.omit(Hitters)  ## na.omit () function removes all rows that have missing values from the data set.
dim(Hitters)  ## Note that 59 entries were removed.
sum(is.na (Hitters$Salary))  ## No missing entries

## The regsubsets () function, part of leaps library, performs best subset selection by identifying the best model
## that contains a given number of predictors, where best is quantified using RSS.
## The summary () function outputs the best set of variables for each model size.

regfit.full = regsubsets(Salary~., Hitters)
summary(regfit.full)  ## The asterix indicates that a given variable is included in the corresponding model.
## By default regsubsets () only reports results upto 8 variable model. 
## The nvmax option can be used to return as many results as desired.

regfit.full = regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)  
## The summary () function also returns R-squared, RSS, adjusted R-squared, Cp and BIC.
names(reg.summary)
reg.summary$rsq  ## R-squared statistic increases as expected as the number of predictor variables increases.

par (mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")

## The point() function works like plot except that it puts points on a plot that has already been created.
## The which.max () function can be used to identify the location of the maximum point of a vector.
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)

plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col="red", cex=2, pch=20)

plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col="red", cex=2, pch=20)

?plot.regsubsets
par(mfrow=c(1,1))
plot(regfit.full, scale="r2")  ##Black square indicates selected variable.
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")
## The model with lowest BIC contains 6 variables. We can use coef () function to see coefficient estimates.
coef(regfit.full, 6)

#################### Forward and Backward Stepwise Selection ###########################

## We can use "method" argument to specify whether forward stepwise selection or backward stepwise selection is to be used.
regfit.fwd=regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)

rm(list=ls())
