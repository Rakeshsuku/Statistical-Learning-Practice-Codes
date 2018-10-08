####################### ISLR Chapter 6 - 6.6.2 The Lasso #######################
library(ISLR)
library(glmnet)

data(Hitters)
dim (Hitters)  ##322 x 20
summary(Hitters)
str(Hitters)
fix(Hitters)
names(Hitters)

sum(is.na(Hitters$Salary))
Hitters =na.omit(Hitters)  ##Columns with na values for Salary removed.
sum(is.na(Hitters$Salary))
dim (Hitters)  ##263 x 20

x=model.matrix(Salary~., Hitters)[, -1]
## the first column of model.matrix corresponds to intercept and hence is removed. 
##See: x1 =model.matrix (Salary~., Hitters) ; fix(x1) ; rm(x1)
## model.matrix() function produce a matrix corresponding to 19 predictors 
## and also transforms any qualitative variables into dummy variables.
## This is important as glmnet() can take only numerical quantitative input.

##By default the glmnet() function performs a ridge regression (alpha =0) for an automatically selected range of lambda value.
## if alpha =1, then a lasso is fit. We can also specify a grid of values for lambda.

y=Hitters$Salary

##Now we split the data set into test set and training set to estimate the test error of ridge regression and lasso.
set.seed(1)
train =sample(1:nrow(x), nrow(x)/2)
test =(-train)
y.test =y[test]
grid =10^seq(10, -2, length=100) ##Grid values for lambda. 100 values ranging from 0.01 to 10^10

lasso.mod =glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
##We can see that depending on the choice of tuning parameter, some of the coefficients are exactly zero.

##We now perform cross validation and compute the associated test error.
set.seed(1)
cv.out =cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam
lasso.pred =predict(lasso.mod, s=bestlam, newx =x[test, ])
mean((lasso.pred - y.test)^2)  ##Test MSE =100743

##Here we see that for the model with best value for lambda, 12 of 19 coefficients are exactly zero.

out =glmnet(x, y, alpha=1, lambda=grid)
lasso.coef =predict(out, type="coefficients", s=bestlam)[1:20, ]
lasso.coef

##Note: Do not supply a single value for lambda (for predictions after CV use 
##predict() instead). Supply instead a decreasing sequence of lambda values. 
## glmnet relies on its warms starts for speed, and its often faster to fit a 
##whole path than compute a single fit.

rm(list=ls())
