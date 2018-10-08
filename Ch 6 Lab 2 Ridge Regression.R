library (ISLR) ##For Hitters dataset.
library (glmnet)  ## To install packages: install.packages("glmnet", repos = "http://cran.us.r-project.org")

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

##By default the glmnet() function performs a ridge regression (alpha =0) for an 
##automatically selected range of lambda value. if alpha =1, then a lasso is fit. We can 
##also specify a grid of values for lambda.

y=Hitters$Salary
grid =10^seq(10, -2, length=100) ##Grid values for lambda. 100 values ranging 
##from 0.01 to 10^10
ridge.mod=glmnet(x, y, alpha=0, lambda=grid)  ##alpha=1 fits a lasso model.
plot(ridge.mod)

## By default, glmnet () function standardizes the variables so that they are on the same scale.
## To turn off the default setting, use the argument standardize=FALSE.
## Associated with each value of lamda is a vector of ridge regression coefficients that can be accessed
## using coef () function. In this case it is a 20 x 100 matrix(19variables + 1intercept).

dim(coef(ridge.mod))
ridge.mod$lambda[50] ##lambda =11497
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2)) ##-1 is to remove intercept term.
##Sum of squares of coefficients exluding intercept when lambda =11497.
## Sum of squares =6.360612

ridge.mod$lambda[60] ##lambda =705
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2)) ##-1 is to remove intercept term.
##Sum of squares of coefficients exluding intercept when lambda =705
## Sum of squares =57.11001

##predict() function for a glmnet object can be used to predict new coefficient values associated a new value of lambda.
predict (ridge.mod, s=50, type="coefficients")[1:20, ] ##s is the value of lambda to be used.
?predict.glmnet ##See help for predict function.

##Now we split the data set into test set and training set to estimate the test error of ridge regression and lasso.
set.seed(1)
train =sample(1:nrow(x), nrow(x)/2)
test =(-train)
y.test =y[test]

##Now we fit a ridge regression model on the training set and evaluate its MSE on the test
##set using lambda =4.
ridge.mod =glmnet(x[train, ], y[train], alpha =0, lambda =grid, thresh =1e-12)
##thresh is the convergence threshhold for coordinate descent. See ?glmnet.
ridge.pred =predict(ridge.mod, s=4, newx=x[test, ])  ##Gives predictions for test set. lamda =4.
mean((ridge.pred - y.test)^2) ##Test set MSE =101037

##If we had just used the mean of training set observation as the prediction (model with only intercept),
##the test MSE would be:
mean((mean(y[train]) -y.test)^2) ##Test set MSE =193253.

##We can get the same result as above by fitting a ridge regression model with a large value of lambda(10^10)
ridge.pred =predict(ridge.mod, s=1e10, newx=x[test, ]) 
mean((ridge.pred - y.test)^2)  ##Test set MSE =193253.

##Now we fit a ridge regression with lamda =0, which is same as a linear model with all variables.
## Here we provide a additional argument "exact=T" when calling the predict function,
## else it will just interpolate over the grid of lamda values used in fitting and
## provide an approximate result.

ridge.pred =predict(ridge.mod, s=0, newx=x[test, ], exact=T)
mean((ridge.pred - y.test)^2) ##Test Set MSE =114783.1

##Instead of randomly choosing a value of lamda, we use Cross Validation.
##The function cv.glmnet() can be used for this purpose.
##This function performs 10 fold cross validation by default. However this can be changed using folds argument.

set.seed(1)
cv.out =cv.glmnet(x[train, ], y[train], alpha=0)
?cv.glmnet
plot(cv.out)
names(cv.out)
bestlam =cv.out$lambda.min
bestlam ##Best value of lambda = 211.74 (212 approx)

##Test MSE for lambda =212.
ridge.pred =predict (ridge.mod, s =bestlam, newx =x[test,])
mean((ridge.pred - y.test)^2) ##Test MSE =96016

##Finally we fit our ridge regression model over the full data set with lamda =212.
out =glmnet(x, y, alpha =0) ##Ridge regression model on full data set.
predict(out, type ="coefficients", s=bestlam)[1:20, ]

##Note: Do not supply a single value for lambda (for predictions after CV use 
##predict() instead). Supply instead a decreasing sequence of lambda values. 
## glmnet relies on its warms starts for speed, and its often faster to fit a 
##whole path than compute a single fit.

## As expected none of the coefficients are 0. Ridge regression does not 
##perform variable selection.

rm(list=ls())
