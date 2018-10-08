################# ISLR Chapter 6 - 6.7 PCR and PLS Regression ##################
## install.packages("pls") - To install pls library.
library(pls) ## For pcr() function.
library(ISLR) ##For Hitters dataset.

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

set.seed(2)
?pcr
pcr.fit =pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
names(pcr.fit)
summary(pcr.fit)

##CV score is provided for each component starting from M=0 (intercept). 
##Note that pcr() reports root mean squared error. In order to get the usual MSE we need to square this quantity.
##For example MSE for Component 1 is (348.9)^2 = 121731.2

##Validationplot() can be used to plot cross validation scores. 
##Using val.type="MSEP" will cause the cross validation MSE to be plotted.
?validationplot ##Part of pls library.
validationplot(pcr.fit, val.type="MSEP")

##we now perform PCR on training dataset and evaluate its performance on test set.

x =model.matrix(Salary~., Hitters)[, -1]
## the first column of model.matrix corresponds to intercept and hence is removed. 
##See: x1 =model.matrix (Salary~., Hitters) ; fix(x1) ; rm(x1)
## model.matrix() function produce a matrix corresponding to 19 predictors 
## and also transforms any qualitative variables into dummy variables.

y=Hitters$Salary

##Create test set and training set.
set.seed(1)
train =sample(1:nrow(x), nrow(x)/2)
test =(-train)
y.test =y[test]

##PCR on training set.
set.seed(1)
pcr.fit =pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)
##Now we see lowest cross validation error when M(no. of components) =7

##Now we compute test MSE.
pcr.pred =predict(pcr.fit, x[test, ], ncomp=7)
mean((pcr.pred - y.test)^2) ##Gives a test MSE of 96556.22

##Finally we fit PCR on full dataset.

pcr.fit =pcr(Salary~., data=Hitters, scale=TRUE, ncomp=7)
summary(pcr.fit)
##Validation argument is required only if we need to see the cross validated 
##error rate for models with different number of components.

#########################Partial Least Square Regression########################

##We fit Partial Least Square regression using plsr() function, also part of pls library.

set.seed(1)
pls.fit =plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")
## We see that lowest CV MSE occurs when M =2.

##We now calculate the corresponding test set MSE.
pls.fit =plsr(Salary~., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)

##Notice that the amount of variance explained by plsr with M=2 is 46.40% 
##which is almost same as that explained by pcr with M=7. This is because pcr looks for
##components that explain variance in predictor variables while plsr considers the response as well.

rm(list=ls())
