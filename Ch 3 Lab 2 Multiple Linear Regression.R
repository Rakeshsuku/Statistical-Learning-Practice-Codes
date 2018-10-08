###############Multiple Linear Regression#############
library(ISLR)
library(MASS)
?Boston
names(Boston)
lm.fit = lm(medv~lstat+age, data=Boston)
##Note: lm(y~x1+x2+x3+x4) is used to fit a model with 4 predictors x1, x2, x3 and x4.
summary(lm.fit)

lm.fit = lm(medv~., data=Boston)
## Uses all variables in Boston data set other than medv to fit the model.
names(lm.fit)
summary(lm.fit)

summary(lm.fit)$r.sq  ##Gives R-squared value for the model.
summary(lm.fit)$sigma ##Gives RSE value for the model.
##See ?summary.lm
names(summary(lm.fit))  ##See what is available in summary.lm

par(mfrow=c(2,2))
plot(lm.fit) ## the U-shaped curve in the Residuals vs Fitted values indicate non linearity.


##The vif () function in car library can be used to compute Variance Inflation Factor.
##A VIF greater than 5 or 10 indicates multicollinearity among predictor variables.
## See page 101 of text.
library(car) ##For vif() function.
vif(lm.fit)
##Calculates variance-inflation and generalized variance-inflation factors for linear and generalized linear models.

##Model excluding age (high p-value)
lm.fit1 = lm(medv~.-age, data=Boston)
summary (lm.fit1)
##Alternatively we can use update () function as below:
lm.fit1 = update(lm.fit, ~.-age)
summary(lm.fit1)
## update() will update and (by default) re-fit a model. It does this by extracting the call stored in the object, updating the call and (by default) evaluating that call. 
## Sometimes it is useful to call update with only one argument, for example if the data frame has been corrected.
