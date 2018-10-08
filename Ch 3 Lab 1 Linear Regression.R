######### Linear Regression ##########
library (MASS)
library (ISLR) ##Use install.packages("ISLR") to download packages.
##fix (Boston)  ##fix function invokes edit on the object and then assigns new 
##version of x in user's workspace. Close window to enter new command.
?Boston
names (Boston)
lm.fit <- lm (medv~lstat, data = Boston)
attach (Boston)  ##Directly use variables in Boston dataframe. Remember to detach the dataframe at the end of the code.
plot (medv~lstat)

lm.fit  ##Basic information on linear fit
summary (lm.fit) ##More details including RSE, R-squared, and t-value, p values and F-statistics

##This function adds one or more straight lines through the current plot. 
##The arguments (a, b) represents the intercept and slope. Note that lm.fit returns the intercept and slope for fitted regression line.
abline (lm.fit, col="red") 

##We can use names () function to find out what other pieces of info are stored in lm.fit.
names (lm.fit)
lm.fit$coefficients
coef (lm.fit) ##Better than "lm.fit$coefficients" method. Also try other extracter functions.

confint (lm.fit) ##Computes confidence intervals for one or more parameters in a fitted model.

##predict is a generic function for predictions from the results of various model fitting functions. The function invokes particular methods which depend on the class of the first argument.
##predict () function can be used to produce confidence interval and prediction intervals for the prediction of medv for a given value of lstat.
predict (lm.fit, data.frame (lstat = (c(5, 10, 15))), interval = "confidence")
predict (lm.fit, data.frame (lstat = (c(5, 10, 15))), interval = "prediction")
##Note: Confidence Interval predicts the values for f(X) whereas 
##Prediction Interval predicts the values for f(X)+epsilon. epsilon => random error.
##Prediction Interval is always greater than Confidence Interval. Argument se.fit=TRUE returns standard error of the predictions made.

plot (lstat, medv)
abline (lm.fit, lwd=3, col="red")

##plotting character for plot function can be changed as below.
plot (lstat, medv, pch=20)
plot (1:25, 1:25, pch=1:25) ##All available plotting characters displayed.

##Four diagnostic plots can be simultaneously created by using plot (lm.fit) - hitting enter to view the next plot.
plot (lm.fit)

##To see all 4 plots in one screen:
par (mfrow = c(2,2))
plot (lm.fit)

par (mfrow=c(2,1))
plot (predict (lm.fit), residuals (lm.fit))
plot (predict (lm.fit), rstudent (lm.fit))

##residuals() gives residual values and rstudent() gives studentized residual values.

##Leverage statistics can be computed for any number of predictors using the hatvalues() function.
par (mfrow=c(1,1))  ##par can be used to set or query graphical parameters.
plot(hatvalues(lm.fit))
which.max (hatvalues (lm.fit))
##The which.max () function identifies the index of the largest element of a vector.
##In this case, it tells us which observation has the largest leverage statistic.

detach (Boston)
