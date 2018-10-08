########### Non-linear transformations of the Predictors #########
library (ISLR)
library (MASS)
library (car)

## For instance given a predictor X we can create a predictor X-square by using I (X^2)
## The I () function is required because ^ has a special meaning in a formula. See ?I.
lm.fit2 = lm (medv~lstat+I(lstat^2), data=Boston)
summary (lm.fit2)

##Next, we compare the model with squared term with the first model.

lm.fit = lm (medv~lstat, data=Boston)
anova (lm.fit, lm.fit2)
## Note: The anova function performs a hypothesis test comparing the two models.
## The null hypothesis is that the two models fit the data equally well and the alternative
## hypothesis is that the full model is superior. Here the F statistics is 135 and the associated 
## p-value is virtually zero. This provides very clear evidence that the model containing the predictors
## lstat and lstat^2 is far superior to the model that only contains lstat.
## See documentation on anova using ?anova.

par (mfrow = c(2,2)) ##To see 4 plots at a time.
plot (lm.fit)  ##Note the pattern in residuals.
plot (lm.fit2)
##Note that there is little discernible pattern in the residuals.

par (mfrow = c(1,1))
plot (Boston$lstat, Boston$medv)
points (Boston$lstat, fitted (lm.fit2), col="red", pch=20) ##Displays the quadratic fit. Arguments are x y coordinates.

##Model with a fifth order polynomial fit (with predictors: X, X^2, X^3, X^4, X^5)
lm.fit5 = lm(medv~poly(lstat,5), data=Boston)
summary (lm.fit5) ##All higher order terms are significant.
anova (lm.fit2, lm.fit5) ##High F value indicates lm.fit5 is a better fit.
points (Boston$lstat, fitted (lm.fit5), col="blue", pch=20) ## Model tend to overfit the data.

##Further investigation reveals that no polynomial term beyond 5th order is significant.
##Fit a model with X^6 check p value and do an anova test.
lm.fit6 = lm (medv~poly(lstat,6), data=Boston)
summary (lm.fit6)
anova (lm.fit5, lm.fit6)

##log transformation.
lm.fitlog = lm (medv~log(rm), data=Boston)
summary (lm.fitlog)
plot (lm.fitlog)

##See the pattern in the residuals
plot (Boston$medv~log (Boston$rm))
points (log (Boston$rm), fitted (lm.fitlog), col="red", pch=21)
abline (reg = lm.fitlog, col="blue")
