############# Chapetr 7 - Lab 1 Polynomial Regression and Step Function ##########

library (ISLR)
attach (Wage) ##Attach Wage data set.
?Wage
dim(Wage) ##a 3000 x 12 data frame.
summary(Wage)
str(Wage)
fix(Wage)


##First, we fit a polynomial regression model of Wage against Age as in figure 7.1 page 267.
## A fourth degree polynomial regression in Age is fit. 
##Take care to use "wage" to refer to the variable wage than the data frame "Wage" (W Capital).

fit1 =lm(wage~poly(age, 4), data =Wage)
coef(summary(fit1)) ##Displays the coefficients with SE and t values.
## The function poly() returns a matrix whose columns are a basis of orthogonal polynomials,
##which essentially means that each column is a linear combination of the variables age, age^2, age^3 and age^4.
##See ?poly and the note on poly on ISLR page 288.

fit2 =lm(wage~poly(age, 4, raw=TRUE), data=Wage) 
##raw =TRUE argument used to get the age, age^2, etc directly instead of orthogonal polynomials.
coef(summary(fit2)) ##Though coefficients vary, the fitted values remain the same.

##There are several other ways of fitting the same model in R. Ex:
fit2a =lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)
coef(fit2a)
fit2b =lm(wage~cbind(age, age^2, age^3, age^4), data=Wage)
## See: coef(summary(fit2b)) - not very explanatory.


## We will now create a grid of values for age at which we want predictions and 
##then call the generic predict() function, specifying that we need standard errors as well.

agelims =range(age) ##Gets the range for the variable age.
agelims ##Range =[18, 80]
age.grid =seq(from=agelims[1], to=agelims[2]) ##Generates a set of integers from 18 to 60.
length(age.grid) ##Length 63
## See ?seq and age.grid[1:10]


preds =predict(fit1, newdata=list(age=age.grid), se=TRUE) ## Standard error set as True.
##predict() require input to be a list or a dataframe.
se.bands =cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)


##Finally we plot the data.
par(mfrow =c(1,2), mar =c(4.5, 4.5, 1, 1), oma =c(0, 0, 4, 0))
## See?par
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Degree 4 Polynomial", outer=TRUE)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)


##Proof that the fitted values obtained from two different poly() functions does not differ:
preds2 =predict(fit2, newdata =list(age=age.grid), se=TRUE)
max(abs(preds$fit - preds2$fit)) ##7.8* 10^(-11) nearly zero.



## In performing a polynomial regression we must decide on the degree of the polynomial to use.
## One way to do this is by using hypothesis tests (ANOVA).
## We now fit models ranging from linear to 5th degree polynomial and seek to find the simplest model sufficient to explain the data.
## We use anova() function which performs an analysis of variance (ANOVA, using an F statistic).
## Null Hypothesis: M1 is sufficient to explain the data.
## Alternate Hypothesis: A more complex model M2 is required.
## In order to use anova(), M1 nad M2 must be nested models. 
## i.e. the predictors in M1 must be a subset of that in M2.

##Here, we fit 5 different models and subsequently compare simpler models to more complex ones.

fit.1 =lm(wage~age, data=Wage)
fit.2 =lm(wage~poly(age, 2), data=Wage)
fit.3 =lm(wage~poly(age, 3), data=Wage)
fit.4 =lm(wage~poly(age, 4), data=Wage)
fit.5 =lm(wage~poly(age, 5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
?anova

## p values comparing (Model 1 to Model 2) and (Model 2 to Model 3) are very small indicating that Model 1 and Model 2 are insufficient.
## p value comparing (Model 3 to Model 4) is approx 5%, so a 3rd or 4th degree polynomial fits data quite well.
## p value comparing (Model 4 to Model 5) is 37% indicating that a 5th degree polynomial is not required.


##We can perform the same analysis we did above with anova() function, by exploiting the fact that poly() function returns orthogonal polynomials.
coef(summary(fit.5))
## Note that the p values are the same, and the F statistic is the square of the t values.

##ANOVA method works even without orthogonal polynomials. It work even when we have different predictors.
fit.1 =lm(wage~education+age, data=Wage)
fit.2 =lm(wage~education+poly(age, 2), data=Wage)
fit.3 =lm(wage~education+poly(age, 3), data=Wage)
anova(fit.1, fit.2, fit.3)

### Figure 7.1 Right hand Plot ###

## Next, we fit a logistic regression to predict whether an individual earns more than $250,000 per year.
fit =glm(I(wage>250)~poly(age, 4), data =Wage, family =binomial)
##Note that we used I() to create a binary response variable on the fly.

##Now we make predictions:
preds =predict(fit, newdata =list(age=age.grid), se=T)

## The default prediction type for a glm() model is type="link". 
##This means we get predictions for logit. i.e. log((Pr(Y==1|X))/(1-Pr(Y==1|X))) and the predictions are of the form X*Beta_hat. Beta_hat being the coefficient estimates.
## The standard errors are also of the same form.

##In order to get the cofidence interval, we use the transformation Pr(Y==1|X) = exp(X*Beta)/(1+exp(X*Beta)).

pfit =exp(preds$fit)/(1 + exp(preds$fit)) ##Calculates Pr(Y==1|X)
se.bands.logit =cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands =exp(se.bands.logit)/(1 + exp(se.bands.logit))

## Note that we could have directly computed the probabilities by selecting the type="response" option in the pred() function.
## However, in this case the corresponding confidence intervals would not have been sensilble and would end up with negative values.
##preds =predict(fit, newdata =list(age=age.grid), type="response", se=T)

##Now, we create the right hand plot of Figure 7.1 in ISLR.

plot(age, I(wage>250), xlim =agelims, type ="n", ylim =c(0, .2))
points (jitter(age), I((wage>250)/5), cex=0.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

##jitter() function is used to add a small amount of noise to age. 
##This is required so that observations with same age do not cover each other up. This is called a rug plot.


################################# Figure 7.2 ###################################


##In order to fit a step function as discussed in Section 7.2, we use the cut() function in R.

table(cut(age, 4))
?cut ##Convert Numeric to Factor. cut divides the range of x into intervals and codes the values in x according to which interval they fall. 
##The leftmost interval corresponds to level one, the next leftmost to level two and so on.

fit =lm(wage~cut(age, 4), data=Wage)
coef(summary(fit)) ##Returns coefficients with standard errors and tvalues.
preds =predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands =cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)

##Finally we plot the data as in the left hand plot for figure 7.2
par(mfrow =c(1,2), mar =c(4.5, 4.5, 1, 1), oma =c(0, 0, 4, 0))
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Piecewise Constant", outer=TRUE)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)


### Logistic Regression plot - Right hand side plot if Figure 7.2 ###

## Next we fit a logistic regression to create the right hand plot of Figure 7.2

fit =glm(I(wage>250)~cut(age, 4), data =Wage, family =binomial)
preds =predict(fit, newdata =list(age=age.grid), se=T)
pfit =exp(preds$fit)/(1 + exp(preds$fit)) ##Calculates Pr(Y==1|X)
se.bands.logit =cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands =exp(se.bands.logit)/(1 + exp(se.bands.logit))

plot(age, I(wage>250), xlim =agelims, type ="n", ylim =c(0, .2))
points (jitter(age), I((wage>250)/5), cex=0.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

detach(Wage)
rm(list=ls())
