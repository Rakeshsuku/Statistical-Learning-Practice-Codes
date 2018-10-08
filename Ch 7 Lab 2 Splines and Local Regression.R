########################### Chapetr 7 - Lab 2 Splines #########################

library(ISLR) ##For Wage dataset.
library(splines)

attach(Wage) ##Attach Wage data set.
?Wage
dim(Wage) ##a 3000 x 12 data frame.
summary(Wage)
str(Wage)
fix(Wage)

agelims =range(age) ##Gets the range for the variable age.
agelims ##Range =[18, 80]
age.grid =seq(from=agelims[1], to=agelims[2]) ##Generates a set of integers from 18 to 60.
length(age.grid) ##Length 63

##################### Cubic Spline ###########################

## The bs() function si used to generate the entire matrix of basis functions for 
##splines with specified set of knots. By default cubic splines are produced.
## This produces a spline with 6 basis function (7 d.o.f - 1 for intercept).
##See explanation at page 293 and 275 -footnote.
## Alternatively we can produce a spline with knots at uniform quantiles by providing the df attribute of bs().

?bs
fit =lm(wage~bs(age, knots =c(25, 40, 60)), data=Wage) ##This fits a cubic spline.
pred =predict(fit, newdata =list(age=age.grid), se=T)
plot(age, wage, col="grey")
title("Figure 7.4 - Cubic Spline vs Natural Spline")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit + 2*pred$se, lty="dashed")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed")

dim(bs(age, knots =c(25, 40, 60))) ##3000 x 6 matrix of basis functions.
str(bs(age, knots =c(25, 40, 60)))
attr(bs(age, knots =c(25, 40, 60)), "degree") ##Displays the degree of the basis function(polynomial) used.

dim(bs(age, df =6)) ##3000 x 6 matrix of basis functions
attr(bs(age, df =6), "knots")

##################### Natural Spline ###########################

##In order to fit a natural spline, we use ns() function.
##Here we fit a natural spline with 3 knots at 25, 40 and 60 to reproduce Figure 7.4 - Seems the figure in the text is incorrect.
##Using df=4 as in lab section will provide a different figure.

fit2 =lm(wage~ns(age, knots =c(25, 40, 60)), data=Wage)
pred2 =predict(fit2, newdata =list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col="red", lwd=2)
lines(age.grid, pred2$fit + 2*pred2$se, lty="dashed", col="red")
lines(age.grid, pred2$fit - 2*pred2$se, lty="dashed", col="red")
legend("topright", legend =c("Cubic Spline", "Natural Spline"), col =c("black", "red"), lty=1, lwd=2, cex=0.8)

##################### Smoothing Spline ###########################

##In order to fit a smoothing spline we use the function smooth.spline()
##To produce figure 7.8:

?smooth.spline
plot(age, wage, xlim =agelims, cex =0.5, col ="darkgrey")
title("Figure 7.8 - Smoothing Spline")
fit =smooth.spline(age, wage, df=16)
fit2 =smooth.spline(age, wage, cv=TRUE)
fit2$df ##Gives a df of 6.794
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend =c("16 DF", "6.8 DF"), col =c("red", "blue"), lty=1, lwd=2, cex=0.8)

##See
names(fit)
str(fit)
names(fit2)
str(fit2)

##################### Local Regression ###########################

##In order to perform local regression, we use loess() function.
?loess

plot(age, wage, xlim =agelims, cex =0.5, col ="darkgrey")
title("Local Regression - Similar to fig 7.10")
fit =loess(wage~age, span =0.2, data =Wage)
fit2 =loess(wage~age, span =0.5, data =Wage)
lines(age.grid, predict(fit, data.frame(age =age.grid)), col ="red", lwd=2)
lines(age.grid, predict(fit2, data.frame(age =age.grid)), col ="blue", lwd=2)
legend("topright", legend =c("Span =0.2", "Span =0.5"), col =c("red", "blue"), lty =1, lwd =2, cex =0.8)

##See:
names(fit)
summary(fit)
str(fit)

##We can also use locfit library to fit a local regression model.

detach(Wage)
rm(list =ls())

