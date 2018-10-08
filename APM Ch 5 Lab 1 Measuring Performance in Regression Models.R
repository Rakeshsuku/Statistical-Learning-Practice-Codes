############ APM Ch 5 Lab 1 Measuring Performance in Regression Models ###########
library(caret)

##To compute model performance the observed and predicted outcomes should be
##stored in vectors.
observed =c(0.22, 0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
            0.62, 0.99, -0.18, 0.32, 0.34, -0.30, 0.04, -0.87, 
            0.55, -1.30, -1.15, 0.20)

predicted =c(0.24, 0.78, -0.66, 0.53, 0.70, -0.75, -0.41, -0.43,
             0.49, 0.79, -1.19, 0.06, 0.75, -0.07, 0.43, -0.42,
             -0.25, -0.64, -1.26, -0.07)


residualValues =observed - predicted
summary(residualValues)

#####Basic Diagnostic Plots:
##Plot of Observed values against Predicted values &
##Plot of residual values against Predicted values
axisRange =extendrange(c(observed, predicted))
par(mfrow =c(1,2))
plot(predicted, observed, ylim =axisRange, xlim =axisRange)
##Add a 45 degree reference line:
abline(0, 1, col ="darkgrey", lty =2)

plot(predicted, residualValues, ylab ="residuals")
abline(h =0, col ="darkgrey", lty =2)

## The caret package contains a function for calculating RMSE and Rsquared value.
R2(predicted, observed) ##51.70%
RMSE(predicted, observed) ##0.5234883

##Base R contains a function to calculate the correlation including  
##Spearman's Rank Correlation.
cor(predicted, observed) ##0.7190357
cor(predicted, observed)^2 ##0.5170

cor(predicted, observed, method ="spearman") ##0.7554552

################################################################################
##Session Information
sessionInfo()
rm(list =ls())
