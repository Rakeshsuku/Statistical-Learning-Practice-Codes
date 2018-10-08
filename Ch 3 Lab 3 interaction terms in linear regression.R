############ Interaction Terms in linear regression##############
library (ISLR)
library (MASS)
library (car)

## The syntax lstat:black tells R to include an interaction term between lstat and back.
## The syntax lstat*age simultaneously include lstat, age, and the interaction term lstat*age as 
## predictors. It is a shorthand for lstat+age+lstat+age.
summary ( lm(medv~lstat*age, data=Boston))
