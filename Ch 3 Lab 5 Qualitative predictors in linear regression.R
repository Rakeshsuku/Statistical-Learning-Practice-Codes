############ Qualitative Predictors #############
library (ISLR) ##Contains Carseats data - A simulated data set containing sales of child car seats at 400 different stores. 
##We will attempt to predict Sales in 400 locations based on a number of predictors.
fix (Carseats) ##Opens the data sheet for editting in tabular format.
names (Carseats) ##ShelveLoc is a qualitative variable indicating quality of shelving location.
## Given a qualitative variable like ShelveLoc, R generates dummy variables automatically.
summary (Carseats)  ## 5 point summary + mean for quantitative. distinct values + frequency for qualitative.

lm.fit = lm (Sales~. +Income:Advertising + Price:Age, data=Carseats)
summary (lm.fit)

##The contrasts() function returns the coding that R uses for dummy variables.
contrasts (Carseats$ShelveLoc)  ##See ?contrasts
