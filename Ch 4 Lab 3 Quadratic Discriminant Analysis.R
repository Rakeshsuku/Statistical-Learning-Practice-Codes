#################### Quadratic Discriminant Analysis #####################

## Quadratic Discriminant Analysis is implemented in R using qda () function part of MASS library.
library (ISLR)
library (MASS) ##for qda() function.

attach (Smarket)
##We will now split the data to training set and test set and fit the model on training set and estimate the prediction accuracy on the test set.
train = (Year<2005) ##Year contains the Year for all 1250 entries in the Smarket data set.
##train now is logical vector. 
Smarket.2005 = Smarket[!train,] ##Test data set.
dim (Smarket.2005) ##Dimensions of the test data set
Direction.2005 = Direction [!train]

qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit ##note that the output does not contain coefficients of the discriminant.

qda.class = predict (qda.fit, Smarket.2005)$class
table (qda.class,Direction.2005)
mean (qda.class == Direction.2005)

detach (Smarket)
rm(list =ls())
