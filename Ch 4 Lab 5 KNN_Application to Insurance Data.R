#################### KNN - Application to Caravan Insurance Data #####################

## Caravan Insurance Data set is a part of ISLR Library. 
## The data contains 85 predictors that measures demographics of 5822 individuals. The response variable is "Purchase".
## In the data set, only 6% of people  purchased caravan insurance.

library(ISLR)  ##For Caravan data set.
library(class) ##For knn() function.
library(MASS)

fix(Caravan)  ##Views Caravan Data set in excel format.
dim(Caravan)
?Caravan
##summary(Caravan)
##str(Caravan)

names(Caravan)
attach(Caravan)

summary(Purchase)
mean(Purchase == "Yes")

##Since the scale of the variables affect KNN classifier, one should standardize the predictor variables before using them in KNN classifier.
## The scale() function does the standardization - mean Zero and std. deviation One.

Standardized.X = scale(Caravan [, -86]) 
##The 86th variable is omitted because it is the response variable "Purchase".

##See the following:
#     var (Caravan [,1])
#     mean (Caravan [,1])
#     var (Standardized.X [,1])
#     mean (Standardized.X [,1])

## We now split the observations into a test set containing 1000 observations and a training set containing the remaining observations.
## We now fit a KNN model on the training data using K = 1.

test = 1:1000
train.X = Standardized.X [-test,]
test.X = Standardized.X [test,]
train.Y = Purchase[-test]
test.Y = Purchase [test]
dim(train.X)
dim(test.X)
set.seed (1)
knn.pred = knn (train.X, test.X, train.Y, k=1)
mean (test.Y!=knn.pred) ##KNN error rate on 1000 test observations is 11.8%
mean (test.Y!="No") 
##However, just by predicting No for all observations we can get an error rate of 6%

table (knn.pred, test.Y)
9/(68+9)

##Using K = 3, the success rate increases to 19% and with K = 5 the rate is 26.7%, which is four times random guessing rate.

knn.pred = knn (train.X, test.X, train.Y, k=3)
table (knn.pred, test.Y)
5/(21+5) ##Success rate of 19.23%

knn.pred = knn (train.X, test.X, train.Y, k=5)
table (knn.pred, test.Y)
4/(11+4) ##Success rate of 26.67%

## As a comparison we fit logistic regression model to the data. If we use 0.5 as the predicted probability cut-off for the classifier.
## However if we use a threshold of 25% we get a success rate of 33%.

glm.fit = glm (Purchase~., data=Caravan, family=binomial, subset=-test)
glm.probs=predict (glm.fit, Caravan [test,], type="response")
glm.pred=rep("No", 1000)
glm.pred[glm.probs > 0.5] = "Yes"
table (glm.pred, test.Y)  ##Not only 7 people are predictted to buy and all these 7 are wrong.

glm.pred=rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"
table (glm.pred, test.Y)  ## A success rate of 33.33%
11/(22+11)

detach (Caravan)
rm(list=ls())
