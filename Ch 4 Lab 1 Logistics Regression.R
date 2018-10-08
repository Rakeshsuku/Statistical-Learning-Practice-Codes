############# Logistic Regression - Predicting market direction ############
library(ISLR)  ##Contains Smarket data.
names(Smarket)
fix(Smarket)
dim(Smarket)
summary(Smarket)
str(Smarket)

attach (Smarket)

pairs(Smarket) ##Generate scatter plot matrix for all variables in Smarket.

cor(Smarket [ ,-9]) ##The cor () function gives the pairwise correlation among the predictor variables in a data set.
## The "Direction" variable must be removed from the input to the above function because it is qualitative.

plot(Volume)

## We will now fit a logistic regression model to predict the "Direction" using Lag1 through Lag5 and Volume.
## The glm () function fits generalized linear models, a class of models that includes logistics regression.

glm.fit = glm (Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
##The family = binomial argument instructs R to fit a logistic model and not any other generalized model.
summary (glm.fit)
coef (glm.fit) ## To access just the coefficients of the model.
## Alternatively, we can use "summary (glm.fit)$coef" or "summary (glm.fit)$coef [,4]"
summary (glm.fit)$coef
names(summary(glm.fit))

## The predict () function can be used to predict the probability that the market will go up given the values of the predictors.
##The type = "response" option tells R to output the probability.
##If no data set is supplied to the predict function, then the probabilities are computed for training data set.
glm.probs = predict (glm.fit, type="response")
glm.probs [1:10]
contrasts (Direction)  ##Displays the coding for the dummy variable Direction.

##Below code creates a vector of class predictions based on the predicted probability of a market increase is greater than or less than 0.5
glm.pred =rep("Down", 1250)
glm.pred[glm.probs > 0.5] ="Up"

## The table () function can be used to create a confusion matrix in order to 
##determine how many observations were correctly or incorrectly classified.

table (glm.pred, Direction)
(507+145)/1250  ##Training set prediction accuracy
mean (glm.pred==Direction) ##Training set prediction accuracy

##We will now split the data to training set and test set and fit the model on training set and estimate the prediction accuracy on the test set.
train = (Year<2005) ##Year contains the Year for all 1250 entries in the Smarket data set.
##train now is logical vector. 
Smarket.2005 = Smarket[!train,] ##Test data set.
summary (Smarket.2005)
dim (Smarket.2005) ##Dimensions of the test data set
Direction.2005 = Direction [!train] ##Test set data.

## We now fit a regression model using training data using the subset argument and then obtain the predicted probabilities for the test data.
glm.fit = glm (Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs = predict (glm.fit, Smarket.2005, type="response")
glm.pred = rep ("Down", 252)
glm.pred[glm.probs>0.5]="Up"
table (glm.pred, Direction.2005) ##Here we compute the prediction for 2005 and compare them to the actual movements of the market over that time period.
mean (glm.pred==Direction.2005)
mean (glm.pred!=Direction.2005)
##The results are worse than random guessing (50%). The model has very poor p values (see: summary (glm.fit))
summary(glm.fit)

glm.fit=glm (Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fit, Smarket.2005, type="response")
glm.pred = rep ("Down", 252)
glm.pred [glm.probs>0.5] = "Up"
table (glm.pred, Direction.2005)
mean (glm.pred == Direction.2005)
106/(106+76)

##Now we predict Direction on two days when Lag1 and Lag2 equal [1.2, 1.1] and [1.5, -0.8] respectively.
predict (glm.fit, newdata=data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8), type = "response"))

detach(Smarket)
rm(list =ls())
