#################### Linear Discriminant Analysis #####################
library (ISLR)
library (MASS)  ##for lda() function.
library (car)  ##for contrast function.
?Smarket
dim(Smarket)
summary(Smarket)
str(Smarket)
fix(Smarket)
names(Smarket)
pairs(Smarket)
cor(Smarket[,-9])
attach (Smarket)

##We will now split the data to training set and test set and fit the model on training set and estimate the prediction accuracy on the test set.
train = (Year<2005) ##Year contains the Year for all 1250 entries in the Smarket data set.
class(train)##train now is logical vector. 
Smarket.2005 = Smarket[!train,] ##Test data set.
dim (Smarket.2005) ##Dimensions of the test data set. 252 observations.
Direction.2005 = Direction [!train]

## We use lda () function for linear discriminant analysis. Syntax is similar to lm () and glm ()
contrasts(Direction) ##1 indicates market going up. Displays coding R uses for qualitative variables.
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=train) 
names(lda.fit)
coef (lda.fit)
lda.fit
## The coefficients of linear discriminants output provide linear combinations of Lag1 and Lag2 that are used to form the LDA decision rule. 
## i.e. if -0.642*Lag1 - 0.514*Lag2 is large, then the LDA classifier will predict a market increase, and if it is small a market decline.
## The plot () function provides plot of linear discriminants obtained by computing -0.642*Lag1 - 0.514*Lag2 for each of the training observations.
par(mfrow =c(1,1))
plot (lda.fit)


## The predict functions returns a list with 3 elements.
## 1. The first element "class" contains LDA's prediction about the movement of the market.
## 2. The second element "posterior" is a matrix whose kth column contains the posterior probability that the corresponding observation belongs to the kth class.
## 3. Finally "x" contains linear discriminants.

lda.pred = predict(lda.fit, Smarket.2005)  ##See ?predict.lda for help.
names(lda.pred)
summary(lda.pred)
dim(lda.pred$posterior)
lda.pred$posterior[1:10,]

lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,1]>=0.5) ##Displays 70
sum(lda.pred$posterior[,1]<0.5) ##Displays 182

lda.pred$posterior[1:20, ]
lda.class[1:20]

##If we want to use a posterior probability other than 50% we can easily do so. Example:
sum (lda.pred$posterior[,1]>0.9) ##Displays 0 indicating that No day in 2005 met 0.9 threshold.

detach(Smarket)
rm(list=ls())
