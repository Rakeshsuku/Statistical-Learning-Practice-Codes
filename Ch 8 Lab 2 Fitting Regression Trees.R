######################Chapter 8 - Fitting Regression Trees###########################

##Here we fit a regression tree on Boston data set. 
##First we create a training set and fit the tree to the training data.

library(MASS) ##For Boston data set.
library(tree)

?Boston
data(Boston)
dim(Boston) ##A 506 x 14 variable data set.
summary(Boston)
str(Boston)
names(Boston)

set.seed(1)
train =sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston =tree(medv~., Boston, subset =train)
summary(tree.boston)
##Note that only three variables lstat, rm and dis were used in the construction of regression tree.
##Residual mean deviance is simply the sum of squared errors for the tree.

plot(tree.boston)
text(tree.boston)

##Now we use cv.tree() function to see pruning will improve performance.
cv.boston =cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type ='b')
##Here cross validation select the most complex tree with 8 nodes.
##However, if we wish to prune the tree, we can do this by using prune.tree() function.
?prune.tree
prune.boston =prune.tree(tree.boston, best =5)
plot(prune.boston)
text(prune.boston, pretty =0)

##As CV supported the use of the most complex tree, we calculate the test error for unpruned tree.
yhat =predict(tree.boston, newdata =Boston[-train, ])
boston.test =Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2) ##This gives a value of 25.05.
##This means that the test MSE of regression tree is 25.05. 
##The square root of MSE is around 5.005 indicating that the model leads to test predictions that are within around $5005 of the true median home value.

rm(list =ls())

