######################Chapter 8 - Boosting###########################

##Here we use gbm package and gbm() function to fit a boosted regression model tree to Bosten data set.

##install.packages("gbm")
library(MASS) ##For Boston data set.
library(gbm)

?Boston
data(Boston)
dim(Boston) ##A 506 x 14 variable data set.
summary(Boston)
str(Boston)
names(Boston)



set.seed(1)
train =sample(1:nrow(Boston), nrow(Boston)/2)
boston.test =Boston[-train, "medv"]
##We run gbm() function with option distribution ="gaussian" for a regression problem.
##And we use distribution ="bernoulli" for classification problems.
##The ntree=5000 indicates that we want 5000 trees and the argument interaction.depth =4 limits depth of each tree.

set.seed(1)
boost.boston =gbm(medv~., data =Boston[train, ], distribution ="gaussian", n.trees =5000, interaction.depth =4 )
summary(boost.boston)
##The summary() function produces a relative influence plot and also outputs the relative influence statistics.

##We see that lstat and rm are the post important variables. Next, we produce a "partial dependenct plot" for these two variables.
##These plots illustrate the marginal effect of the selected variable on the response after integrating onto other variables.

par(mfrow =c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")


##We now use the boosted model to predict accuracy on a test set,
yhat.boost =predict(boost.boston, newdata =Boston[-train, ], n.trees =5000)
mean((yhat.boost - boston.test)^2)
##The test MSE is nearly equal to that of 11.8

##Next we perform boosting with a different value of shrinkage parameter lambda.
##The default value for lambda is 0.001

boost.boston =gbm(medv~., data =Boston[train,], distribution ="gaussian", n.trees =5000, interaction.depth =4, shrinkage =0.2, verbose =F)
yhat.boost =predict(boost.boston, newdata =Boston[-train, ], n.trees =5000)
mean((yhat.boost - boston.test)^2) ##We get an MSE of 11.51
##In this case, using lambda =0.2 leads to a slightly lower Test MSE than lambda =.001

rm(list =ls())
