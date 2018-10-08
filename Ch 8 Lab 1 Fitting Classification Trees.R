######################Chapter 8 - Fitting Classification Trees###########################
##install.packages("tree")
library(tree) ##tree library is used to construct classification and regression trees.
library(ISLR)

##We use classification trees to analyze the Carseats data set.
##Sales is a continuous variable and first we begin by recording it as binary variable.
##We use ifelse() function to create a variable High, which takes on a value of "Yes" if the Sales > 8 else "No".
attach(Carseats)
dim(Carseats) ##A 400 x 11 matrix.
summary(Carseats)
str(Carseats)
fix(Carseats)

##See ?ifelse
High =ifelse(Sales<=8, "No", "Yes")
##Now we merge the variable High with rest of the Carseats data.
Carseats =data.frame(Carseats, High)

##We now use tree() function to fit a classification tree.
?tree
tree.carseats =tree(High~.-Sales, Carseats)
##The summary() function lists the variables that are used as internal nodes in the tree, the number of terminal nodes and the training error rate.
summary(tree.carseats)
## We see that the training error rate in 9%. See ISLR page 325 to see what this value is.
##The residual mean deviance reported is deviance/(n - T0).
##Trees can be graphically displayed using plot() function and text() function displays node labels.
plot(tree.carseats)
text(tree.carseats, pretty=0)
##pretty=0 argument instructs R to include the category names for any qualitative predictors rather than simply displaying a letter for each category.
##The most inportant indicator of Sales seems to be sheling locations.
##If we just enter the name of the tree object, R prints output corresponding to each branch of the tree.
##R displays the split criterion, the number of observations in that branch, the deviance, 
##the overall prediction for the branch(Yes or No), and the fraction of observations 
##in that branch that takes on values of Yes and No.
##Branches that lead to terminal nodes are indicated by asterisks.
##See: tree.carseats



##We now split the dataset into training set and test set, build the tree on training set and evaluate its performance on test set.
set.seed(2)
train =sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.test =Carseats[-train,]
High.test =High[-train]
tree.carseats =tree(High~.-Sales, Carseats, subset=train)
tree.pred =predict(tree.carseats, Carseats.test, type ="class")
##For classification trees, the argument type ="class" instructs R to return the actual class predictions.
table(tree.pred, High.test)
(86+57)/200 ##Prediction accuracy of 71.5%.


##Next we consider whether purning of tree will lead to improved results.
##The function cv.tree() performs crossvalidation in order to determine the optimal level of tree complexity.
##Cost complexity purning is used in order to select a sequence of trees for consideration.
##We use the argument "FUN =prune.misclass" in order to indicate that we want the classification error rate to guide the
## cross validation and pruning process rather than deviance, the default for cv.tree() function.
?cv.tree
set.seed(3)
cv.carseats =cv.tree(tree.carseats, FUN =prune.misclass)

names(cv.carseats)
##cv.tree() function reports the number of terminal nodes of each tree(size), corresponding error rate and the value of the cost-complexity parameter used("k" in the output).
##dev corresponds to crossvalidation error rate.

cv.carseats

which.min(cv.carseats$dev)
cv.carseats$dev[which.min(cv.carseats$dev)] 
cv.carseats$size[which.min(cv.carseats$dev)] ##Best size =9
cv.carseats$k[which.min(cv.carseats$dev)] ##Best k =1.75
##Minimum cross validation error of 50 correspond to the tree with 9 terminal nodes.

##We plot error rate as a function of size and k.
par(mfrow =c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

##We now use prune.misclass() function to obtain the best 9 node tree.
prune.carseats =prune.misclass(tree.carseats, best=9) ##Using k=1.75, yields same effect as best =9.
par(mfrow =c(1,1))
plot(prune.carseats)
text(prune.carseats, pretty=0)


##Next we evaluate how well does the pruned tree performs on the test data.
tree.pred =predict(prune.carseats, Carseats.test, type ="class")
table(tree.pred, High.test)
(94+60)/200 ##Classification accuracy =77%
##The pruning has not only produced a more interpretable tree but also improved the classification accuracy.

##If we increase the value of best, we obtain larger pruned trees with lower classification accuracy.
prune.carseats =prune.misclass(tree.carseats, best =15)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred =predict(prune.carseats, Carseats.test, type ="class")
table(tree.pred, High.test)
(86+62)/200 ##Prediction accuracy of 74%

detach(Carseats)
rm(list =ls())
