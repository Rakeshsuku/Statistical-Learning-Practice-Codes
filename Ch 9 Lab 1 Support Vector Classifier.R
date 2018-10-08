###################### Ch 09 Lab 1 - Support Vector Classifiers ##################

##We use e1071 library in R to fit Support vector classifiers & Support Vector Machines.
##Another option is Liblinear library which is useful for very large linear problems.

##install.packages("e1071")
library(e1071)
##e1071 contains implementations for a number of statistical learning methods.
##We can use svm() function to fit a support vector classifier with the argument kernel ="linear".
##A cost argument allows us to specify the cost of a violation to the margin.
##When the cost is small, the margin will be wide and many support vectors will be on the margin or vilate the margin.
##When the cost is large, the margin will be narrow and there will be a few support vectors on the margin or violating it.

##Here we use the svm function on two diamensional example so that we can plot the resulting decision boundary.
##We begin by generating the observations which belog to two classes.

set.seed(1)
x =matrix(rnorm(20*2), ncol =2) ## A 20 x 2 matrix - predictor variables.
y =c(rep(-1, 10), rep(1,10)) ## A 20 element vector - response variable.
x[y==1, ] =x[y==1, ] + 1 ##Increments the second half of x(from row 11 to row 20) by 1.

##We begin by checking if the classes are linearly seperable.
plot(x, col =(3-y)) ##Indicates that the classes are not linearly seperable.

##Next we fit a support vector classifier using svm() function.
##Note that in order for the svm() function to perform classification (instead of 
##svm based regression), we must encode the response as a factor variable.
##The argument scale =FALSe tells svm() to not to center-scale each of the feature variables. 
dat =data.frame(x =x, y =as.factor(y)) ##Creates a data frame.
?svm
svmfit =svm(y~., data =dat, kernel ="linear", cost =10, scale =FALSE)
names(svmfit)
plot(svmfit, dat) ##Plots the svm classifier
##See ?plot.svm

##Note that only one observation is misclassified. y =-1: black cross or circle; y =+1: red cross or circle.
##Crosses indicate support vectors. Note that there are 7 support vectors. 
##We can determine the identities of support vectors as follows:

svmfit$index
x[svmfit$index, ] ##Gives the support vectors.

##We can use summary() function to obtain basic information about support vector classifier.
summary(svmfit) ##It tells us that a linear classifier with cost =10 and that 
##there are 7 support vectors (4, 3), 4 in one class and 3 in the other.
names(summary(svmfit))


##Now we refit the model using a smaller cost parameter.
svmfit =svm(y~., data =dat, kernel ="linear", cost =0.1, scale =FALSE)
plot(svmfit, dat)
svmfit$index
x[svmfit$index, ]
## Now there are more support vectors as a smaller value of cost parameter is used.
##The svm() does not explicitly output the coefficients of the linear decision boundary when support vector classifier is fit, 
##nor does it output the width of the margin.


## The e1071 library includes a built-in tune() function to perform cross validation.
##tune() function performs 10 fold CV by default.
?tune
set.seed(1)
tune.out =tune(svm, y~., data =dat, kernel ="linear", ranges =list(cost =c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
names(tune.out)

##We can access the cross validation errors for each of these models using summary() function.
summary(tune.out)
names(summary(tune.out))
##We see that cost =0.1 results in the lowest cross validation error rate.

##The tune function stores the best model obtained which can be accesses as follows:
bestmod =tune.out$best.model
names(bestmod)
summary(bestmod)


##The predict() function can be used to predict class labels on a set of test observations at any given value of the cost parameter.
?predict.svm
##We first generate test data:
xtest =matrix(rnorm(20*2), ncol =2)
ytest =sample(c(-1, 1), 20, rep=TRUE)
xtest[ytest==1, ] =xtest[ytest==1, ] + 1
testdat =data.frame(x=xtest, y=as.factor(ytest))

##Now we predict the class labels for this test observations.
##Here we use the best model obtained by CV to make our predictions.
ypred =predict(bestmod, testdat)
table(predict =ypred, truth =testdat$y)
##We see that 19 of the test observations are correctly classified.


##Lets repeat the above with a model built using cost parameter of 0.01.
svmfit =svm(y~., data =dat, kernel ="linear", cost =0.01, scale =FALSE)
ypred =predict(svmfit, testdat)
table(predict =ypred, truth =testdat$y) ##In this case 2 observations are misclassified.

#######################

##Now lets consider a case were two classes are linearly seperable. 
##In this case svm() function can find the seperating hyperplane.
##We start be making the two classes in our training data linearly seperable.
x[y==1, ] =x[y==1, ] + 0.5
plot(x, col =(y+5)/2, pch =19) ##Now the classes are barely linearly seperable. Compare with the first plot made.
##Now we fit a support vector classifier using a very large cost function so that no observations are misclassified.
dat =data.frame(x=x, y=as.factor(y))
svmfit =svm(y~., data =dat, kernel ="linear", cost =1e5, scale =FALSE) ##1e5 means 10^5
summary(svmfit) ##Only 3 support vectors.
plot(svmfit, dat)
##See that no training error was made. 
##Also note that the margin is very narrow. Hence the model might perform poorly on test data.

##We will now fit a new model with cost =1.
svmfit =svm(y~., data =dat, kernel ="linear", cost =1, scale =FALSE) ##1e5 means 10^5
summary(svmfit) ##7 support vectors.
plot(svmfit, dat)
##Here we misclassify one training obervation but the margin is much wider with 7 support vectors.
##This model might perform better on test data than the previous one with cost =10^5.


rm (list =ls())
