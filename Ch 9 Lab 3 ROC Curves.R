########################### Ch 09 Lab 3 - ROC Curves #############################

##The ROCR package can be used to produce ROC curves such as those in Fig 9.10 & 9.11.

##install.packages("ROCR")
library(e1071) ##For svm() function
library(ROCR)

##First, we generate some data with a non-linear class boundary.
set.seed(1)
x =matrix(rnorm(200*2), ncol=2)
x[1:100, ] =x[1:100, ] + 2 ##Predictor data for Class I
x[101:150, ] =x[101:150, ] - 2 ##Predictor data for Class I 
y =c(rep(1, 150), rep(2, 50)) ##First 150 samples class I, last 50 samples class II
##Class I distributed at lower and upper parts of the distribution of x with class II at the middle.
dat =data.frame(x=x, y=as.factor(y))
plot(x, col=y)
train =sample(200, 100)


##We first write a short function to plot an ROC curve given a vector containing 
##a numerical score for each observation, pred and a vector containing the class 
##label for each observation i.e. the truth.
rocplot =function(pred, truth, ...) {
  predob =prediction(pred, truth)  ##See ?prediction
  perf =performance(predob, "tpr", "fpr")  ##See ?performance
  plot(perf, ...)
}

##SVM and support vector classifiers output class labels for each observations. However, it is also possible
## to obtain fitted values which are numerical scores used to obtain the class label.
##For an SVM with a non linear kernel, the equation that yields  fitted values is given by Eq 9.23 in ISLR.
##The sign of the fitted value determines which class the observation is assigned to.

##In order to obtain fitted values for a given SVM model we use decision.values =TRUE argument when fitting svm. 
##Then the predict() function will output the fitted values.

svmfit.opt =svm(y~., data =dat[train, ], kernel ="radial", gamma =2, cost =1, decision.values =T)
fitted =attributes(predict(svmfit.opt, dat[train, ], decision.values =TRUE))$decision.values
## See: names(attributes(predict(svmfit.opt, dat[train, ], decision.values =TRUE)))

##Now we plot the ROC plot.
par(mfrow =c(1,2))
rocplot(fitted, dat[train, "y"], main="Training Data")

## By increasing gamma we can try to further increase the accuracy.
svmfit.flex =svm(y~., data =dat[train, ], kernel ="radial", gamma =50, cost =1, decision.values =T)
fitted =attributes(predict(svmfit.flex, dat[train, ], decision.values =TRUE))$decision.values
rocplot(fitted, dat[train, "y"], add=T, col ="red") ##Adds red plot.

## The above ROC plot is on training data. 
##When we computes ROC for test data, the model with gamma=2 appears to perform better.

fitted =attributes(predict(svmfit.opt, dat[-train, ], decision.values =TRUE))$decision.values
rocplot(fitted, dat[-train, "y"], main="Test Data")
fitted =attributes(predict(svmfit.flex, dat[-train, ], decision.values =TRUE))$decision.values
rocplot(fitted, dat[-train, "y"], add=T, col ="red") ##Adds red plot.


rm(list =ls())
