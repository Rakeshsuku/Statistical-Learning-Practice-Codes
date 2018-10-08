################# Ch 09 Lab 5 - Application to Gene Expression Data ###############
library(e1071)
library(ISLR) ##For Khan data set

##We now examine the Khan data set, which consists of a number of tissue samples
##corresponding to four distinct types of small round blue cell tumors.
##The data set consists of training data, xtrain and ytrain, and testing data, xtest and ytest.
##The format of the data set is list. See ?Khan

?Khan ##xtrain 2308 gene expressions for 63 subjects. ytrian corresponding tumor type.
##xtest and ytest contains corresponding testing sample info for 20 subjects.
class(Khan) ##Data set is of type list.
names(Khan)
summary(Khan)
str(Khan)
dim(Khan$xtrain) ## 63 x 2308
length(Khan$ytrain) ## 63
dim(Khan$xtest) ## 20 x 2308
length(Khan$ytest) ## 20

table(Khan$ytrain)
table(Khan$ytrain)

##In this data set there are a very large number of features(predictors) relative to number of samples (p>>>n).
##This suggest that we should use linear kernel as additional flexibility that will 
##result from using a polynomial or radial kernel is not necessary. 

dat =data.frame(x =Khan$xtrain, y =as.factor(Khan$ytrain))
out =svm(y~., data =dat, kernel ="linear", cost =10)
summary(out)
table(out$fitted, dat$y)

##We see that there are no training errors. This is not surprising because the 
##large number of variables relative to sample implies that it is easy to find 
##hyperplanes that fully separate the classes. But we are more interested in 
##test set performance.

dat.te =data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te =predict(out, newdata =dat.te)
table(pred.te, dat.te$y)
## We see that cost =10 yields two test set errors on this data.


rm(list =ls())
