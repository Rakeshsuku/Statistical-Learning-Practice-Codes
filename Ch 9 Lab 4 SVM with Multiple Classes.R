########################### Ch 09 Lab 4 - SVM with Multiple Classes #############################

library(e1071)
##IF the response is a factor containing more that two levels, then svm() function 
##will perform multi class classification using the one-versus-one approach.
##We explore this by adding a third class of observations to the data used in previous lab.

set.seed(1)
x =matrix(rnorm(200*2), ncol=2)
x[1:100, ] =x[1:100, ] + 2 ##Predictor data for Class I
x[101:150, ] =x[101:150, ] - 2 ##Predictor data for Class I 
y =c(rep(1, 150), rep(2, 50)) ##First 150 samples class I, last 50 samples class II
##Class I distributed at lower and upper parts of the distribution of x with class II at the middle.

##Adding a Third Class.
set.seed(1)
x =rbind(x, matrix(rnorm(50*2), ncol =2))
y =c(y, rep(0, 50))
x[y==0, 2] =x[y==0, 2] + 2
dat =data.frame(x=x, y=as.factor(y))
par(mfrow =c(1,1))
plot(x, col=y+1)
train =sample(200, 100)


##We now fit an SVM to this data.

svmfit =svm(y~., data =dat, kernel ="radial", cost=10, gamma=1)
plot(svmfit, dat)
summary(svmfit)
##Note that the e1071 can also be used to perform SVM regression if the response 
##vector that is passed to svm() is numerical rather than factor.

rm(list =ls())
