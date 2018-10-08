###################### Ch 09 Lab 2 - Support Vector Machines #####################
library(e1071) ##For svm() function.

##To fit a Support Vector Machine model we again use the svm() function in e1071 library.
##To fit an SVM with a polynomial kernel, we use kernel ="polynomial" argument 
##and use degree argument to specify the degree for the polynomial kernal.
##To fit an SVM with a radial kernel, we use kernel ="radial" argument and use
##gamma argument to specify the value of gamma for the radial basis kernel(Equation 9.24).

##First, we generate some data with a non-linear class boundary.
set.seed(1)
x =matrix(rnorm(200*2), ncol=2)
x[1:100, ] =x[1:100, ] + 2 ##Predictor data for Class I
x[101:150, ] =x[101:150, ] - 2 ##Predictor data for Class I 
y =c(rep(1, 150), rep(2, 50)) ##First 150 samples class I, last 50 samples class II
##Class I distributed at lower and upper parts of the distribution of x with class II at the middle.
dat =data.frame(x=x, y=as.factor(y))

##Plotting makes it clear that class boundary is indeed non-linear.
plot(x, col=y)


##Now the dataset is randomly split into test and training sets.
##We then fit the svm model with a radial kernel using the training set.
train =sample(200, 100)
svmfit =svm(y~., data =dat[train, ], kernel ="radial", gamma =1, cost =1)
plot(svmfit, dat[train, ])
summary(svmfit)

##The plot shows that the resulting SVM has a non-linear decision boundary.
##We can see that there are a fair number of training errors. 
##We can reduce the number of training errors if we incease the value of cost. However, this results
## in a more irregular decision boundary and may be overfitting the data.
svmfit =svm(y~., data =dat[train, ], kernel ="radial", gamma =1, cost =1e5)
plot(svmfit, dat[train, ]) ##Highly irregular decision boundary.

##We can use tune() function to perform cross validation to select best choice of gamma and cost.
set.seed(1)
tune.out =tune(svm, y~., data =dat[train, ], kernel ="radial", ranges =list(cost =c(0.1, 1, 10, 100, 1000), gamma =c(0.5, 1, 2, 3, 4)))
summary(tune.out)
##The best model is one with cost =1 and gamma =2
tune.out$best.model
plot(tune.out$best.model, dat[train, ])

##We can now view the test set predictions with the best model selected.
pred =predict(tune.out$best.model, newx =dat[-train, ])
table(true =dat[-train, "y"], pred =pred)
##We can combine the above two lines of code as follows:
##table(true =dat[-train, "y"], pred =predict(tune.out$best.model, newx =dat[-train, ]) )

##39% of the test observations are misclassified by this SVM.
(21+18)/(56+21+18+5)


rm(list =ls())
