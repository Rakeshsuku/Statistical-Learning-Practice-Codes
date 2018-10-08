#################### Using Validation set and Cross validation for model selection ###########################
library(ISLR)
library(leaps)  ##For regsubsets () function.

data(Hitters)
dim(Hitters)
summary(Hitters)
str(Hitters)
fix(Hitters)
names(Hitters)

sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters)
sum(is.na(Hitters$Salary))
pairs(Hitters) ##Display scatter plot of all variables.
cor(Hitters[, -c(14,15,20)]) ##Computes correlation matrix after removing factor variables.

## For Validation set error or Cross validation error to be accurate, 
## we should only use training set data for model selection. 

set.seed(1)
train =sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test =!train
regfit.best=regsubsets (Salary~., data=Hitters[train, ], nvmax=19)
## The regfit.best now contains the best model for a given number of predictors up till 19.

test.mat =model.matrix(Salary~., data=Hitters[test, ])
## The model.matrix () function is used in many regression packages for building an "X" matrix from data.

## We now compute the test MSE for each of the models in the regfit.best
val.error = rep (NA, 19)
for (i in 1:19)  {
        coefi=coef (regfit.best, id=i)  ## See coef (regfit.best, id=3) for example.
        pred=test.mat[, names(coefi)]%*%coefi   ## Matrix multiplication
        val.error[i] = mean((Hitters$Salary[test]-pred)^2)        
}
val.error  ##Displays validation error for all the models.
plot(val.error) ##Plots the validation set error for best models of different sizes.
which.min(val.error)  ##10th model gives the lowest test MSE
val.error[which.min(val.error)]
coef(regfit.best, 10) ##Gives the variables and coefficients in the 10th model.

## Now we perform best subset selection on the full data set and select the best ten-variable model.
## It is important to use the full data set(training set + test set) to obtain more accurate coefficient estimates.
regfit.best =regsubsets(Salary~., data =Hitters, nvmax =19)
coef(regfit.best, 10) ##Coefficients of the best 10 variable subset.

## The above procedure was a little tedious as there is no predict function for regsubsets (). 
## Since we will be needing such a function again, we create a function of our own for the purpose.
predict.regsubsets =function(object, newdata, id, ...) {
        form =as.formula(object$call[[2]])
        mat =model.matrix(form, newdata)
        coefi =coef(object, id=id)
        xvars =names(coefi)
        mat[, xvars]%*%coefi  ## Matrix multiplication
}
## The "form =as.formula..." code is used to extract the formula used in the call to regsubsets().
## See names(regsubsets_object)

################## Best subset selection using Cross Validation ######################

##We first create a vector that allocates each observation to one of the k =10 folds,
## and we create a matrix in which we will store the results.

k =10 ## 10 fold cross validation
set.seed(1)
folds=sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors=matrix(NA, k, 19, dimnames=list(NULL, paste(1:19))) 
##Creates a 10(k) row 19(p) column matrix 
##to store error values for each k subsets of a p variable model. 
dim(cv.errors)

## Now we write a loop to perform the cross validation.
for (j in 1:k)  {
        best.fit =regsubsets(Salary~., data=Hitters[folds!=j, ], nvmax=19)
        for(i in 1:19)  {
                pred =predict(best.fit, Hitters[folds==j, ], id=i) ##Predict function used here is the one defined above. 
                ## See test codes below for better understanding.
                cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
        }
}

####################Test Codes for better understanding - BEGIN####################

##Best subset fit on all data except the first fold(j =1)
best.fit.test =regsubsets(Salary~., data=Hitters[folds!=1, ], nvmax=19) 

## Prediction made on best subset for the data in the first fold using the model with all j=4 variables.
pred.test =predict(best.fit.test, Hitters[folds==1, ], id=4)

##CV error computed for the first fold.
cv.errors.test =mean((Hitters$Salary[folds==1]-pred.test)^2)
cv.errors.test
cv.errors[1, 4]

##Check defined predict function code:
best.fit.test$call[[2]]
best.fit.test$call[[1]]
best.fit.test$call[[3]]
best.fit.test$call[[4]]
best.fit.test$call
names(best.fit.test)

X =model.matrix(as.formula(best.fit.test$call[[2]]), Hitters[folds==1, ])
X
## Repeat for different values of j(4 is used here) till 19(model with all variables)

rm("X", "best.fit.test", "cv.errors.test", "pred.test")
####################Test Codes for better understanding - END####################


## This (code above Test code) has given us a 10X19 matrix of which the (i,j)th element corresponds to the test MSE for ith cross-validation fold
## for the best j-variable model. We use the apply () function to average over the columns of this matrix in order to obtain a vector 
## for which the jth element is the cross validation error for the j variable model.

mean.cv.error = apply(cv.errors, 2, mean) 
##2 indicates that mean() should be applied over columns of cv.errors
mean.cv.error
par(mfrow=c(1,1))
plot(mean.cv.error, type='b')
## We see that cross validation selects a 11 variable model. We now perform the best subset selection on the full data set in order to 
## obtain the 11 variable model.
reg.best=regsubsets (Salary~., data=Hitters, nvmax=19)
coef (reg.best, 11)

rm(list=ls())
