##################### APM Ch 4 Lab 1 - Over Fitting and Model Tuning #######################

library(AppliedPredictiveModeling)
library(caret)

############## Data Splitting ##############

##The two class model shown in Fig 4.1 is available in the Package 
## AppliedPredictiveModeling.
data(twoClassData)

##The predictors for the data set are stored in the data frame "predictors". There 
##are 208 samples. The outcome classes are contained in a factor vector "classes".
str(predictors)
summary(predictors)
str(classes)
summary(classes)

##The base R function sample() can be used to simple random splits of the data.
##To create stratified random splits of the data (based on the classes), the 
##createDataPartition() function in caret package can be used. The percentage of
##data that will be allocated to the training set should be specified.
set.seed(1)
trainingRows =createDataPartition(classes, p =0.8, list =FALSE)
##By default, the number are returned as a list, with the "list =FASLE" argument
##a matrix of row numbers is returned.
head(trainingRows)
trainPredictors =predictors[trainingRows, ]
trainClasses =classes[trainingRows]
testPredictors =predictors[-trainingRows, ]
testClasses =classes[-trainingRows]
str(trainPredictors) ##167 Observations
str(testPredictors) ##41 observations

############## Resampling ##############

##The caret function createDataPartition() can be used to create repeated 
##training/test splits with an additional argument "times".
set.seed(1)
repeatedSplits =createDataPartition(trainClasses, p =0.80, times =3)
str(repeatedSplits)

##Similarly caret library has functions createResamples() - for bootstrapping,
##createFolds() - for K fold CV and createMultiFolds() for repeated CV

##To create 10-fold CV:
set.seed(1)
cvSplits =createFolds(trainClasses, k =10, returnTrain =TRUE) 
##Above function returns training set for each fold.
?createFolds
str(cvSplits) ##List of 10 integer vectors.
##To get the first set of row number:
fold1 =cvSplits[[1]]
##To get first 90% of the data:
cvPredictors1 =trainPredictors[fold1, ]
cvClasses1 =trainClasses[fold1]
nrow(trainPredictors) ##167 samples
nrow(cvPredictors1) ##151 samples

############## Basic Model Building in R ##############
##Now we will fit a KNN model to this dataset. There ar multiple functions in R
##that can do this: knn() function in MASS package, ipredknn() function in ipred
##package, knn3() function in caret package. knn3() function can produce the 
##porportion of neighbours for each class.

##Here, we use knn3() to build a 5 Nearest Neighbour model. 
##knn3() take a matrix interface instead of a formula interface.
trainPredictors =as.matrix(trainPredictors)
knnFit =knn3(x =trainPredictors, y =trainClasses, k =5)
knnFit

##We can use the predict() function to predict classes for new samples.
testPredictions =predict(knnFit, newdata =testPredictors, type ="class")
head(testPredictions)
str(testPredictions)

##Summarizes the test set predictions against observed values.
table(testPredictions, testClasses) 


################################################################################
##Session Information
sessionInfo()
rm(list =ls())
