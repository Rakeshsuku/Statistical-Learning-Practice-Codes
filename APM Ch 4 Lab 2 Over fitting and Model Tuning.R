##################### APM Ch 4 Lab 2 Over Fitting and Model Tuning #######################
##install.packages("kernlab")

library(AppliedPredictiveModeling)
library(caret)
library(kernlab) ##For sigest() function.

############## Determination of Tuning Parameters ##############

##To choose tuning parameters a set of candidate values are evaluated using 
##different resamples of the data. A performance profile is the created to
##understand the relationship between performance and parameter value.

##The tune() function in e1071 package can evaluate 4 types of models across a range
##of parameters. errorest() function in ipred package can resample single models.
##train() function in caret package has built-in modules for 144 models and includes
##capabilities for different resampling methods, performance measures and algorithm
##for choosing the best model from the profile.

##Here we use train() function to tune the radial basis SVM model described in Section 4.6
##A radial basis SVM model has an additional parameter sigma which impacts the 
##smoothing of the decision boundary. However, an analytical formula can be used
##to get reasonable estimates of sigma. The train() function uses this method. Hence
##only the Cost parameter of the SVM need to be estimated by model tuning.

##The training set samples and outcome are contained in R dataframe called GermanCreditTrain.

data(GermanCredit)
##The chapters directory of AppliedPredictiveModeling package contains the code 
##for creating the training set and test set. These data are contained in the 
##dataframes GermanCreditTrain and GermanCreditTest respectively.
##The outcome classes are stored in the dataframe column Class.

dim(GermanCredit) ## a 1000 x 62 dataframe.
str(GermanCredit)
summary(GermanCredit)
fix(GermanCredit)
?GermanCredit

## First, remove near-zero variance predictors then get rid of a few predictors 
## that duplicate values. For example, there are two possible values for the 
## housing variable: "Rent", "Own" and "ForFree". So that we don't have linear
## dependencies, we get rid of one of the levels (e.g. "ForFree")

?nearZeroVar ##nearZeroVar() requires a numeric vector or matrix or a dataframe 
##with all numeric data as input. GermanCredit data meests this criteria except
##for the class variable which is a factor with two levels(Good/Bad).
##See str(GermanCredit)
GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)] 

## Removes redundant info as described above (e.g. "ForFree").
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL


## Split the data into training (80%) and test sets (20%)
set.seed(100)
inTrain <- createDataPartition(GermanCredit$Class, p = .8)[[1]]
GermanCreditTrain <- GermanCredit[ inTrain, ]
GermanCreditTest  <- GermanCredit[-inTrain, ]

## The model fitting code shown in the computing section is fairly
## simplistic.  For the text, we estimate the tuning parameter grid
## up-front and pass it in explicitly. This generally is not needed,
## but was used here so that we could trim the cost values to a
## presentable range and to re-use later with different resampling
## methods.

set.seed(231)
?sigest
##sigest estimates the range of values for the sigma parameter which would return 
##good results when used with a Support Vector Machine (ksvm). It returns a 
##vector of length 3 defining the range (0.1 quantile, median and 0.9 quantile) 
##of the sigma hyperparameter.
sigDist <- sigest(Class ~ ., data = GermanCreditTrain, frac = 1)
sigDist
class(sigDist)
svmTuneGrid <- data.frame(sigma = as.vector(sigDist)[1], C = 2^(-2:7))

##Optional parallel processing can be used. See chapters directory of APM 
##package for code.

set.seed(1056)
svmFit <- train(Class ~ .,
                data = GermanCreditTrain,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneGrid = svmTuneGrid,
                trControl = trainControl(method = "repeatedcv", 
                                         repeats = 5,
                                         classProbs = TRUE))
?train.svm
##By default basic bootstrap is used to calculate performance measure. Here
##we have explicitly specified to use repeated 10 fold CV with 5 repeats.
## classProbs = TRUE was added since the text was written

## Print the results
svmFit ##

## A line plot of the average performance. The 'scales' argument is actually an 
## argument to xyplot that converts the x-axis to log-2 units.
plot(svmFit, scales = list(x = list(log = 2)), main="Repeated 10-Fold CV")

## Test set predictions
predictedClasses <- predict(svmFit, GermanCreditTest)
str(predictedClasses)

## Use the "type" option to get class probabilities
predictedProbs <- predict(svmFit, newdata = GermanCreditTest, type = "prob")
head(predictedProbs)

## Now we fit the same model using different resampling methods. The main 
##syntax change is the control object.
##Using 10 fold CV:
set.seed(1056)
svmFit10CV <- train(Class ~ .,
                    data = GermanCreditTrain,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneGrid = svmTuneGrid,
                    trControl = trainControl(method = "cv", number = 10))
svmFit10CV
plot(svmFit10CV, scales = list(x = list(log = 2)), main="10 Fold CV")

##Leave One Out CV: DO NOT RUN - Takes TOOOOOOOOO MUCH TIME!!!
##set.seed(1056)
##svmFitLOO <- train(Class ~ .,
##                   data = GermanCreditTrain,
##                   method = "svmRadial",
##                   preProc = c("center", "scale"),
##                   tuneGrid = svmTuneGrid,
##                   trControl = trainControl(method = "LOOCV"))
##svmFitLOO
##plot(svmFitLOO, scales = list(x = list(log = 2)), main="LOOCV")

##Leave Group Out CV:
set.seed(1056)
svmFitLGO <- train(Class ~ .,
                   data = GermanCreditTrain,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   tuneGrid = svmTuneGrid,
                   trControl = trainControl(method = "LGOCV", 
                                            number = 50, 
                                            p = .8))
svmFitLGO
plot(svmFitLGO, scales = list(x = list(log = 2)), main="Leave Group Out CV")

##Bootstrap:
set.seed(1056)
svmFitBoot <- train(Class ~ .,
                    data = GermanCreditTrain,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneGrid = svmTuneGrid,
                    trControl = trainControl(method = "boot", number = 50))
svmFitBoot
plot(svmFitBoot, scales = list(x = list(log = 2)), main="Bootstrap")

##Bootstrap .632:
set.seed(1056)
svmFitBoot632 <- train(Class ~ .,
                       data = GermanCreditTrain,
                       method = "svmRadial",
                       preProc = c("center", "scale"),
                       tuneGrid = svmTuneGrid,
                       trControl = trainControl(method = "boot632", 
                                                number = 50))
svmFitBoot632
plot(svmFitBoot, scales = list(x = list(log = 2)), main="Bootstrap 632")

##validate() - Design package can also estimate performance via resampling. 

################################################################################
############## Section 4.8 Choosing Between Models ##############

##Here we contrast the SVM model built above(1st one with Repeated 10 fold CV) 
##with a logistic regression model as in section 4.8.

set.seed(1056)
logisticReg =train(Class~.,
                   data =GermanCreditTrain,
                   method ="glm",
                   trControl =trainControl(method ="repeatedCV",
                                           number =10,
                                           repeats =5))

logisticReg

##To compare these two models based on their cross-validation statistics, the 
##resamples function can be used with models that share a common set of 
##resampled data set. Since the random number seed was initialized prior to 
##running the SVM & Logistic models, paired accuracy measurements exists for
##each data set. First, we create a resamples object from the models.
?resamples
resamp =resamples(list(SVM =svmFit, Logistic =logisticReg))
summary(resamp)

##The summary() indicates that the performance distributions are very similar.
##The NA columns correspond to cases where the resampled models failed usually 
##due to numerical issues. The resample class has several methods for visualizing
##the paired values. To assess the possible difference between the models, the
##diff() method can be used.
modelDifference =diff(resamp)
summary(modelDifference)


################################################################################
##Session Information
sessionInfo()
rm(list =ls())
