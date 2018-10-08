################## APM Ch 7 Lab 1 Nonlinear Regression Models ##################

#### Neural Networks: ####
##R has a number of package to fit neural networks. Example: nnet, neural, RSNNS.
##We use nnet model here as it supports the basic neural network model and
##weight decay and has simple syntax. The RSNNS package supports a wide array
##of neural networks.

##The nnet() function in nnet package or avNNet() -with model averaging, function
##in caret package can be used to fit a Neural Network. However, here we use the
##train() function to tune over the number of hidden units and the weight decay.

##We analyze the Solubility dataset analyzed in the previous chapter.

##install.packages(c("kernlab"))

library(AppliedPredictiveModeling)
library(caret)
library(nnet)
library(earth)
library(kernlab)

data(solubility)

### Create a control funciton that will be used across models. We
### create the fold assignments explictily instead of relying on the
### random number seed being set to identical values.
set.seed(100)
indx =createFolds(solTrainY, returnTrain =TRUE)
ctrl =trainControl(method ="cv", index =indx)

##Next we remove predictors to ensure that the maximum absolute pairwise 
##correlation between the predictors is less than 0.75. We use findCorrelation()
##function for this. It applies the algorithm discussed in APM Sec 3.5 (Page 56).

tooHigh =findCorrelation(cor(solTrainXtrans), cutoff =0.75)
trainXnnet =solTrainXtrans[, -tooHigh]
testXnnet =solTestXtrans[, -tooHigh]

##
nnetGrid <- expand.grid(decay = c(0, 0.01, .1), 
                        size = c(1, 3, 5, 7, 9, 11, 13), 
                        bag = FALSE)

##set.seed(100)    ######DOES NOT WORK - NEED REVIEW######
##nnetTune =train(x =trainXnnet, y =solTrainY,
##                method ="avNNet",
##                tuneGrid =nnetGrid,
##                trControl =ctrl,
##                preProc =c("center", "scale"),
##                linout =TRUE,
##                trace =FALSE,
##                MaxNWts =10*(ncol(trainXnnet) + 1) + 10 + 1,
##                maxit =500)


#### Multivariate Adaptive Regression Spline####

##The most extensive implementation of MARS models are in earth package.
##MARS model using nominal forward pass and purning step can be fit by:
marsFit =earth(solTrainXtrans, solTrainY)
marsFit
##Note that this model used internal GCV technique for model selection.
summary(marsFit) ##h() in the output is the hinge function.

##The plotmo() function in earth package can be used to make plots for MARS 
##models similar to Figure 7.5 in the text(APM).
?plotmo
plotmo(marsFit)

##Also note that we can use train() function to tune the model using external
##resampling. The following code produced Figure 7.4 in the text.

marsGrid =expand.grid(degree =1:2, nprune =2:38)
set.seed(100)
marsTune =train(x =solTrainXtrans, y=solTrainY,
                method ="earth",
                tuneGrid =marsGrid,
                trControl =trainControl(method ="cv"))

marsTune
summary(marsTune)
head(predict(marsTune, solTestXtrans))
plot(marsTune)

##There are two functions that measure the importance of variables in MARS 
##model. The evimp() in earth package and varImp() in caret package; although 
##the latter call the former.
testResults =data.frame(obs = solTestY, 
                        MARS = predict(marsTune, solTestXtrans))

names(testResults)[2] ="MARS"
head(testResults)
marsImp =varImp(marsTune, scale =FALSE)
plot(marsImp, top =25)


####Support Vector Machines####

##The svm() function in e1071 package has an interface to LIBSVM library for 
##regression. A more comprehensive implementations of SVM models for regression 
## kernlab package. In kernlab package ksvm() function is available for a large 
##number of kernel functions. The radial basis function is the default.

##IF approach values of Cost and kernel parameters are know, the model can be 
##fit as:
?ksvm
set.seed(100) -##BELOW CODE DOES NOTE Need Rework.
svmFit =ksvm(x =as.matrix(solTrainXtrans), y =solTrainY,
             kernel ="rbfdot", kpar ="automatic",
             C =1, epsilon =0.1)
svmFit
head(predict(svmFit, solTestXtrans))
head(testResults$obs)
summary(svmFit)
str(svmFit)
##The function automatically uses the analytical approach to estimate sigma.  
##Since y is a numeric vector, the function fits a regression model.
## Other kernel functions can be used including polynomial (kernel ="polydot")
## and linear (kernel ="vanilladot").

  
##If the parameter values are unknown, they can be estimated through resampling.
##In the train() function, arguments method ="svmRadial"/"svmLinear"/"svmPoly"
##can be used to fit radial/linear/polynomial kernel SVM models.
svmTuned =train(solTrainXtrans, solTrainY,
                method ="svmRadial",
                preProc =c("center", "scale"),
                tuneLength =14,
                trControl =trainControl(method ="cv"))


##The tuneLength argument will use the default grid of 14 cost values between 
## 2^(-2), 2^(-1), ..., 2^(11). Again sigma is estimated analytically by default. 

svmTuned
##The subobject "finalModel" contains the model created by the ksvm function.
svmTuned$finalModel
##Here we see that the model used 637 training set data points as support vectors.
##kernlab package has an implementation of RVM model for regression in rvm() 
##function. The syntax is similar to ksvm()


####K-Nearest Neighbors####
##The knnreg() function in the caret package fits the KNN regression model. 
##train() function tunes the model over K.

##First we remove a few sparse and unbalanced fingerprints from the predictor 
##set.
knnDescr =solTrainXtrans[, -nearZeroVar(solTrainXtrans)]
set.seed(100)
knnTune =train(knnDescr, solTrainY,
               method ="knn",
               preProc =c("center", "scale"),
               ##Center Scaling will occur for new predictions too.
               tuneGrid =data.frame(.k =1:20),
               trControl =trainControl(method ="cv"))

##When predicting new samples using this object, the new samples are 
##automatically centered and scaled using the values determined by the 
##training set. 

################################################################################
##Session Information
sessionInfo()
rm(list =ls())
