################# APM Ch 6 Lab 1 Linear Regression & Its Cousins #################
##Here we analyze the Solubility dataset contained in AppliedPredictiveModeling 
##library.

install.packages("elasticnet")

library(AppliedPredictiveModeling)
library(lattice) ##For xyplot()
library(caret) ##For featureplot()
library(corrplot) ##For corrplot() function.
library(MASS) ##For rlm() function part of robust linear reggresion model.
library(pls) ##For olsr() function
library(elasticnet) ##For enet() function.

data(solubility) ##See ?data to understand how the dataset is loaded
ls(patter ="^solT") ##Names matching the pattern are displayed.

class(solTrainX) ##Dataframe
class(solTrainY) ##Numeric Vector
dim(solTrainX) ##A 951 x 228 dataframe.
dim(solTestX) ##A 316 x 228 dataframe.
fix(solTrainX)
?solTrainX

####Solubility Dataset:
##1267 compounds and a set of more understandable descriptors that fall into one 
##of three groups: 208 binary "fingerprints" that indicate the presence or 
##absence of a particular chemical sub-structure, 16 count descriptors (such as 
##the number of bonds or the number of Bromine atoms) and 4 continuous 
##descriptors (such as molecular weight or surface area).
##Training Set: 951 samples, Test set: 316 Samples
##SolTrainY, SolTestY: Vector of log10 solubility values for training/test sets.

##Alternate versions of the data that have been Box Cox transformed are found in
##dataframes solTrainXtrans & solTestXtrans. We use these datasets for our 
##analysis. Box-Cox transformation is done to mitigate the skewness in 
##numerical predictor variables.


## A random sample of column names in the training set:
set.seed(2)
sample(names(solTrainX), 8) ##FP indicates binary finger prints.

########## Initial Plots ##########
##We start by checking some initial plots of the data.

xyplot(solTrainY ~ solTrainX$MolWeight, type = c("p", "g"),
       ylab = "Solubility (log)",
       main = "(a)",
       xlab = "Molecular Weight")

xyplot(solTrainY ~ solTrainX$NumRotBonds, type = c("p", "g"),
       ylab = "Solubility (log)",
       xlab = "Number of Rotatable Bonds")

##Box Plot:
bwplot(solTrainY ~ ifelse(solTrainX[,100] == 1, 
                          "structure FP100 present", 
                          "structure FP100 absent"),
       ylab = "Solubility (log)",
       main = "(b)",
       horizontal = FALSE)

names(solTrainX)[100] ##FP100 is the 100th Column in the dataset.

##Next we find column names which are not Finger Prints (i.e. continous 
##predictors). grep() will return a list of integers corresponding to column 
##names that contain the pattern "FP".
?grep
Fingerprints =grep("FP", names(solTrainXtrans))
head(Fingerprints)

?featurePlot()
##Part of caret library. Is a shortcut to produce lattice graphs.
featurePlot(solTrainXtrans[, -Fingerprints],
            solTrainY,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))


##Correlation Matrix plot:
##A graphical display of a correlation matrix, confidence interval.
##Part of corrplot library.
##?See ?corrplot
corrplot::corrplot(cor(solTrainXtrans[, -Fingerprints]), 
                   order = "hclust", 
                   tl.cex = .8)

### We used the full namespace above (library:func_name) to call this function 
##because the pls package (also used in this chapter) has a function with the 
##same name.

########## Linear Regression ##########

### Create a control function that will be used across models. We
### create the fold assignments explicitly instead of relying on the
### random number seed being set to identical values.

set.seed(100)
indx =createFolds(solTrainY, returnTrain = TRUE)
ctrl =trainControl(method = "cv", index = indx)
?trainControl
##index argument: A list with elements for each resampling iteration. Each list 
##element is a vector of integers corresponding to the rows used for training 
##at that iteration.
##See str(indx)


##Linear Regression with all Predictors:
### Linear regression model with all of the predictors. This will
### produce some warnings that a 'rank-deficient fit may be
### misleading'. This is related to the predictors being so highly
### correlated that some of the math has broken down.

set.seed(100)
lmTune0 <- train(x = solTrainXtrans, y = solTrainY,
                 method = "lm",
                 trControl = ctrl)

lmTune0 ##Note the warning messages.

##We can also try fitting a linear model using  lm() function.
trainingData =solTrainXtrans
class(trainingData) ##Dataframe
trainingData$Solubility =solTrainY
lmFitAllPredictors =lm(Solubility~., data =trainingData)
summary(lmFitAllPredictors)
##Note that here we do not get any warning of high Predictor correlations, 
##though the issue exists.

rm(lmFitAllPredictors) ##Note that we haven't removed the data frame
##trainingData.


### Now we will fit another model using a set of predictors reduced by 
###unsupervised filtering. We apply a filter to reduce extreme between-predictor
### correlations. Note the lack of warnings.

tooHigh <- findCorrelation(cor(solTrainXtrans), .9)
trainXfiltered <- solTrainXtrans[, -tooHigh]
testXfiltered  <-  solTestXtrans[, -tooHigh]

set.seed(100)
lmTune <- train(x =trainXfiltered, y =solTrainY,
                method ="lm",
                trControl =ctrl)

lmTune

##The type of models that can be fit with train function can be obtained as 
##below:
names(getModelInfo())

##Next we plot the basic diagnostic plots for the above linear model.
xyplot(solTrainY~predict(lmTune),
       ##plot the points (type ='p') and a background grid ('g')
       type =c("p", "g"),
       xlab ="Predicted", ylab ="Observed")
       
xyplot(resid(lmTune)~predict(lmTune),
       type =c("p", "g"),
       xlab ="Predicted", ylab ="Residuals") 

##Note that the above funcion also work with object of type lm.


### Save the test set results in a data frame                 
testResults <- data.frame(obs = solTestY,
                          Linear_Regression =predict(lmTune, testXfiltered))

##We can use the caret function defaultSummary() to estimate the test set 
##performance. To do this we need to form a datafram consisting of 
##observed (column name: obs) and predicted values (pred).
lmTestResults =data.frame(obs =solTestY, pred = predict(lmTune, testXfiltered))
defaultSummary(lmTestResults) ##RMSE =0.761790, Rsquared =0.8669049
rm(lmTestResults) ##Remove the above data frame.

## If we want to fit a robust linear regression model, we can use rlm() 
##function part of MASS library. It employes Huber function.

##Alternatively, we can use train function to fit an rlm model. However, note
## that rlm() does not allow the covariance matrix of the predictors to be
##singular (unlike the lm function). To ensure that the predictors are not
##singular, we will pre-process the predictors using PCA.
set.seed(100)
rlmPCA =train(solTrainXtrans, solTrainY, 
              method ="rlm",
              preProcess ="pca",
              trControl =ctrl)
rlmPCA
summary(rlmPCA)

################################################################################
### Section 6.3 Partial Least Squares

##The pls package has functions for pls and pcr.
##By default, the pls package uses the first Dayal and MacGregor kernal 
##algorithm while the other algorithms can be specified using the method
##argument using values "oscorespls", "simpls", or "widekernalpls".

plsFit =plsr(Solubility~., data =trainingData)
##The number of components can be fixed using ncomp argument - If left 
##unspecified, the maximum number of components will be calculated
##Predictions on new samples can be made using predict() function.
##Predictions can be made for a specific number of components or for several
##values at a time.

predict(plsFit, solTestXtrans[1:5, ], ncomp =1:2)

?plsr
##The plsr() function has option for either K-fold or LOOCV (via validation 
##argument) or the PLS algorithm to use, such as SIMPLS (using method argument).
##There are also several helper functions to extract the PLS components (in the
##function loadings()), the PLS scores (scores()) and other quantities.
##The plot() function has visualiztion for many aspects of the model.
?pls::loadings ##R has another function with the same name loadings().
?scores
dim(loadings(plsFit)) ## 228 x 228
loadings(plsFit)[1:5, 1:5]
dim(scores(plsFit)) ## 951 x 228
scores(plsFit)[1:5, 1:5]
plot(plsFit)

##The train() function can also be used with method values of pls such as 
##"oscorepls", "simpls", or "widekernelpls".

set.seed(100)
plsTune =train(x =solTrainXtrans, y =solTrainY,
               method ="pls",
               tuneGrid =expand.grid(ncomp =1:20),
               trControl =ctrl)

plsTune

##We can also fit a Principal Component Regression (PCR) with train()

set.seed(100)
pcrTune =train(x =solTrainXtrans, y =solTrainY,
               method ="pcr",
               tuneGrid =expand.grid(ncomp =1:35),
               trControl =ctrl)

pcrTune

##We can compare the results of PCR and PLS as follows:
plsResamples <- plsTune$results
plsResamples$Model <- "PLS"
pcrResamples <- pcrTune$results
pcrResamples$Model <- "PCR"
plsPlotData <- rbind(plsResamples, pcrResamples)

xyplot(RMSE ~ ncomp,
       data = plsPlotData,
       #aspect = 1,
       xlab = "# Components",
       ylab = "RMSE (Cross-Validation)",
       auto.key = list(columns = 2),
       groups = Model,
       type = c("o", "g"))

plsImp <- varImp(plsTune, scale = FALSE)
plot(plsImp, top = 25, scales = list(y = list(cex = .95)))

?varImp ##A generic method for calculating variable importance for objects 
##produced by train and method specific methods.

################################################################################
### Section 6.4 Penalized Models

##Ridge regression models can be created using lm.ridge() function in the MASS
##package or the enet() function in the elasticnet package. When calling the
##enet() function, the lambda argument specifies the ridge-regression penalty.
##Note that elasticnet model has both lasso and ridge penalities.
?enet
ridgeModel =enet(x =as.matrix(solTrainXtrans), y =solTrainY,
                 lambda =0.001)

##The lasso penality can be computed effciently fpr many values of the penalty.
##The predict function for the enet objects generate predictions for one or 
##more values of the lasso penalty simulatenously using s and mode arguments.
##For ridge regression we only want a single lasso penality of 0, the full 
##solution. So to produce a ridge-regression solution we define s=1 with 
##mode="fraction". The last option specifies how the amount of penaltization 
##is defined; in this case, a value of 1 corresponds to a fraction of 1 
##i.e. the full solution.

ridgePred =predict(ridgeModel, newx =as.matrix(solTestXtrans),
                   s =1, mode ="fraction", type ="fit")

head(ridgePred$fit)

##To tune over the penalty train() can be used.
ridgeGrid =expand.grid(lambda = seq(0, .1, length = 15))
##Alternatively, ridgeGrid =data.frame(.lambda =seq(0, 0.1, length =15))
?expand.grid

set.seed(100)
ridgeTune =train(x =solTrainXtrans, y =solTrainY,
                   method ="ridge",
                   tuneGrid =ridgeGrid,
                   trControl =ctrl,
                   preProc =c("center", "scale"))

ridgeTune
print(update(plot(ridgeTune), xlab ="Penalty"))

##Next we repeat the above for an elasticnet model.

enetGrid =expand.grid(lambda =c(0, 0.01, .1), 
                        fraction =seq(.05, 1, length =20))
set.seed(100)
enetTune =train(x =solTrainXtrans, y =solTrainY,
                  method ="enet",
                  tuneGrid =enetGrid,
                  trControl =ctrl,
                  preProc =c("center", "scale"))
enetTune
plot(enetTune)
testResults$Enet <- predict(enetTune, solTestXtrans)

##Lasso:
##The lasso model can be estimated using a number of different functions. The 
##lars package contains the lars() function, the elasticnet package has enet()
##function and the glmnet package has glmnet() function. Other packages to fit
##a lasso model or an alternate version of the model are: biglars - for large
##data sets, FLLat - for fused lasso, grplasso - the group lasso, 
##penalized/relaxo - relaxed lasso.

##Here we use the enet() function to fit a lasso model.

enetModel =enet(x =as.matrix(solTrainXtrans), y =solTrainY,
                lambda =0, normalize =TRUE)

##The predictors should be center-scaled. The "normalize =TRUE" argument does 
##this. Lasso penality need not be specified until the time of prediction.
enetPred =predict(enetModel, newx =as.matrix(solTestXtrans),
                  s =0.1, mode ="fraction", type ="fit")
##A list is returned with several items.
names(enetPred)
##The component fit has predicted values.
head(enetPred$fit)
##To determine which predictors are used in the model, the predict method is 
##used with type ="coefficients"
enetCoef =predict(enetModel, newx =as.matrix(solTestXtrans),
                  s =0.1, mode ="fraction", type ="coefficients")

tail(enetCoef$coefficients)
##Note that more than one value of s can be used with predict function to
##generate predictions for more than one models simultaneously.

################################################################################
##Session Information
sessionInfo()
rm(list =ls())
