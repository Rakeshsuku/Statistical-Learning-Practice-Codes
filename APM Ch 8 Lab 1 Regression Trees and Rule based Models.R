############ APM Ch 8 Lab 1 - Regression Trees and Rule-Based Models ############

##Two widely used implementation for single regression trees are in rpart and 
##party packages.rpart makes splits based on CART methodology using rpart() 
##function whereas the party makes splits based on conditional inference framework 
##using ctree() function.

##The rpart() function has many control arguments that can be accessed through 
##rpart.control argument. Two commonly used parameters that can be accessed 
##through train function are complexity parameter(cp) and maximum node 
##depth(maxdepth). To tune over cp, the method option in train should be set to 
##method = "rpart". To tune over maxdepth, the method option in train should be 
##method = "rpart2".

##Similarly, party package has several control parameters that can be accessed 
##through ctree.control argument. Two parameters that can be tuned via train() 
##function are mincriterion (the statistical criterion that must be met in order 
##to continue splitting - method = "ctree") & maxdepth (maximum depth of the 
##tree - method = "ctree2"). The plot() method in party package cab produce 
##diagrams such as Fig 8.4 in the text. Similarly plots for rpart trees can be 
##produced with partykit package as shown below:

library(AppliedPredictiveModeling)
library(caret)
##install.packages(c("rpart", "party","partykit", "RWeka", "ipred", "do"), dependencies = TRUE)
library(rpart)
library(party)
library(partykit)
library(RWeka)
library(gbm)
library(Cubist)

library(ipred) ##For Bagged trees. 
library(randomForest)

data(solubility)

set.seed(100)
indx = createFolds(solTrainY, returnTrain = TRUE)
ctrl = trainControl(method = "cv", index = indx)

###### Rpart Example (CART Tree) ######

##We fit two CART models to show the splitting process. rpart inly uses formula, 
##hence we putthe predictor and outcome in a single data frame.
trainData <- solTrainXtrans
trainData$y <- solTrainY

rpStump = rpart(y ~., data = trainData, control = rpart.control(maxdepth = 1))
rpSmall = rpart(y ~., data = trainData, control = rpart.control(maxdepth = 2))

set.seed(100)
rpartTune = train(solTrainXtrans, solTrainY, 
                  method = "rpart2", 
                  tuneLength = 25, 
                  trControl = ctrl)

rpartTune
plot(rpartTune, scales = list(x = list(log = 10)))

##Next we get the variable importance for the model. 'competes' is an argument 
##that controls whether splits not used in the tree should be included in the 
##importance calculations.
rpartImp <- varImp(rpartTune, scale = FALSE, competes = FALSE)
rpartImp

##Now, we save the results in a data frame.
testResults =data.frame(obs = solTestY, CART = predict(rpartTune, solTestXtrans))

##We can use partykit package to construct nice plots. For this first convert the 
##rpart object to party object.
rpartTree = as.party(rpartTune$finalModel)
plot(rpartTree)


###### ctree Example (Conditional Inference framework)- Not working ######

cGrid <- data.frame(mincriterion = sort(c(.95, seq(.75, .99, length = 2))))

set.seed(100)
ctreeTune = train(solTrainXtrans, solTrainY, 
                  method = "ctree", 
                  tuneGrid = cGrid, 
                  trControl = ctrl)
ctreeTune
plot(ctreeTune)
testResults$cTree <- predict(ctreeTune, solTestXtrans)

################################################################################
###### Model Trees ######
##The main implementation of Model trees can be found in Weka software suite, but 
##the model can be accessed in R using the RWeka package. There are two different
##interfaces: M5P fits model tree, while M5Rules uses the rule based version. 
##In either case the function work with formula method.

m5tree = M5P(y~., data = trainData) ##For Trees.
m5rules =M5Rules(y~., data = trainData) ##For Rules.

##In the example used in the text, the minimum number of training set points 
##required to create additional splits was raised to 10 (from the default 4). 
##To do this:
m5tree = M5P(y~., data = trainData, 
             control = Weka_control(M = 10))

##The control argument also has option for toggling the use of smoothing and 
##pruning. If full model tree is used, a visualization similar to Fig 8.10 can 
##be created by plot function on the output of M5P().

##We can use train() function to tune this model. train() with method = M5 
##evaluates model trees and rule based versions of the model as well as the use 
##of smoothing and pruning. train() with method = M5Rules evaluates only the 
##rule based version of the model.
set.seed(100)
m5Tune = train(solTrainXtrans, solTrainY,
               method = "M5",
               trControl = trainControl(method = "cv"),
               control = Weka_control(M = 10)) 
##Use an option for M5() to specify the minimum number of samples needed to further 
##split the data.
m5Tune
plot(m5Tune)

m5Tune$finalModel
plot(m5Tune$finalModel)

##To see the rule based model too:
ruleFit = M5Rules(y~., data = trainData, control = Weka_control(M = 10))
ruleFit

################################################################################
######### Bagged Trees #########
##ipred package has 2 functions for bagged trees: bagging() has formula 
##interface, while ipredbagg() has non-formula interface.
baggedTrees = ipredbagg(solTrainY, solTrainXtrans) ##OR
baggedTrees = bagging(y~., data = trainData)
##These function uses rpart function and details about the type of the trees can 
##be specified by rpart.control argument (both for bagging() & ipredbagg()).
##By default, the largest possible tree is created.

##Parallel processing can be used via do packages such as doMC(Not on Windows), 
##doMPI etc.
### WARNING: Be aware of how much memory is needed to parallel
### process. It can very quickly overwhelm the available hardware. The
### estimate of the median memory usage (VSIZE = total memory size) 
### was 9706M for a core, but could range up to 9706M. This becomes 
### severe when parallelizing randomForest() and (especially) calls 
### to cforest(). 
### WARNING 2: The RWeka package does not work well with some forms of
### parallel processing, such as mutlicore (i.e. doMC).
##library(doMC) - NOT FOR WINDOWS
##registerDoMC(5)

set.seed(100)
treebagTune <- train(x = solTrainXtrans, y = solTrainY,
                     method = "treebag",
                     nbagg = 50,
                     trControl = ctrl)

treebagTune

################################################################################
######### Random Forest #########
##The primary implementation of random forest comes from randomForest package.

rfModel = randomForest(solTrainXtrans, solTrainY) ##OR
rfModel = randomForest(y~., data = trainData)

##The two main arguments are mtry for the number of predictors that are randomly
##sampled as candidates for each split and ntree for the number of bootstrap 
##samples. Default for mtry = no. of samples/3 & ntree should be at-least 1000 
##(though default value is 500). Another important variable is importance, 
##variable importance scores are not calculated by default. 
##Setting importance = TRUE will generate these values. 

rfModel = randomForest(solTrainXtrans, solTrainY,
                       importance = TRUE,
                       ntrees = 1000)

##For forest built using conditional inference trees, the cforest function in
## party package can be used. 

##Neither cforest() nor randomForest() can be used with missing data

##train() function can be used to train the model. Tunning over mtry usually 
##yield better performance. Also train can compute performance using standard
##resampling techniques as opposed to Out-of-bag estimate.

mtryGrid <- data.frame(mtry = floor(seq(10, ncol(solTrainXtrans), length = 10)))

set.seed(100)
rfTune <- train(x = solTrainXtrans, y = solTrainY,
                method = "rf",
                tuneGrid = mtryGrid,
                ntree = 1000,
                importance = TRUE,
                trControl = ctrl)
## Use method = "cforest" for cforest models. 

rfTune
plot(rfTune)
rfImp <- varImp(rfTune, scale = FALSE)
rfImp


### Tune the model using the OOB estimates
ctrlOOB <- trainControl(method = "oob")
set.seed(100)
rfTuneOOB <- train(x = solTrainXtrans, y = solTrainY,
                   method = "rf",
                   tuneGrid = mtryGrid,
                   ntree = 1000,
                   importance = TRUE,
                   trControl = ctrlOOB)
rfTuneOOB
plot(rfTuneOOB)


### Tune the conditional inference forests
set.seed(100)
condrfTune <- train(x = solTrainXtrans, y = solTrainY,
                    method = "cforest",
                    tuneGrid = mtryGrid,
                    controls = cforest_unbiased(ntree = 1000),
                    trControl = ctrl)
condrfTune
plot(condrfTune)


##Tune Conditional inference trees with OOB
set.seed(100)
condrfTuneOOB <- train(x = solTrainXtrans, y = solTrainY,
                       method = "cforest",
                       tuneGrid = mtryGrid,
                       controls = cforest_unbiased(ntree = 1000),
                       trControl = trainControl(method = "oob"))
condrfTuneOOB
plot(condrfTuneOOB)

##Variable importance can be accessed using importance() for randomForest() model
##and varimp() for cforest() model. Caret package has a unifying function
##varImp() which can compute variable importance for rpart(), classbagg(), 
##randomForest(), cforest(), gbm() and cubist() models.

################################################################################
######## Boosted Trees ########
##Stochastic gradient boosting machines can be implemented with gbm package.

gbmModel = gbm.fit(solTrainXtrans, solTrainY, distribution = "gaussian") ##OR
gbmModel = gbm(y~., data = trainData, distribution = "gaussian")

##The distribution argument defines the type of loss function that will be
##optimized during boosting. For a continuous response, distribution should be 
##set to gaussian. The number of trees (n.trees), depth of trees 
##(interaction.depth) shrinkage(shrinkage) and the proportion of observations to 
##be sampled (bag.fraction) can all be directly set in the call to gbm.


gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                       n.trees = seq(100, 1000, by = 50),
                       shrinkage = c(0.01, 0.1))
set.seed(100)
gbmTune <- train(x = solTrainXtrans, y = solTrainY,
                 method = "gbm",
                 tuneGrid = gbmGrid,
                 trControl = ctrl,
                 verbose = FALSE)
gbmTune

plot(gbmTune, auto.key = list(columns = 4, lines = TRUE))

gbmImp <- varImp(gbmTune, scale = FALSE)
gbmImp

################################################################################
######## Cubist Trees ########
##The R package Cubist contains the implementation of Cubist model. It does not 
##takes a formula interface.

##To create a single model with a single committee and no instance-based 
##adjustment, we can use the simple code:
cubistMod = cubist(solTrainXtrans, solTrainY) 

##An argument committees fits multiple models, The predict method can be used 
##for new samples:
predict(cubistMod, solTestXtrans)

##The choice of instance based corrections does not need to be made until 
##samples are predicted. The predict function has an argument "neighbors" that
## can take on a single integer value between 0 & 9 to adjust rule based 
##predictions from the training set.
##Once the model is trained, the summary() function generate exact rules that 
##were used as well as the smoothed linear model for each rule.

##The train() function in caret package can tune the model over values of 
##committees and neighbors through resampling.

cbGrid <- expand.grid(committees = c(1:10, 20, 50, 75, 100), 
                      neighbors = c(0, 1, 5, 9))

fix(cbGrid)

set.seed(100)
cubistTune <- train(solTrainXtrans, solTrainY,
                    method = "cubist",
                    tuneGrid = cbGrid,
                    trControl = ctrl)

cubistTune
plot(cubistTune, auto.key = list(columns = 4, lines = TRUE))
cbImp <- varImp(cubistTune, scale = FALSE)
cbImp

##To get a list of all models (230 models) train() can tune: names(getModelInfo()) 

################################################################################
### Session Information

sessionInfo()
rm(list = ls())
q("no")
