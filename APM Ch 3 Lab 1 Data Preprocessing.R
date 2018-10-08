##################### APM Ch 3 Lab 1- Data Preprocessing #######################

##install.packages("AppliedPredictiveModeling")
##install.packages("caret")
##install.packahes("impute") ##impute library was removed from CRAN.
##install.packages("corrplot")
##install.packages("subselect")


library(AppliedPredictiveModeling)
library(e1071) ##For skewness() function.
library(caret) ##For BoxCoxTrans() function
##library(impute) ##For impute.knn() - This function is not used as a short cut 
##is available. Also impute library is now removed from CRAN.
library(corrplot) ##For corrplot() function.

apropos("confusion") ##It will search any loaded R package for a given term.
?apropos ##find() is a different UI for the same function.
RSiteSearch("confusion", restrict ="functions") ##Search online for a given term and open results in a browser.
?RSiteSearch


data(segmentationOriginal) ##Raw segmentation data set is contained in AppliedPredictiveModeling package.
summary(segmentationOriginal)
str(segmentationOriginal)
names(segmentationOriginal)

##Variable Cell identified each cell and a factor vector Class indicated which cells are well segmented.
##The variable Case identified which cells are originally used for training and test sets.
##Here we are focused on training set samples, hence the test set samples are filtered out.
segData =subset(segmentationOriginal, Case =="Train")

##We now save Class, Case and Cell fields to a separate vector and then remove them from segData.
cellID =segData$Cell
class =segData$Class
case =segData$Case
segData =segData[, -(1:3)]

##The original data contained several "status" columns which are binary versions of the predictors.
##To remove these, we find the column names containing "Status" and remove them.
fix(segData)
statusColNum =grep("Status", names(segData))
statusColNum
?grep ##grep, grepl, regexpr, gregexpr and regexec search for matches to argument 
##pattern within each element of a character vector: they differ in the format of 
##and amount of detail in the results
segData =segData[, -statusColNum]

#################### Transformations ####################

####### Skewness & Boxcox transformation #######

?skewness
## The skewness() function in e1071 package computes skewness statistic. Example:
skewness(segData$AngleCh1) ##Gives a skewness statistic of -0.0242

##Since all variables are numeric, apply function can be used to compute skewness statistic for all variables together.
skewValues =apply(segData, 2, skewness)
head(skewValues) ##Returns first few values of skewValues.
##Using these values, variables can be prioritized for visualization.

##The boxcox() function in MASS library can be used to compute lamda values for boxcox transformation.
##However, this function does not perform the boxcox transformation.
##We use the BoxCoxTrans() in caret library to compute lambda values and apply the transformation.
Ch1AreaTrans =BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans

head(segData$AreaCh1) ##Original Data
predict(Ch1AreaTrans, head(segData$AreaCh1)) ##After transformation
(819^(-.9) - 1)/(-.9) ##Give the first element after transformation. lambda = -0.9
##Another caret function preProcess applies this transformation to a set of predictors.

####### Principal Component Analysis #######

##Base function prcomp() can be used to perform PCA.
##We center-scale data prior to PCA.
pcaObject =prcomp(segData, center =TRUE, scale =TRUE)
##To calculate the % of variance explained by each component:
percentVariance =((pcaObject$sd^2) / sum(pcaObject$sd^2))*100
head(percentVariance)
##The transformed values are stored in pcaObject as sub-objects called x:
head(pcaObject$x[ , 1:5])
##Another sub-object called rotation stores the variable loadings where row corresponds
##to predictor variables and columns are associated with components.
head(pcaObject$rotation[, 1:3])


####### spatialSign transformation for Outliers #######

## The caret package function spatialSign() contains functionality for spatialSign transformation.
##The syntax would be spatialSign(segData). We do not use the function here as a shortcut is available.


####### Imputation for missing values #######

##The impute library function imput.knn() can be used to impute missing values for predictors.
##There are no missing values in the segmentation data set.


####### Shortcut for a serise of transformation to a group of variables - preProcess() #######

## preProcess() function, part of caret library, can be used to apply a series of transformations to a group of predictor variables.
##It can transform, center-scale, impute values, and apply spatial sign transformation and perform feature extraction.
##After calling the preProcess function, the predict method applies the results to a set of data.

##To Box-Cox transform, center, scale and execute PCA, the syntax is:
trans =preProcess(segData, method =c("BoxCox", "center", "scale", "pca"))
trans
transformed =predict (trans, segData) ##Applies the transformation
##Note that these values are different than the previous PCA components since they are transformed prior to PCA.
head(transformed[, 1:5])

##The order in which the transformation are applied are:
## Transformation -> Centering -> Scaling -> Imputation -> Feature Extraction -> Spatial sign.

##Many of the modeling functions have option to center and scale prior to modeling.
##For example, when using the train() function, there is an option to use preProcess prior to modeling within the resampling iteration.


####### Filtering #######

## The caret package function nearZeroVar() will return the column numbers of any 
##predictors that fulfill the conditions of near zero variance predictor(see APM Sect 3.5)
nearZeroVar(segData) ##For segmentation data set there are no problematic predictors.

##cor() function calculates the between predictor corelation of all combination of predictors.
correlations =cor(segData)
dim(correlations) ##a 58 x 58 matric.
correlations[1:4, 1:4]

## The corrplot() function in the corrplot library can be used to visually examine 
##the correlation structure of the data. It has option to reorder predictors in a way that 
##reveals clusters of highly correlated predictors.
corrplot(correlations, order ="hclust")
##The size and color of the points are associated with the strength of the correlation.

##We can use pairs() function to create a correlation matrix plot.
pairs(segData[1:10, 1:10])

##The caret library function findCorrelation applies the logic in sec 3.5 for a given threshold of pairwise correlations.
##The function returns column numbers denoting the predictors that are recommended for deletion.
highCorr =findCorrelation(correlations, cutoff =0.75)
length(highCorr) ##32 columns recommended for deletion.
head(highCorr)
filteredSegData =segData[, -highCorr]
##There are also several functions in the subselect package that can accomplish the same goal.


####### Creating Dummy Variables #######

##Some models like tree based model might benefit from creating dummy variables for all levels of the factor predictor variable.
##We illustrate this using cars data set in caret library.
##The objective of the model was to predict the price of the car based on known charateristics.

data(cars)
type <- c("convertible", "coupe", "hatchback", "sedan", "wagon")
cars$Type <- factor(apply(cars[, 14:18], 1, function(x) type[which(x == 1)]))
set.seed(1)
carSubset <- cars[sample(1:nrow(cars), 20), c(1, 2, 19)]
head(carSubset)
levels(carSubset$Type)

##We use dummyVars() function, part of caret library, to determine the encodings for the predictors.
simpleMod =dummyVars(Price~Mileage+Type, data =carSubset, levelsOnly =TRUE) 
simpleMod
names(simpleMod)
##levelsOnly argument  removes variable name from the column name.
##To generate dummy variables for the training set or any new samples, the 
##predict() method can be used with dummyVars object.
predict(simpleMod, head(carSubset))
##Note that the type field was expanded into 5 variables for 5 factor levels.

##To fit a more complex model we assume there is an interaction effect of mileage and car type.
withInteraction =dummyVars(Price~Mileage + Type + Mileage:Type, data=carSubset, levelsOnly =TRUE)
withInteraction
predict(withInteraction, head(carSubset))


rm(list =ls())
