#################### K-Nearest Neighbour #####################
library (ISLR)
library (MASS)
library (class)
## We use knn () function in class library to fit a KNN model. The knn () function takes 4 inputs. see text page 163.

attach (Smarket)
##We will now split the data to training set and test set and fit the model on training set and estimate the prediction accuracy on the test set.
train = (Year<2005) ##Year contains the Year for all 1250 entries in the Smarket data set.
##train now is logical vector. 
Smarket.2005 = Smarket[!train,] ##Test data set.
dim (Smarket.2005) ##Dimensions of the test data set
Direction.2005 = Direction [!train]

train.X = cbind (Lag1, Lag2)[train,] ##Joins Lag1 and Lag2 variables for training data and assigns it to train.X
test.X = cbind (Lag1, Lag2)[!train,]
train.Direction = Direction[train]

## Now we must set seed to ensure reproducibility when R randomly tries to break ties as nearest neighbours.
set.seed (1)
knn.pred = knn (train.X, test.X, train.Direction, k=1)
table (knn.pred, Direction.2005)
mean (knn.pred == Direction.2005) ##The result for K=1 is not good as only 50% observations are correctly classified.

## Below is an analysis using K = 3.
knn.pred = knn (train.X, test.X, train.Direction, k=3)
table (knn.pred, Direction.2005)
mean (knn.pred == Direction.2005)
## Further increasing k (not done here) is found to avail no additional improvement.
