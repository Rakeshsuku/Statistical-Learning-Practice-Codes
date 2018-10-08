#####################Ch 10 Lab 2 - K-Means Clustering ###########################

##We start with a simulated data set with two clusters.
set.seed(2)
x =matrix(rnorm(50*2), ncol =2)
x[1:25, 1] =x[1:25, 1] +3
x[1:25, 2] =x[1:25, 2] -4

##We now perform K-means clustering with K =2.
?kmeans
km.out =kmeans(x, 2, nstart =20)
##The cluster assignments of the 50 observations can be seen by
km.out$cluster ##kmeans() function separated the data into two sets by itself.
names(km.out)

##We can now plot the data with each observation colored according to its cluster assignment.
plot(x, col =(km.out$cluster +1), main ="K-Means Clustering Results with K =2", xlab="", ylab ="", pch =20, cex =2)

##Here the observation can be easily plotted as they are 2 diamensional.
##If there are more than 2 variables, then we can perform a PCA and plot the first two Principal Components.
##Here we knew the number of cluster and hence performed K-Mean Clustering with K =2.

## Now we try the same analysis with K =3 as we would not know the number of 
##clusters in the data set in real practice.
set.seed(4)
km.out =kmeans(x, 3, nstart =20)
km.out
plot(x, col =(km.out$cluster +1), main ="K-Means Clustering Results with K =3", xlab="", ylab ="", pch =20, cex =2)

## To run K Means Clustering in R with multiple initial cluster assignments, we 
##use nstart argument. If nstart > 1, the K-mean clustering will be performed using 
##multiple random assignments (Step 1 of Algorithm 10.1).
##However the kmeans() function will report only the best results.


set.seed(3)
km.out =kmeans(x, 3, nstart =1)
par(mfrow =c(1, 2))
plot(x, col =(km.out$cluster +1), main ="K-Mean Clustering K =1, nstart =1", xlab="", ylab ="", pch =20, cex =2)
km.out$tot.withinss ##81.474

##Now with nstart =20.
km.out =kmeans(x, 3, nstart =20)
km.out$tot.withinss ##76.34817
plot(x, col =(km.out$cluster +1), main ="K-Means Clustering K =1, nstart =20", xlab="", ylab ="", pch =20, cex =2)

##km.out$tot.withinss is the total within-cluster sum of squares which we seek 
##to minimize by K-Mean Clustering.
##Individual within class sum of squares are contained in km.out$withinss.
km.out$withinss  ##Individual values for the 3 clusters.

##It is recommended to run K Mean Clustering with a large value of nstart such as 
##20 or 50, since otherwise an undesirable local optimum may be obtained.

rm(list =ls())




