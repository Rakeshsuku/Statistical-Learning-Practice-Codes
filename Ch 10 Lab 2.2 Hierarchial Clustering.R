#####################Ch 10 Lab 2.2 - Hierarchial Clustering - Incomplete ###########################

##We start with a simulated data set with two clusters.
set.seed(2)
x =matrix(rnorm(50*2), ncol =2)
x[1:25, 1] =x[1:25, 1] +3
x[1:25, 2] =x[1:25, 2] -4


##The hclust() function implements hierarchial clustering in R.
##We use the above data to plot the hierarchial clustering dendrogram using complete, 
##single and average linkage clustering with Euclidean distance as the 
##dissimilarity measure.

##We start with complete linkage.
##dist() function is used to calculate 50 x 50 Euclidean distance matrix.
hc.complete =hclust(dist(x), method ="complete")
##Similarly we perform the hierarchial clustering using average and single linkage.
hc.average =hclust(dist(x), method ="average")
hc.single =hclust(dist(x), method ="single")

##Next we plot the dendrogram using plot() function.
##The number at the bottom indicates  the plot identity of each observation.
par(mfrow =c(1,3))
plot(hc.complete, main ="Complete Linkage", xlab ="", ylab ="", sub ="", cex =0.9)
plot(hc.average, main ="Average Linkage", xlab ="", ylab ="", sub ="", cex =0.9)
plot(hc.single, main ="Single Linkage", xlab ="", ylab ="", sub ="", cex =0.9)


##To determine the cluster labels for each observation with a given cut of the 
##dendrogram, we can use cutree() function.
?cutree
cutree(hc.complete ,2)
cutree(hc.average ,2)
cutree(hc.single ,2)

cutree(hc.single ,4)


##To scale the variable before performing hierarchial clustering of the observations, 
##we use scale() function.
xsc =scale(x)
par(mfrow =c(1,1))
plot(hclust(dist(xsc), method ="complete"), main ="Hierarchical clustering with scaled features")


##Correlation-based distance can be computed using the as.dist() function which converts an arbitary
##square symmetric matrix into a form that the hclust() function recognizes as a distance matrix.
##However, this only makes sense for data with at least 3 features since the absolute correlation
##between any two observations with measurements on two features is always 1.

x =matrix(rnorm(30*3), ncol =3)
dd =as.dist(1 -cor(t(x)))
plot(hclust(dd, method ="complete"), main ="Complete Linkage with Correlation-based Distance", xlab="", sub="")

rm(list =ls())
