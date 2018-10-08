#####################Ch 10 Lab 3 - NCI60 Data Example ###########################

##Unsupervised techniques are often used in the analysis of genomic data.
##Here We apply these techniques on NCI60 cancer cell line microarray data.

library(ISLR)
?NCI60
##The data contains expression levels on 6830 genes from 64 cancer cell lines. 
##Cancer type is also recorded.
str(NCI60)
summary(NCI60)
class(NCI60) ##List
names(NCI60)
dim(NCI60$data)  ##A 64 x 6830 matrix.

nci.labs =NCI60$labs
nci.data =NCI60$data

##Each cell is labelled with a Cancer type. 
##However we do not make use of the cancer type in performing PCA and Clustering.
##Instead we will use these lables to evaluate the results of our clustering.

nci.labs[1:4]
table(nci.labs) ##Tables the cancer types in the data set.

############################## PCA on NCI60 Data #################################

##We begin by scaling the variables to have sd =1.
pr.out =prcomp(nci.data, scale =TRUE)
##We now plot the first few principal component score vectors to visualize 
##the data.

##The observations corresponding to same cancer type will be plotted in same 
##color(using labs data).

##We first create a simple function that assigns a distinct color to each element 
##of a numeric vector. This function is used to assign a unique color to each of the 
##64 cell lines based on cancer type to which it corresponds to.

Cols =function(vec) {
  cols =rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

##Note that the function rainbow takes as its argument a vector of positive 
##integers and returns a vector containing that number of distinct colors. See:
##as.factor(nci.labs)
##as.numeric(as.factor(nci.labs))
##rainbow(as.numeric(as.factor(nci.labs)))

par(mfrow =c(1,2))
plot(pr.out$x[, 1:2], col =Cols(nci.labs), pch =19, xlab ="Z1", ylab ="Z2")
plot(pr.out$x[, c(1,3)], col =Cols(nci.labs), pch =19, xlab ="Z1", ylab ="Z3")

##On the whole, samples corresponding to a single cancer type tend to haev similar
##values on the first few principal components score vectors. This suggest that 
##samples from the same cancer type tend to have similar gene expression levels.

##We can obtain a summary of the variance explained (PVE) for the first few principal
##components using summary()
summary(pr.out))

##Using plot() function we can plot the varaince explained.
plot(pr.out) ##Height of each bar obtained by squaring corresponding elements of pr.out$sd

##Next we plot the Percentage variance explained & cumulative Percentage variance explained.
pve =100*(pr.out$sdev^2/sum(pr.out$sdev^2))
par(mfrow =c(1,2))
plot(pve, type ="o", ylab ="PVE", xlab ="Principal Component", col ="blue")
plot(cumsum(pve), type ="o", ylab ="Cumulative PVE", xlab ="Principal Component", col ="brown")

##The first 7 components explain 40% of the variance (This is not a large number).
##But there is an elbow in the plot at around 7th Principal Component.
##This indicates that there may be little benefit to examining more than 7 components.

################### Hierarchial Clustering  on NCI60 Data #######################

##To begin with we center scale each variables.
sd.data =scale(nci.data)

##We now perform hierarchial clustering of the data using complete, single and average linkage.
##Euclidean distance is used as the dissimilarity measure.
par(mfrow =c(1, 3))
data.dist =dist(sd.data)
plot(hclust(data.dist), labels =nci.labs, main ="Complete Linkage", xlab ="", sub ="", ylab ="")

plot(hclust(data.dist, method ="average"), labels =nci.labs, main ="Average Linkage", xlab ="", sub ="", ylab ="")

plot(hclust(data.dist, method ="single"), labels =nci.labs, main ="Single Linkage", xlab ="", sub ="", ylab ="")

##We see that the choice of linkage certainly affect the results obtained.
##Single linkage tends to yield trailing clusters - very large clusters onto which 
##individual observations attach one-by-one.

##On the other hand Complete and Average linkage tend to yield more balance 
##attractive clusters and hence are preferred over Single linkage.
##We will use Complete linkage hierarchial clustering for our analysis here.

hc.out =hclust(dist(sd.data))
hc.clusters =cutree(hc.out, 4)
table(hc.clusters, nci.labs)
##There are some clear patterns. All the leukemia cell lines fall in cluster 3.
##While the breast cancer lines are spread out over three different clusters.

##We can plot the cut on the dendrogram that produces these 4 clusters.
par(mfrow =c(1,1))
plot(hc.out, labels =nci.labs)
abline(h=139, col="red") ##The h=139 instructs abline to draw a horizontal 
##line at height 139

##Next we compare the K mean clustering result with 4 clusters with that of hierarchial clustering.
set.seed(2)
km.out =kmeans(sd.data, 4, nstart =20)
km.clusters =km.out$cluster
table(km.clusters, hc.clusters)

## We see that the clusters obtained be the two methods are somewhat different.
##We see that Cluster 2 in K Mean clustering is similar to Cluster 3 in hierarchial clustering.


##Rather that performing hierarchial clustering on the full data set, 
##we can perform hierarchial cluster of the first few Principal Components.
hc.out =hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels =nci.labs, main ="Hier. Clust. on First 5 PC score Vectors")
table(cutree(hc.out, 4), nci.labs)
## We see that the results obtained are different.
##Sometimes performing hierarchial clustering on Principal Component score vectors,
##can give better results. In this case, we see PCA as one for denoising the data.


rm(list =ls())
