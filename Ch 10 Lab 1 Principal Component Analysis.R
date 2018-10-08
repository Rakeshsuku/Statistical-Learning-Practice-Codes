##################Ch 10 Lab 1 - Principal Component Analysis ####################

##Here we analyse USArrest data set.
data()  ##Displays all available data sets.
?USArrests ##A data frame with 50 observations on 4 variables.
dim(USArrests)
fix(USArrests)
str(USArrests)
summary(USArrests)
names(USArrests)
states =row.names(USArrests) ##Copies the row names for the data set into variable states
##states

apply(USArrests, 2, mean) ##Displays the mean for all 4 variables in the data set.
apply(USArrests, 2, var) ##Displays the variance.
plot(USArrests) ##Plots pairwise scatter plot

par(mfrow =c(2,2))
##Plots the distribution all 4 variables
for(i in 1:4) {
  hist(USArrests[, i], main =names(USArrests)[i])
}

##We now perform PCA using prcomp() function after scaling the variables.
## By default prcomp() function centers the variables to mean 0.
pr.out =prcomp(USArrests, scale =TRUE)
names(pr.out)
##Center and Scale components corresponds to the mean and standard deviation values used for center-scaling.
##The rotation matrix the principal component loadings.
pr.out$rotation


##The 50 x 4 matrix x in the output of the prcomp() has the principal component scores.
##With column 1 being the scores for 1st principal component.
head(pr.out$x)
dim(pr.out$x)


##Now we can plot the first two principal component using biplot() function:
?biplot
?biplot.princomp
par(mfrow =c(1,1))
biplot(pr.out, scale=0)
##The scale =0 argument ensures that arrows are scaled to represent the loadings.
##Other values for scale slightly different biplots with different interpretation.

##The principal components are unique up to a sign change.
pr.out$rotation =-pr.out$rotation
pr.out$x =-pr.out$x
biplot(pr.out, scale =0)

##The prcomp() function also output the standard deviation of each principal component.
pr.out$sdev
##The variance explained by each principal component is obtained by:
pr.var =pr.out$sdev^2
pr.var

## To obtain the percentage of variance explained by each component, we devide the 
##variance explained by each component with the total variance explained.
pve =pr.var/sum(pr.var)
pve ##We see that first two PCs explain 62% and 24% of the total variation.

##Next we plot the PV explained by each component and the cumulative PV(% Variance Explained).
par(mfrow =c(1, 2))
plot(pve, xlab ="Principal Component", ylab ="Proportion of Variance Explained", ylim =c(0, 1), type ='b')
plot(cumsum(pve), xlab ="Principal Component", ylab ="Cumulative Proportion of Variance Explained", ylim =c(0, 1), type ='b')


rm(list =ls())
