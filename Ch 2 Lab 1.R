x <- c(1:50)
y <- rnorm (50)
getwd () #Gets the current working directory. Use setwd () to change working directory.
pdf ("Figure.pdf")    ##Creates Figure.pdf in the working directory. Can use jpeg ()
plot (x, y, col="green")
dev.off ()   ## Indicates that the plot is complete.

##More Plots##
y <- x
z <- outer (x, y, function (x, y) cos(y)/(1+x^2))
contour (x, y, z)
contour (x, y, z, nlevels = 45, add = T)
fa <- (z-t(z))/2
contour (x, y, fa, nlevels = 15)
image (x, y, fa)  ##Similar to contour, but colour coded. Also called Heatmap.
persp (x, y, fa, col="grey") ## 3D graph
persp (x, y, fa, theta = 30, col="grey")
persp (x, y, fa, theta = 30, phi = 30, col="grey")

a=matrix(data=c(1:9), nrow=3, ncol=3)  ##Creates a matrix.
a ##Note that data got saved by column.
b=matrix(data=c(1:9), nrow=3, ncol=3, byrow=TRUE)  ##Creates a matrix.
b ##Note that data got saved by row.
sqrt(a) ##Operations on matrix.
a*b ##Element by element multiplication.
dim (b) ##Diamension of b.

##Standard normal random variables
set.seed(1000) ##Sets seed to create same set of random variables. Takes an arbitary integer input.
norm_var1= rnorm(50) ##Creates a standard normal random variables with a mean 0 and sd 1.
norm_var2= norm_var1+rnorm(50, mean=50, sd=.1)
cor(norm_var1, norm_var2) ##Computes the correlation between two variables.
mean(norm_var2)
var(norm_var2) ##Variance.
sqrt(var(norm_var2))
sd(norm_var2) ##Standard Deviation

c=seq(1,10) ##Creates a sequence of integers between(including) 1 and 10.
c= 1:10 ##Same as above
d=seq (3, 30, length=10)
d=seq (-pi, pi, length=50)


ls() ##Lists all objects such as data and function that we have saved so far.
rm(fa) ##Removes rm.
rm (list=ls()) ##Removes all objects.
