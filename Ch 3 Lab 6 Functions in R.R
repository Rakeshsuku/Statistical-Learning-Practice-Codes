############# Writing Functions in R #############

##A function that reads in the ISLR and MASS libraries called LoadLibraries ()
LoadLibraries = function () {
        library (ISLR)
        library (MASS)
        print ("The libraries have been loaded.")
}
LoadLibraries  ##Displays the code for the function LoadLibraries ()
LoadLibraries ()  ##Runs the function LoadLibraries ().

## A function to make a regression model and plot the regression lines.
regplot = function (x,y) {
  fit = lm (y~x)
  plot(x,y)
  abline (fit, col="red")
}

attach (Carseats)
regplot (Price, Sales)  ##Use regplot function.

## Adding ... argument to the function regplot.
regplot = function (x,y,...) {
  fit = lm (y~x)
  plot(x,y,...)
  abline (fit, col="red")
}

##Use regplot function with ... arguments.
regplot (Price, Sales, xlab="Price", ylab="Sales", col="blue", pch=20, main="Sales as a function of Price")
##Additinal arguments passed as such to plot function inside regplot function.
