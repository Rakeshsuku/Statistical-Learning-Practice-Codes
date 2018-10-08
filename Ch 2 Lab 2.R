getwd()
setwd ("D:/Linux Drive/Statistical Learning/My Work")
Auto <- read.table ("Auto.data", header=T, na.strings="?")
##Read data file "Auto.data". Note: It is a text file. Indicates first row is header.
## Also "?" should be treated as missing character.
class(Auto) ##Displays data type of Auto.
fix (Auto) ##Views the dataframe in a spreadsheet like window.
dim (Auto)
Auto = na.omit (Auto) ##Only five rows contain missing values. Hence these rows are omitted.
names (Auto) ##Displays variable names of Auto.
summary(Auto) ##5 point summary + mean for variables in Auto.


plot (Auto$cylinders, Auto$mpg)
attach (Auto)
plot(cylinders, mpg)
cylinders = as.factor(cylinders)  ##Convert cylinders to a qualitative variable.
plot(cylinders, mpg, col="red", varwidth = T, horizontal = T, ylab = "cylinders", xlab = "MPG")
hist (mpg, col = 2, breaks = 15)
pairs (Auto)
## pairs (Auto) creates a scatter plot matrix i.e. one for each pair of variables

## Use of identify () function:
plot (horsepower, mpg)
identify (horsepower, mpg, name)  ##Prints identified pts. Only after exit (right click or Esc button)

## q() function quits R. savehistory () function saves a record of all cmnds typed.
## loadhistory () function loads saved history.

ls()
rm (list=ls())
