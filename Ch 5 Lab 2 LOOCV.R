############################ Leave-One-Out Cross-Validation (LOOCV)  ################################
library (ISLR)
library (boot)  ## For cv.glm () function
library(graphics) ##for barplot() function 

glm.fit = glm (mpg~horsepower, data=Auto)
## Here glm is used to fit a linear regression so that the output can be used with cv.glm () function
## Note that the family argument is not provided.
coef (glm.fit)
cv.err = cv.glm (Auto, glm.fit)  ##Default value of k = 1 (LOOCV) is used.
names(cv.err)
cv.err$delta  ##The two numbers in the delta vector contains the cross validation results.

## We can repeat the process for increasingly complex polynomial functions. To automate the process we use a for () function as below:

cv.error = rep (0,5)
for (i in 1:5) {
        glm.fit=glm(mpg~poly (horsepower, i), data = Auto)
        cv.error[i] = cv.glm (Auto, glm.fit)$delta[1] 
}

cv.error
barplot(cv.error, col="green", main ="CV MSE estimate vs Degree of Polynomial", ylab ="CV MSE estimate", xlab ="Degree of Polynomial", names.arg =1:5 )
## The estimated MSE shows a sharp drop between linear and quadratic fits but there is no clear improvement for higher orders.

rm(list =ls())
