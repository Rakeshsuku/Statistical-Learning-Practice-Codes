############################ K-fold Cross-Validation  ################################
library (ISLR)
library (boot)  ## For cv.glm () function. Here we use k = 10
library (graphics)
set.seed (17)
cv.error.10 = rep (0,10)
for (i in 1:10) {
        glm.fit = glm(mpg~poly (horsepower, i), data = Auto)
        cv.error.10[i] = cv.glm (Auto, glm.fit, K=10)$delta[1]
}

cv.error.10
barplot(cv.error.10, col="green", main ="CV MSE estimate vs Degree of Polynomial", ylab ="CV MSE estimate", xlab ="Degree of Polynomial", names.arg =1:10 )

rm(list =ls())
