# Split training and test data
library(ISLR)
set.seed(1)
train = sample(392, 196)

# Error rates with linear, squared, and cubic fits
lm.fit = lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg-predict(lm.fit, Auto)) [-train]^2)

lm.fit2 = lm(mpg~poly(horsepower ,2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto)) [-train]^2)

lm.fit3 = lm(mpg~poly(horsepower, 3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# Different training data, slightly different error rates
set.seed(2)
train = sample(392, 196)
lm.fit = lm(mpg~horsepower, subset = train)

lm.fit = lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit, Auto)) [-train]^2)

lm.fit2 = lm(mpg~poly(horsepower ,2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto)) [-train]^2)

lm.fit3 = lm(mpg~poly(horsepower, 3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# Leave one out validation for logistic ression fits (also works for linear
# reression if you leave out a family argument

# Linear since family argument not passed
glm.fit = glm(mpg~horsepower, data = Auto)
coef(glm.fit)

# Since LOOCV only works for logistic model, we will use this notation to
# get linear fits even with a logistic model
library(boot)
glm.fit = glm(mpg~horsepower, data = Auto)

# This should take while
cv.err = cv.glm(Auto, glm.fit)

# Delta vector contains the cross validation results
cv.err$delta

cv.error = rep(0, 5)

# This should take even longer
for (i in 1:5) {
	glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
	cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}

cv.error

# K Fold Cross Validation with K = 10

set.seed(17)
cv.error.10 = rep(0, 10)

# This should take a long time too
for (i in 1:10) {
	glm.fit = glm(mpg~horsepower, i), delta = Auto)
	cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10

# Bootstrap
