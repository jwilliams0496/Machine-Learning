# We will now use the glmnet package to perform ridge reggression and lasso
# to predict salary of baseball hitters
library(ISLR)
names(Hitters)

# Remove players who are missing salaries in the data
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

# To perfom ridge and lasso, we do not use the y~x syntax. We must pass in
# an x matrix and a y vector
x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary

# Ridge Regression
library(glmnet)

# Creates a selected range of values for lambda
grid = 10^seq(10, -2, length = 100)

# with alpha = 0 ridge will be used, if alpha = 1 lasso will be used
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)

# Note: glmnet automatically standardizes variables, to turn this off use
# standardize = FALSE as an arguement

# Associated with each lambda value is a vector of regression coefficients
# stored in a matrix accessed by coef(). In this lab it's a 20x100 (20 rows
# for each predictor, and 100 columns for each value of lambda
dim(coef(ridge.mod))

# Here are the coefficient values when lambda = 11,498
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

# along with the coefficients L squared norm:
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

# In contrast, here are the coefficients when lambda = 705, along with L
# squared norm
ridge.mod$lambda[60]
coef(ridge.mod)[,60]

# Notice much larger L squared norm of the coefficients with a smaller lambda
# value
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

# Predict can be used for a lot of things in ridge regression. For instance
# we can obtain the rige regression coefficients for a new value of lambda = 50
predict(ridge.mod, a = 50, type = "coefficients")[1:20,]

# We will now split the samples into a training set and a test set in order
# to estimate the test error of ridge regression and the lasso

# We can either produce a random vector of TRUE, FALSE elements and select
# the observations corresponding to TRUE for the training data. Or we can
# randomly choose a numbset of numbers between 1 and n to be the training set

# We will use the random subset for training data split
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

# Next fit a ridge regression of the training set and evaluate its MSE on the
# test set using lambda = 4. This time we will get predictions by replacing
# type = "coefficients" with the newx argument
ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred-y.test)^2)

# Note that if we fitted a model with just an intercept, we would have
# predicted each test observation using the mean of the training observations
# MSE would look like this:

mean((mean(y[train])-y.test)^2)

# We could also get the same result with a very large value of lambda
ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred-y.test)^2)

# We now need to check whether there is any benefit to performing ridge
# regression with lambda = 4 instead of just prefoming least squares regression
# Recall that least squares regression is just ridge regression with lambda = 0
ridge.pred = predict(ridge.mod, s = 0, newx = x[test,], exact = T)
mean((ridge.pred-y.test)^2)

# If we want to fit an unpenalizing least squares model, then we should use
# the lm() function since it provides more useful outputs, like p-vals etc
lm(y~x, subset = train)
predict(ridge.mod, s = 0, exact = T, x = x, y = y, type = "coefficients")[1:20,]

# To get a better tuning value for lambda rather than just arbitrarily picking
# lambda = 4, it would be better to use cross validation to choose the tuning
# parameter. We can do this with the built in cross validation function cv.glmnet()
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train], alpha = 0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam

# Therefore we see that the value of lambda with the smallest validation error
# is 212, predicting with lambda = 212 results with this MSE
ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])
mean((ridge.pred -y.test)^2)

# Finally we refit out ridge reggression model on the full data set
out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

# None of the coefficients are 0, ridge regression does not preform variable
# selection, unlike the lasso



# The Lasso is the same process, just with alpha = 1
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2)

# Here we see that the lasso yeilds even better results than the null model
# or the least squares regression. If we look at the lasso model, we can see
# that it has set 12 of the 19 variables to 0
out = glmnet(x, y, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef

# Here are the non zero predictors
lasso.coef[lasso.coef != 0]