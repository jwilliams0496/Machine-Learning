# We wish to know the best combination of variables to predict a baseball
# players salary

library(ISLR)
names(Hitters)

# Some salaries not given in the data, so we will omit them
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

# Leaps has the function regsubsets for testing alternatives to cross
# validation. Here we will use it to prefom best subset selection by RSS
library(leaps)
regfit.full = regsubsets(Salary~.,Hitters)
summary(regfit.full)

# By default, regsebsets uses the best 8 predictors, but we can provide
# the max number of predictors to use, in this case 19 predictors
regfit.full = regsubsets(Salary~., data = Hitters, nvmax = 19)
reg.summary = summary(regfit.full)
reg.summary = summary(regfit.full)

# Summary will contain the RSq, RSS, adjusted RSq, Cp, and BIC
names(reg.summary)

# This accesses just the RSq statistics. 32% for only one variable, but
# 54% with 10 variables. The more predictors added, RSq will rise
reg.summary$rsq

# Here we will plot RSS, adjusted RSq, Cp, and BIC for all of the models
# to help decide which one to pick
par(mfrow=c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# which idicates the models with the smallest error statistics
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

# Here we can plot the Cp and BIC statistics and then indicate the models
# with the smallest error statistics
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)

points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)
which.min(reg.summary$bic)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

# More easily, regsubsets has a built in plot function which can also be used
# to display the best models using a given number of predictors

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

# The top row of each plot contains a black square for each variable selected
# according to the optimal model associated with that statistic.

# We can use coef to see the coefficient estimates associated with this model
coef(regfit.full, 6)


# Forward And Backward Stepwise Selection

# regsubsets can also be used for forward and backward stepwise selection
# this is done using the argument method = "forward" or "backward"

regfit.fwd = regsubsets(Salary~., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)

regfit.bwd = regsubsets(Salary~., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

# Looking at the best 7 variable models, we can see that their coefficients
# for different models are different
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

# Model Selection

set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test = (!train)

regfit.best = regsubsets(Salary~.,data = Hitters[train,], nvmax = 19)

test.mat = model.matrix(Salary~., data = Hitters[test,])

val.errors = rep(NA, 19)

for(i in 1:19) {
	coefi = coef(regfit.best, id = i)
	pred = test.mat[,names(coefi)]%*%coefi
	val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}

val.errors
which.min(val.errors)

coef(regfit.best,10)