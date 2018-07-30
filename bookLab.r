# Load the ISLR Library
library(ISLR)

# Shows predictors on S&P Market Percentages from 2001-2005
names(Smarket)

# number of predictors in Smarket
dim(Smarket)

# Shows the percentages of the S&P Market from 2001-2005
summary(Smarket)

# Produces a matrix of pairwise correlations between all predictors
# in the dataset
cor(Smarket[,-9])

# R formality
attach(Smarket)

# Uncomment to show the correlation between Year and Volume
#plot(Volume)

# Fit a logistic regression model to predict 
# Direction using Lag1 through Lag5 and Volume
glm.fits = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+ Volume,
data = Smarket, family = binomial)

summary(glm.fits)

# Access just the coefficients for the model
coef(glm.fits)

# Accesses particular aspects of the coefficients
summary(glm.fits)$coef

# Access just the p-values of the coefficients
summary(glm.fits)$coef[,4]

# Use predict to show probablity of market going up
glm.probs = predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)
glm.pred = rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"

# Use table to show how many observations were classified correctly
table(glm.pred.Direction)

# Get the percentage of the time the model was right
mean(glm.pred == Direction)

# This time test with test data, not just the training data
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

# Train is now a boolean vector, it can be used to pik out
# stock market data before 2005, which is data from out training set

# Now that we have trained on all data before 2005, we will now test
# accuracy of the model's prediction on the year 2005
glm.fits = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5 + Volume,
data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")

# Same procedure as before
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005)

# Compute test success rate
mean(glm.pred == Direction.2005)

# Compute test error rate
mean(glm.pred != Direction.2005)

# Test results are disapointing, so we will retry with only Lag1 and Lag2
# since they had the highest p-values. Lag3 through Lag5 are not meaningful
glm.fits = glm(Direction ~ Lag1+Lag2, data = Smarket,
family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106/(106+76)

# Test results are a little better

# If we want to predict Direction on specific Lag1 and Lag2 values:
predict(glm.fits, newdata = data.frame(Lag1 = c(1.2,1.5),
Lag2 = c(1.1, -0.8)), type = "response")