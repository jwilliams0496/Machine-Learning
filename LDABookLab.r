# Now we will preform Linear Discriminant Analysis on Smarket
library(ISLR)
attach(Smarket)

# Set uptraining data, all data before the year 2005
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

# Fit an LDA model with lda(), which is the same as lm() and glm() but
# no family option. Only use observation before 2005
library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit

#plot(lda.fit)

# Returns the names of predictions (Class), a posterior matrix (posterior), 
# and linear discriminants (x)
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

# Results of the LDA and the Logistic regression are almost identical

# Using the posterior probabilities, we can get the number of days
# whose probabilities meet a certain threshold probability of decreasing

# Threshold of at least 50%
sum(lda.pred$posterior[,1] >= .5)

# Threshold of less than 50%
sum(lda.pred$posterior[,1] < .5)

# We will now fit a QDA model to the Smarket Data
qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)

# The QDA model predicts corretly about 60% of the time, which is
# better than the LDA model or the Logistic Regression model