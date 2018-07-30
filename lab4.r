default_data = read.csv("Default.csv")

attach(default_data)
default_data = default_data[-1]

summary(default_data)

set.seed(1)
train = sample(10000, 5000)

glm.fit = glm(default ~ income + balance, family = binomial, data = Default)
coef(glm.fit)

mean((default - predict(glm.fit, default_data)) [-train]^2)