threat_train = read.csv("threat_data.csv")
threat_test = read.csv("threat_data_test.csv")

# Info on Threat
names(threat_train)
dim(threat_train)
summary(threat_train)

# info on Threat Test
names(threat_test)
dim(threat_test)
summary(threat_test)

# R formality
attach(threat_train.data)

# Fit logistic regression model to predict non-threat
glm.fits = glm(threat ~ time + s1 + s2 + s3, data = threat_train, family = binomial)

# info on logistic fit model
summary(glm.fits)

coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

glm.probs = predict(glm.fits, type = "response") > .965
glm.pred = rep("Threat", 1000)
glm.pred[glm.probs < .965] = "Not Threat"
table(glm.pred, threat)