college_data = read.csv("College.csv")
attach(College)

names(college_data)

PAccept = (Accept/Apps)

regfit.full = regsubsets(PAccept~.-Accept-Apps, College, nvmax = 16)
reg.summary = summary(regfit.full)

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

coef(regfit.full, 10)



# Forward and Backward Stepwise

regfit.fwd = regsubsets(PAccept~.-Accept-Apps, College, nvmax = 16, method = "forward")

reg.summary.fwd = summary(regfit.bwd)
which.min(reg.summary.fwd$bic)

regfit.bwd = regsubsets(PAccept~.-Accept-Apps, College, nvmax = 16, method = "backward")

reg.summary.bwd = summary(regfit.fwd)
which.min(reg.summary.bwd$bic)

