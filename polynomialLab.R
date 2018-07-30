# Fit polynomial model
fit=lm(wage~poly(age ,4) ,data=Wage)
coef(summary (fit))

# raw=T changes coefficient estimates, but nothing else in a meaningful way
fit2=lm(wage~poly(age ,4,raw=T),data=Wage)
coef(summary(fit2))

# Another way of fitting this model which uses I() or identify
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)

# Create a grid of values at which we want predictions
agelims =range(age)
age.grid=seq(from=agelims [1],to=agelims [2])

# Call predict with standard errors on
preds=predict (fit ,newdata =list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit +2* preds$se .fit ,preds$fit -2* preds$se .fit)

# All of this will plot the data and add the fit from the degree 4 polynomial
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(age ,wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title(" Degree -4 Polynomial ",outer=T)
lines(age.grid ,preds$fit ,lwd=2,col="blue")
matlines (age.grid ,se.bands ,lwd=1, col=" blue",lty=3)

# Regardless if orthangonal polynomials are used or not, we get same values
preds2=predict(fit2 ,newdata =list(age=age.grid),se=TRUE)
max(abs(preds$fit -preds2$fit))

# Analysis of variance between null hypothesis and degree polynomials
fit.1=lm(wage~age ,data=Wage)
fit.2=lm(wage~poly(age ,2),data=Wage)
fit.3=lm(wage~poly(age ,3),data=Wage)
fit.4=lm(wage~poly(age ,4),data=Wage)
fit.5=lm(wage~poly(age ,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit .5)
# model 1 is sufficient against the other models

# Alternative to anova, we could get the p-vals like this
coef(summary (fit.5))
(-11.983)^2

# anova also works without orthagonal
fit.1=lm(wage~education +age ,data=Wage)
fit.2=lm(wage~education +poly(age ,2) ,data=Wage)
fit.3=lm(wage~education +poly(age ,3) ,data=Wage)
anova(fit.1,fit.2,fit .3)

# Rather than hypothesis tests and anova, we could choose
fit=glm(I(wage >250)~poly(age ,4),data=Wage , family=binomial)

pfit=exp(preds$fit )/(1+exp(preds$fit ))
se.bands.logit = cbind(preds$fit +2* preds$se .fit , preds$fit -2*
preds$se .fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

preds=predict(fit,newdata =list(age=age.grid),type=" response ",
se=T)

plot(age ,I(wage >250),xlim=agelims ,type="n",ylim=c(0,.2))
points(jitter(age), I((wage >250)/5),cex=.5,pch ="|",
col="darkgrey ")
lines(age.grid ,pfit ,lwd=2, col ="blue")
matlines (age.grid ,se.bands ,lwd=1, col=" blue",lty=3)




