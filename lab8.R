########

library(ISLR)
library(leaps)
library(splines)
library(gam)
set.seed(1)
train=sample(777, 500)
test=(-train)

question.1=function(){
  fit=regsubsets(Outstate~., data=College[train,], nvm=17, method="forward")
  reg.summary =summary(fit)

  plot(reg.summary$rsq, xlab = "Number of Variables", ylab = "rsq")
  plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "adjr2")
  plot(reg.summary$cp, xlab = "Number of Variables", ylab = "cp")
  print(reg.summary$which[11,])
}

gam1=gam(Outstate~s(Private,df =1)+s(Apps,df =2)+s(Accept,df =3)
	+s(Enroll,df =4)+s(Top10perc,df =5)+s(Top25perc,df =6)
	+s(F.Undergrad,df =7)+s(P.Undergrad,df =8)+s(Room.Board,df =9)
	+s(Books,df =10)+s(Personal,df =11), data=College[train,])

gam1=gam(Outstate~Private+Apps+Accept+Enroll+Top10perc+Top25perc
	+F.Undergrad+P.Undergrad+Room.Board+Books+Personal, data=College[train,])

outstateLims = range(Outstate)
outstate.grid = seq(from = outstateLims[1], to = outstateLims[2])
preds = predict(gam1, newdata = list(Outstate = outstate.grid), se = TRUE)


mse1 = mean((preds-College$Outstate[test,])^2)

