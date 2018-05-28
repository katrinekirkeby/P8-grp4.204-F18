library(glmnet)
library(hydroGOF)
library(rpart)
library(rpart.plot)
library(car)
library(xtable)

#fuld lineær model med interaktion

lm.inter<- lm(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR*rs142, data=hudcomplete)
lm.inter$call
summary(lm.inter)
##prøver med fuld model

CVfuldlminter <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR*rs142, data=train)
  pred <- predict(fit,test)
  CVfuldlminter[i] <- rmse(pred,test$arm)
}


##prøver med BIC step model
lmstepBIC <- step(lm.inter,k=log(nrow(hudcomplete)))
lmstepBIC$call
summary(lmstepBIC)

CVlminterstep <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs129 + rs139 + rs442 + rs611 + EUR + AFR*rs142, data=train)
  pred <- predict(fit,test)
  CVlminterstep[i] <- rmse(pred,test$arm)
}


##Prøver med AIC i step til at finde lineær model
lmstepAIC <- step(lm.inter)
lmstepAIC$call


CVlminterstepAIC <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs108 + rs112 + rs129 + rs139 + rs140 + rs168 + 
              rs180 + rs242 + rs442 + rs611 + køn + EUR + AFR*rs142, data=train)
  pred <- predict(fit,test)
  CVlminterstepAIC[i] <- rmse(pred,test$arm)
}


## Prøver med færre parametre i lm (ændrer k i step)
lmstep <- step(lm.inter, k=10)
lmstep$call

CVlminterstepk10 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs139  + rs442 + rs611 + AFR*rs142, data=train)
  pred <- predict(fit,test)
  CVlminterstepk10[i] <- rmse(pred,test$arm)
}


lmstep <- step(lm.inter, k=12)
lmstep$call

CVlminterstepk12 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~rs442 + EUR + AFR*rs142, data=train)
  pred <- predict(fit,test)
  CVlminterstepk12[i] <- rmse(pred,test$arm)
}

lmstep <- step(lmfit, k=20)
lmstep$call

CVlminterstepk20 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs142 + AFR, data=train)
  pred <- predict(fit,test)
  CVlminterstepk20[i] <- rmse(pred,test$arm)
}



Model <- c('interarmBIC', 'interarmfuld','interarmAIC','interarmk10','interarmk12','interarmk20')
Krydsvalideringsscore <- c(mean(CVlminterstep), mean(CVfuldlminter),mean(CVlminterstepAIC),mean(CVlminterstepk10),mean(CVlminterstepk12),mean(CVlminterstepk20))
sd <- c(sd(CVlminterstep), sd(CVfuldlminter),sd(CVlminterstepAIC),sd(CVlminterstepk10),sd(CVlminterstepk12),sd(CVlminterstepk20))
Parametre <- c(17,50,28,12,6,4)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')

mean(CVlminterstepAIC)
