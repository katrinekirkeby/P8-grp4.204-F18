library(glmnet)
library(hydroGOF)
library(rpart)
library(rpart.plot)
library(car)
library(ggplot2)
#fuld lineær model
lmfit <- lm(pande~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=hudcomplete)

##prøver med fuld model

CVracefuldlm <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(pande~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=train)
  pred <- predict(fit,test)
  CVracefuldlm[i] <- rmse(pred,test$pande)
}


##prøver med BIC step model
lmstepBIC <- step(lmfit,k=log(nrow(hudcomplete)))
lmstepBIC$call
CVracelmstep <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(pande ~ rs108 + rs129 + rs142 + rs442 + rs611 + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstep[i] <- rmse(pred,test$pande)
}

##Prøver med AIC i step til at finde lineær model
lmstepAIC <- step(lmfit)
lmstepAIC$call

CVracelmstepAIC <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(pande ~ rs108 + rs123 + rs129 + rs132 + rs139 + 
              rs140 + rs142 + rs242 + rs442 + rs611 + køn + EUR + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepAIC[i] <- rmse(pred,test$pande)
}


## Prøver med færre parametre i lm (ændrer k i step)

lmstep <- step(lmfit, k=8)
lmstep$call

CVracelmstepk8 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(formula = pande ~rs129 + rs142 + rs442 + rs611 + 
              AFR, data = train)
  pred <- predict(fit,test)
  CVracelmstepk8[i] <- rmse(pred,test$pande)
}



CVracelmnull <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(pande ~ 1, data=train)
  pred <- predict(fit,test)
  CVracelmnull[i] <- rmse(pred,test$pande)
}



Model <- c('fuldlmpande','lmpandeBIC','lmpandeAIC','lmpandek8')
Krydsvalideringsscore <- c(mean(CVracefuldlm), mean(CVracelmstep),mean(CVracelmstepAIC),mean(CVracelmstepk8))
sd <- (1/sqrt(10))*c(sd(CVracefuldlm), sd(CVracelmstep),sd(CVracelmstepAIC),sd(CVracelmstepk8))
Parametre <- c(48,12,24,10)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')


