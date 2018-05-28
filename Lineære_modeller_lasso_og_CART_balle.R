library(glmnet)
library(hydroGOF)
library(rpart)
library(rpart.plot)
library(car)
library(ggplot2)
library(xtable)

############################## Med Race ################################
#fuld lineær model
lmfit <- lm(balle~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=hudcomplete)

##prøver med fuld model

CVracefuldlm <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=train)
  pred <- predict(fit,test)
  CVracefuldlm[i] <- rmse(pred,test$balle)
}


##prøver med BIC step model
lmstepBIC <- step(lmfit,k=log(nrow(hudcomplete)))
lmstepBIC$call
sum <- summary(lmstepBIC)$coefficients
konf <- confint(lmstepBIC)
estimater <- cbind(sum[,1],konf)
print(xtable(estimater),format.args=list(decimal.mark=','))

CVracelmstep <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle ~ rs122 + rs123 + rs129 + rs142 + rs442 + 
              rs611 + EUR + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstep[i] <- rmse(pred,test$balle)
}


##Prøver med AIC i step til at finde lineær model
lmstepAIC <- step(lmfit)
lmstepAIC$call

CVracelmstepAIC <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle ~ rs108 + rs122 + rs123 + rs129 + rs139 + 
              rs140 + rs142 + rs168 + rs180 + rs242 + rs267 + rs442 + rs611 + 
              køn + EUR + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepAIC[i] <- rmse(pred,test$balle)
}
sum <- summary(lmstepAIC)$coefficients
konf <- confint(lmstepAIC)
estimater <- cbind(sum[,-(2:4)],konf)
print(xtable(estimater),format.args=list(decimal.mark=','))
## Prøver med færre parametre i lm (ændrer k i step)

lmstep <- step(lmfit, k=7)
lmstep$call

CVracelmstepk7 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle ~ rs123 + rs129 + rs142 + rs442 + EUR + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepk7[i] <- rmse(pred,test$balle)
}

lmstep <- step(lmfit, k=8)
lmstep$call

CVracelmstepk8 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle ~ rs123 + rs142 + rs442 + EUR + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepk8[i] <- rmse(pred,test$balle)
}

lmstep <- step(lmfit, k=10)
lmstep$call

CVracelmstepk10 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle ~ rs142 + rs442 + EUR, data=train)
  pred <- predict(fit,test)
  CVracelmstepk10[i] <- rmse(pred,test$balle)
}


CVracelmnull <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle ~ 1, data=train)
  pred <- predict(fit,test)
  CVracelmnull[i] <- rmse(pred,test$balle)
}

########################### Uden Race #################################

CVfuldlm <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn, data=train)
  pred <- predict(fit,test)
  CVfuldlm[i] <- rmse(pred,test$balle)
}


##prøver med BIC step model
lmfitur <- lm(balle~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn, data=hudcomplete)

lmstepBIC <- step(lmfitur, k=log(nrow(hudcomplete)))
lmstepBIC$call

CVlmstep <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle ~ rs108 + rs122 + rs123 + rs129 + rs142 + 
              rs168 + rs442 + rs611, data=train)
  pred <- predict(fit,test)
  CVlmstep[i] <- rmse(pred,test$balle)
}

lmstepAIC <- step(lmfitur)
lmstepAIC$call
CVlmstepAIC <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle ~ rs108 + rs112 + rs122 + rs123 + rs129 + 
              rs140 + rs142 + rs168 + rs180 + rs242 + rs442 + rs611 + køn, data=train)
  pred <- predict(fit,test)
  CVlmstepAIC[i] <- rmse(pred,test$balle)
}


########################### Kun Race ##################################

lmfitrace <- lm(balle~EUR+AFR, data=hudcomplete)
CVlmkunrace <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete),replace = T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle~EUR+AFR,data=train)
  pred <- predict(fit,test)
  CVlmkunrace[i] <- rmse(pred, test$balle)
}

########################### CV Plot ###################################
Model <- c('lmballenul','afstamninglmballe')
Krydsvalideringsscore <- c(mean(CVracelmnull),mean(CVlmkunrace))
sd <- (1/sqrt(10))*c(sd(CVracelmnull),sd(CVlmkunrace))
ggplot()+geom_point(aes(Model,Krydsvalideringsscore)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25)


Model <- c('afstamninglmballe','fuldlmballe', 'lmballeBIC','lmballeAIC','lmballek07','lmballek08','lmballek10')
Krydsvalideringsscore <- c(mean(CVlmkunrace),mean(CVracefuldlm), mean(CVracelmstep),mean(CVracelmstepAIC),mean(CVracelmstepk7),mean(CVracelmstepk8),mean(CVracelmstepk10))
sd <- (1/sqrt(10))*c(sd(CVlmkunrace),sd(CVracefuldlm), sd(CVracelmstep),sd(CVracelmstepAIC),sd(CVracelmstepk7),sd(CVracelmstepk8),sd(CVracelmstepk10))
Parametre <- c(3,48,15,30,11,9,6)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')


Model <- c('lmballeAIC','lmballeBIC','SNPfuldlmballe','SNPlmballeAIC','SNPlmballeBIC')
Krydsvalideringsscore <- c(mean(CVracelmstepAIC),mean(CVracelmstep),mean(CVfuldlm),mean(CVlmstepAIC),mean(CVlmstep))
sd <- (1/sqrt(10))*c(sd(CVracelmstepAIC),sd(CVracelmstep),sd(CVfuldlm),sd(CVlmstepAIC),sd(CVlmstep))
Parametre <- c(30,15,46,26,17)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')
