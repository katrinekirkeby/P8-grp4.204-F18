library(glmnet)
library(hydroGOF)
library(rpart)
library(rpart.plot)
library(car)
library(xtable)

#fuld lineær model
lmfit <- lm(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=hudcomplete)

#Finder optimal værdi af lambda
x <- model.matrix(lmfit)
x <- x[,-1]
cv <- cv.glmnet(x,hudcomplete$arm, type.measure = 'mse')
plot(cv)
#lambda der giver mindste mse i krydsvalidering
cv$lambda.min
#største lambda der giver krydsvalidering inden for en standardafvigelse af mindste cv
cv$lambda.1se
coef(cv, s='lambda.min')
coef(cv, s=0.1961425)

## Tabel over coef
coef<-c(coef(cv, s=0.1961425)[,1])
coef <- coef[coef!=0]
xtable(data.frame((coef)))


##Laver krydsvalidering for lasso
s <- 0.1961425
CVracelasso <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  xtrain <- model.matrix(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=train)
  xtest <- model.matrix(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=test)
  fit <- glmnet(xtrain[,-1], train$arm, lambda = s)
  pred <- as.vector(predict(fit,xtest[,-1], type='link'))
  CVracelasso[i] <- rmse(pred,test$arm)
}
mean(CVracelasso)
sd(CVracelasso)


##Fitter lm med samme variable og laver krydsvalidering
#Slår parametre sammen

hudpostlm <- hudcomplete
hudpostlm$rs108 <- recode(hudpostlm$rs108, "c('C','CT')='CogCT'; else='T'")
hudpostlm$rs123 <- recode(hudpostlm$rs123, "c('GA','A')='GAogA'; else='G'")
hudpostlm$rs129 <- recode(hudpostlm$rs129, "c('A','G')='AogG'; else='GA'")
hudpostlm$rs132 <- recode(hudpostlm$rs132, "c('GC','C')='GCogC'; else='G'")
hudpostlm$rs139 <- recode(hudpostlm$rs139, "c('T','C')='TogC'; else='CT'")
hudpostlm$rs140 <- recode(hudpostlm$rs140, "c('GA','A')='AogGA'; else='G'")
hudpostlm$rs168 <- recode(hudpostlm$rs168, "c('G','C')='GogC'; else='GC'")
hudpostlm$rs442 <- recode(hudpostlm$rs442, "c('C','CT')='CogCT'; else='T'")
hudpostlm$rs611 <- recode(hudpostlm$rs611, "c('GC','C')='GCogC'; else='G'")


postlassofit <- lm(arm~rs108+rs123+rs129+rs132+rs139+rs140+rs142+rs168+rs442+rs611+EUR+AFR, data = hudpostlm)

xtable(data.frame(summary(postlassofit)$coefficients[,1]))




CVracepostlm <- rep(0,10)
idx <- sample(1:10,nrow(hudpostlm), replace=T)
for (i in 1:10){
  train <- hudpostlm[idx!=i,]
  test <- hudpostlm[idx==i,]
  fit <- lm(arm~rs108+rs123+rs129+rs132+rs139+rs140+rs142+rs168+rs442+rs611+EUR+AFR, data=train)
  pred <- predict(fit,test)
  CVracepostlm[i] <- rmse(pred,test$arm)
}

library(ggplot2)


##prøver med fuld model

CVracefuldlm <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=train)
  pred <- predict(fit,test)
  CVracefuldlm[i] <- rmse(pred,test$arm)
}


##prøver med BIC step model
lmstepBIC <- step(lmfit,k=log(nrow(hudcomplete)))
lmstepBIC$call

CVracelmstep <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs129+rs139+rs142+ rs442 + rs611+AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstep[i] <- rmse(pred,test$arm)
}


##Cart både før og efter trimning

hudcart<- rpart(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=Data, minsplit=2)

CVracefuldcart <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- rpart(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=train, minsplit=2)
  pred <- predict(fit,test)
  CVracefuldcart[i] <- rmse(pred,test$arm)
}

plotcp(hudcart)

CVracecart026 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit1 <- rpart(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=train, minsplit=2)
  fit <- prune.rpart(fit1, cp=0.026)
  pred <- predict(fit,test)
  CVracecart026[i] <- rmse(pred,test$arm)
}


##Prøver med AIC i step til at finde lineær model
lmstepAIC <- step(lmfit)
lmstepAIC$call

CVracelmstepAIC <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs108 + rs112 + rs123 + rs129 + rs132 + rs139 + 
              rs142 + rs168 + rs242 + rs442 + rs611 + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepAIC[i] <- rmse(pred,test$arm)
}

## Prøver med færre parametre i lm (ændrer k i step)
lmstep <- step(lmfit, k=10)
lmstep$call

CVracelmstepk10 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs139 + rs142 + rs442 + rs611 + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepk10[i] <- rmse(pred,test$arm)
}


lmstep <- step(lmfit, k=11)
lmstep$call

CVracelmstepk11 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs142 + rs442 + rs611 + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepk11[i] <- rmse(pred,test$arm)
}

lmstep <- step(lmfit, k=20)
lmstep$call

CVracelmstepk20 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs142 + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepk20[i] <- rmse(pred,test$arm)
}

CVracelmnull <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ 1, data=train)
  pred <- predict(fit,test)
  CVracelmnull[i] <- rmse(pred,test$arm)
}


Model <- c('CART.fuldarm','CART.cp=026arm', 'lmarmAIC','lmarmk10')
Krydsvalideringsscore <- c(mean(CVarmfuld),mean(CVarm026),mean(CVracelmstepAIC),mean(CVracelmstepk10))
sd <- (1/sqrt(10))*c(sd(CVarmfuld),sd(CVarm026),sd(CVracelmstepAIC),sd(CVracelmstepk10))
Parametre <- c(15,5,24,4)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')




Model <- c('fuldlmarm', 'lmarmBIC','lmarmAIC','lmarmk10','lmarmk11')
Krydsvalideringsscore <- c(mean(CVracefuldlm), mean(CVracelmstep),mean(CVracelmstepAIC),mean(CVracelmstepk10),mean(CVracelmstepk11))
sd <- (1/sqrt(10))*c(sd(CVracefuldlm), sd(CVracelmstep),sd(CVracelmstepAIC),sd(CVracelmstepk10),sd(CVracelmstepk11))
Parametre <- c(48,12,24,10,8)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')



Model <- c('lassoarm', 'lmarmAIC','lmarmk10','postlassolmarm')
Krydsvalideringsscore <- c(mean(CVracelasso), mean(CVracelmstepAIC),mean(CVracelmstepk10),mean(CVracepostlm))
sd <- (1/sqrt(10))*c(sd(CVracelasso),sd(CVracelmstepAIC),sd(CVracelmstepk10),sd(CVracepostlm))
Parametre <- c(14,24,10,14)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')
