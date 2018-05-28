library(glmnet)
library(hydroGOF)
library(rpart)
library(rpart.plot)
library(car)
library(xtable)
library(leaps)
library(ggplot2)


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



postlassofit <- lm(arm~rs108T+rs123G+rs129GA+rs132G+rs139CT+rs140G+rs142G+rs142GA+rs168GC+rs442T+rs611G+EUR+AFR, data = dummydataarm)

xtable(data.frame(summary(postlassofit)$coefficients[,1]))




CVracepostlm <- rep(0,10)
idx <- sample(1:10,nrow(dummydataarm), replace=T)
for (i in 1:10){
  train <- dummydataarm[idx!=i,]
  test <- dummydataarm[idx==i,]
  fit <- lm(arm~rs108T+rs123G+rs129GA+rs132G+rs139CT+rs140G+rs142G+rs142GA+rs168GC+rs442T+rs611G+EUR+AFR, data=train)
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
  fit <- lm(arm ~ rs129 + rs139 + rs142 + rs442 + rs611 + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstep[i] <- rmse(pred,test$arm)
}


##Cart både før og efter trimning
hudcart<- rpart(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=Data, minsplit=2)
rpart.plot(hudcart)
plotcp(hudcart)
arm_026<-prune.rpart(hudcart, cp=0.026)
rpart.plot(arm_026)


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
              rs140 + rs142 + rs168 + rs242 + rs442 + rs611 + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepAIC[i] <- rmse(pred,test$arm)
}

## Prøver med færre parametre i lm (ændrer k i step)
lmstep <- step(lmfit, k=9)
lmstep$coefficients

CVracelmstepk9 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~rs139 + rs142 + rs442 + rs611 + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepk9[i] <- rmse(pred,test$arm)
}


lmstep <- step(lmfit, k=13)
lmstep$call

CVracelmstepk13 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs142 + rs442 + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepk13[i] <- rmse(pred,test$arm)
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





Model <- c('fuldlmarm', 'lmarmBIC','lmarmAIC','lmarmk09','lmarmk13')
Krydsvalideringsscore <- c(mean(CVracefuldlm), mean(CVracelmstep),mean(CVracelmstepAIC),mean(CVracelmstepk9),mean(CVracelmstepk13))
sd <- (1/sqrt(10))*c(sd(CVracefuldlm), sd(CVracelmstep),sd(CVracelmstepAIC),sd(CVracelmstepk9),sd(CVracelmstepk13))
Parametre <- c(48,12,26,10,6)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')





############################# dummy ################################
lmfit <- lm(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=hudcomplete)

x <- model.matrix(lmfit)
dummydataarm <- data.frame(cbind(x[,-1],hudcomplete[,'arm']))
names(dummydataarm)[48] <- 'arm'

lmfitdummy <- lm(arm~.,data = dummydataarm)
lmnulldummy <- lm(arm~1,data=dummydataarm)

dummyarmAIC <- step(lmfitdummy,k=2)
summary(dummyarmAIC)

subset <- regsubsets(formula(lmfitdummy),data = dummydataarm,nvmax = 20,method = 'exhaustive',really.big = T)
subsetvalgt <- summary(subset)$which[,-1]
navne <- names(dummydataarm)[-48]
navne[subsetvalgt[8,]]


CVlm <- function(fit,data,k=10){
  idx <- sample(1:k,nrow(data),replace = T)
  form <- formula(fit)
  response <- as.character(attributes(terms(fit))$variables[[2]])
  CV <- rep(NA,k)
  for (i in 1:k){
    train <- data[idx!=i,]
    test <- data[idx==i,]
    fit <- lm(form,data = train)
    pred <- predict(fit,test)
    obs <- test[,response]
    CV[i] <- rmse(pred,obs)
  }
  return(CV)
}

CVsub <- rep(NA,nrow(subsetvalgt))
sesub <- rep(NA,nrow(subsetvalgt))
AICsub <- rep(NA,nrow(subsetvalgt))
BICsub <- rep(NA,nrow(subsetvalgt))
for(i in 1:nrow(subsetvalgt)){
  dat <- dummydataarm[,c('arm',navne[subsetvalgt[i,]])]
  lm <- lm(arm~.,data=dat)
  CV <- CVlm(lm,dat)
  CVsub[i] <- mean(CV)
  sesub[i] <- (1/sqrt(10))*sd(CV)
  AICsub[i] <- AIC(lm)
  BICsub[i] <- AIC(lm,k=log(nrow(dummydataarm)))
}

CVnull <- CVlm(lmnulldummy,dummydataarm)
CVsub <- c(mean(CVnull),CVsub)
sesub <- c((1/sqrt(10))*sd(CVnull),sesub)
AICsub <- c(AIC(lmnulldummy),AICsub)
BICsub <- c(AIC(lmnulldummy, k=log(nrow(dummydataarm))),BICsub)
ggplot()+geom_point(aes(0:20,AICsub,col='AIC'))+geom_line(aes(0:20,AICsub,col='AIC'))+geom_point(aes(0:20,BICsub,col='BIC'))+geom_line(aes(0:20,BICsub,col='BIC'))+guides(colour=guide_legend(title=''))+ylab('Værdi')+xlab('Antal prædiktorer')+scale_x_continuous(breaks = 0:20)+theme(legend.position = c(0.8,0.8))

ggplot()+geom_line(aes(0:20,CVsub))+geom_point(aes(0:20,CVsub)) + geom_errorbar(aes(x=0:20, ymin=CVsub-sesub, ymax=CVsub+sesub),width=0.25) + geom_hline(yintercept=CVsub[which.min(CVsub)]+sesub[which.min(CVsub)], linetype='dashed')+scale_x_continuous(breaks=0:20)+xlab('Antal prædiktorer')+ylab('Krydsvalideringsscore')

#Vælger 15 og 8

Model <- c('lmarmdummy8','lmarmdummy15','lmarmAIC','lmarmk09')
Krydsvalideringsscore <- c(mean(CVsub[8]), mean(CVsub[15]),mean(CVracelmstepAIC),mean(CVracelmstepk9))
sd <- c(sesub[8], sesub[15],(1/sqrt(10))*sd(CVracelmstepAIC),(1/sqrt(10))*sd(CVracelmstepk9))
Parametre <- c(9,16,26,10)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')


dat <- dummydataarm[,c('arm',navne[subsetvalgt[8,]])]
lm <- lm(arm~.,data=dat)
sum <- summary(lm)$coefficients
konf <- confint(lm)
estimater <- cbind(sum[,1],konf)
print(xtable(estimater),format.args=list(decimal.mark=','))


Model <- c('lassoarm', 'lmarmAIC','lmarmdummy8','postlassolmarm')
Krydsvalideringsscore <- c(mean(CVracelasso), mean(CVracelmstepAIC),CVsub[8],mean(CVracepostlm))
sd <- (1/sqrt(10))*c(sd(CVracelasso),sd(CVracelmstepAIC),sqrt(10)*sesub[8],sd(CVracepostlm))
Parametre <- c(14,26,9,14)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')


Model <- c('CART.fuldarm','CART.cp=026arm', 'lmarmAIC','lmarmdummy8')
Krydsvalideringsscore <- c(mean(CVracefuldcart),mean(CVracecart026),mean(CVracelmstepAIC),CVsub[8])
sd <- (1/sqrt(10))*c(sd(CVracefuldcart),sd(CVracecart026),sd(CVracelmstepAIC),sqrt(10)*sesub[8])
Parametre <- c(15,5,26,9)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')+scale_size_continuous(name = 'Kompleksitet')

