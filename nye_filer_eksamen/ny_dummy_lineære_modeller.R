library(leaps)
library(hydroGOF)
library(ggplot2)
library(glmnet)
#Omkoder referenceniveau
hudcomplete[,2] <- relevel(hudcomplete[,2],ref = 'CT')
hudcomplete[,3] <- relevel(hudcomplete[,3],ref = 'CT')
hudcomplete[,4] <- relevel(hudcomplete[,4],ref = 'CT')
hudcomplete[,5] <- relevel(hudcomplete[,5],ref = 'GA')
hudcomplete[,6] <- relevel(hudcomplete[,6],ref = 'CT')
hudcomplete[,7] <- relevel(hudcomplete[,7],ref = 'GA')
hudcomplete[,8] <- relevel(hudcomplete[,8],ref = 'AT')
hudcomplete[,9] <- relevel(hudcomplete[,9],ref = 'CA')
hudcomplete[,10] <- relevel(hudcomplete[,10],ref = 'GA')
hudcomplete[,11] <- relevel(hudcomplete[,11],ref = 'GC')
hudcomplete[,12] <- relevel(hudcomplete[,12],ref = 'CT')
hudcomplete[,13] <- relevel(hudcomplete[,13],ref = 'GA')
hudcomplete[,14] <- relevel(hudcomplete[,14],ref = 'GA')
hudcomplete[,15] <- relevel(hudcomplete[,15],ref = 'GC')
hudcomplete[,16] <- relevel(hudcomplete[,16],ref = 'CT')
hudcomplete[,17] <- relevel(hudcomplete[,17],ref = 'CT')
hudcomplete[,18] <- relevel(hudcomplete[,18],ref = 'CT')
hudcomplete[,19] <- relevel(hudcomplete[,19],ref = 'GA')
hudcomplete[,20] <- relevel(hudcomplete[,20],ref = 'GA')
hudcomplete[,21] <- relevel(hudcomplete[,21],ref = 'CT')
hudcomplete[,22] <- relevel(hudcomplete[,22],ref = 'GT')
hudcomplete[,23] <- relevel(hudcomplete[,23],ref = 'GC')
hudcomplete[,24] <- relevel(hudcomplete[,24],ref = 'GT')

#Gennemfører analysen med manuel dummykodning igen
lmfit <- lm(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=hudcomplete)

x <- model.matrix(lmfit)
dummydataarm <- data.frame(cbind(x[,-1],hudcomplete[,'arm']))
names(dummydataarm)[48] <- 'arm'

lmfitdummy <- lm(arm~.,data = dummydataarm)
lmnulldummy <- lm(arm~1,data=dummydataarm)


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

lmarmdummy8 <- lm(arm~rs129GA+rs139CT+rs142G+rs142GA+rs442T+rs611G+rs611GC+AFR, data=dummydataarmgammel)
CVdummy8 <- CVlm(lmarmdummy8,dummydataarmgammel)

lmarmdummy15 <- lm(arm~rs108CT+rs108T+rs123G+rs129G+rs129GA+rs132G+rs139CT+rs140G+rs142G+rs142GA+rs168GC+rs442T+rs611G+rs611GC+AFR, data=dummydataarmgammel)
CVdummy15 <- CVlm(lmarmdummy15,dummydataarmgammel)

lmarmAIC <- lm(arm~rs108+rs112+rs123+rs129+rs132+rs139+rs140+rs142+rs168+rs242+rs442+rs611+AFR, data = hudcomplete)
CVracelmstepAIC <- CVlm(lmarmAIC,hudcomplete)

lmarmk09 <- lm(arm~rs139+rs142+rs442+rs611+AFR, data=hudcomplete)
CVracelmstepk9 <- CVlm(lmarmk09,hudcomplete)

Model <- c('lmarmdummy8','lmarmdummy15','lmarmAIC','lmarmk09')
modelny <- c('lmdummyny8','lmdummyny15')
Krydsvalideringsscore <- c(mean(CVdummy8), mean(CVdummy15),mean(CVracelmstepAIC),mean(CVracelmstepk9))
krydsny <- c(CVsub[8],CVsub[15])
sd <- c((1/sqrt(10))*sd(CVdummy8),(1/sqrt(10))*sd(CVdummy15) ,(1/sqrt(10))*sd(CVracelmstepAIC),(1/sqrt(10))*sd(CVracelmstepk9))
sdny <- c(sesub[8],sesub[15])
Parametre <- c(9,16,26,10)
parny <- c(9,16)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + 
  geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + 
  geom_point(aes(modelny,krydsny,size=parny), col='blue')+
  geom_errorbar(aes(x=modelny, ymin=krydsny-sdny, ymax=krydsny+sdny),width=0.25, col='blue')+
  geom_hline(yintercept=c(Krydsvalideringsscore,krydsny)[which.min(c(Krydsvalideringsscore,krydsny))]+c(sd,sdny)[which.min(c(Krydsvalideringsscore,krydsny))], linetype='dashed')
  



#Kører lasso igen
#Finder optimal værdi af lambda
x <- model.matrix(lmfit)
x <- x[,-1]
cv <- cv.glmnet(x,hudcomplete$arm, type.measure = 'mse')
plot(cv)
#lambda der giver mindste mse i krydsvalidering
cv$lambda.min
#største lambda der giver krydsvalidering inden for en standardafvigelse af mindste cv
cv$lambda.1se
coef(cv, s=0.2378298)

## Tabel over coef
coef<-c(coef(cv, s=0.2378298)[,1])
coef <- coef[coef!=0]
xtable(data.frame((coef)))

##Laver krydsvalidering for lasso
s <- 0.2378298
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


##Fitter lm med samme variable og laver krydsvalidering
postlassofit <- lm(arm~rs108C+rs108T+rs123G+rs129A+rs139C+rs142G+rs142A+rs168G+rs442T+rs611G+rs611C+EUR+AFR, data = dummydataarm)
CVpostlmny <- CVlm(postlassofit,dummydataarm)

s <- 0.1961425
CVracelassogammel <- rep(0,10)
idx <- sample(1:10,nrow(hudcompletegammel), replace=T)
for (i in 1:10){
  train <- hudcompletegammel[idx!=i,]
  test <- hudcompletegammel[idx==i,]
  xtrain <- model.matrix(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=train)
  xtest <- model.matrix(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=test)
  fit <- glmnet(xtrain[,-1], train$arm, lambda = s)
  pred <- as.vector(predict(fit,xtest[,-1], type='link'))
  CVracelassogammel[i] <- rmse(pred,test$arm)
}

postlassolmarm <- lm(arm~rs108T+rs123G+rs129GA+rs132G+rs139CT+rs140G+rs142G+rs142GA+rs168GC+rs442T+rs611G+EUR+AFR,data=dummydataarmgammel)
CVracepostlm <- CVlm(postlassolmarm,dummydataarmgammel)

Model <- c('lassoarm', 'lmarmAIC','lmarmdummy8','postlassolmarm')
modelny <- c('lassony','postlassony')
Krydsvalideringsscore <- c(mean(CVracelassogammel), mean(CVracelmstepAIC),mean(CVdummy8),mean(CVracepostlm))
krydsny <- c(mean(CVracelasso),mean(CVpostlmny))
sd <- (1/sqrt(10))*c(sd(CVracelassogammel),sd(CVracelmstepAIC),sd(CVdummy8),sd(CVracepostlm))
sdny <- (1/sqrt(10))*c(sd(CVracelasso),sd(CVpostlmny))
Parametre <- c(14,26,9,14)
parny <- c(14,14)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) +
  geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + 
  geom_point(aes(modelny,krydsny,size=parny), col='blue')+
  geom_errorbar(aes(x=modelny, ymin=krydsny-sdny, ymax=krydsny+sdny),width=0.25, col='blue')+
  geom_hline(yintercept=c(Krydsvalideringsscore,krydsny)[which.min(c(Krydsvalideringsscore,krydsny))]+c(sd,sdny)[which.min(c(Krydsvalideringsscore,krydsny))], linetype='dashed')
