library(rpart)
library(rpart.plot)
library(ggplot2)
library(nnet)
library(NeuralNetTools)
library(hydroGOF)
library(lme4)

#Laver hud data
hudarm <- hudcomplete[,-c(28,30)]
hudarm$sted <- 'arm'
names(hudarm)[28] <- 'pigmentering'

hudballe <- hudcomplete[,-c(29,30)]
hudballe$sted <- 'balle'
names(hudballe)[28] <- 'pigmentering'


hudpande <- hudcomplete[,-c(28,29)]
hudpande$sted <- 'pande'
names(hudpande)[28] <- 'pigmentering'

hud <- rbind(hudarm,hudballe,hudpande)
hud$sted <- factor(hud$sted)

#Fitter træ
hudcart<- rpart(pigmentering~sted+rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=hud, minsplit=2)
rpart.plot(hudcart)
plotcp(hudcart)
arm_014<-prune.rpart(hudcart, cp=0.014)
rpart.plot(arm_014)


CVracecart014 <- rep(0,10)
idx <- sample(1:10,nrow(hud), replace=T)
for (i in 1:10){
  train <- hud[idx!=i,]
  test <- hud[idx==i,]
  fit1 <- rpart(pigmentering~sted+rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=train, minsplit=2)
  fit <- prune.rpart(fit1, cp=0.014)
  pred <- predict(fit,test)
  CVracecart014[i] <- rmse(pred,test$pigmentering)
}

#Fitter neuralt net
size <- 1:10
CV <- rep(0,length(size))
sd <- rep(0,length(size))
for(j in 1:length(size)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hud), replace=T)
  for (i in 1:10){
    train <- hud[idx!=i,]
    test <- hud[idx==i,]
    fit <- nnet(pigmentering ~ sted + rs101 + rs107 + rs108 + rs112 + rs122 + rs123 + rs126 + rs128 + rs129 + rs132 + rs139 + rs140 + rs142 + rs168 + rs180 + rs203 + rs242 + rs267 + rs442 + rs491 + rs611 + rs674 + køn + EUR + AFR,data=train,size=size[j],linout=TRUE,skip=FALSE,decay=0)
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$pigmentering)
  }
  CV[j] <- mean(CVnn)
  sd[j] <- sd(CVnn)
}

ggplot()+xlab('Antal knuder i skjult lag')+ylab('Krydsvalideringsscore')+geom_point(aes(factor(size),CV)) + geom_errorbar(aes(x=size, ymin=CV-(1/(sqrt(10)))*sd, ymax=CV+(1/(sqrt(10)))*sd),width=0.25) + geom_hline(yintercept=CV[which.min(CV)]+(1/(sqrt(10)))*sd[which.min(CV)], linetype='dashed')

decay <- seq(0,0.5,0.01)
CVfuldnn <- rep(0,length(decay))
sdfuldnn <- rep(0,length(decay))
for(j in 1:length(decay)){
  CVfuldnnnn <- rep(0,10)
  idx <- sample(1:10,nrow(hud), replace=T)
  for (i in 1:10){
    train <- hud[idx!=i,]
    test <- hud[idx==i,]
    fit <- nnet(pigmentering ~ sted + rs101 + rs107 + rs108 + rs112 + rs122 + rs123 + rs126 + rs128 + rs129 + rs132 + rs139 + rs140 + rs142 + rs168 + rs180 + rs203 + rs242 + rs267 + rs442 + rs491 + rs611 + rs674 + køn + EUR + AFR,data=train,size=6,linout=TRUE,skip=FALSE,decay=decay[j])
    pred <- c(predict(fit,test))
    CVfuldnnnn[i] <- rmse(pred,test$pigmentering)
  }
  CVfuldnn[j] <- mean(CVfuldnnnn)
  sdfuldnn[j] <- sd(CVfuldnnnn)
}

Decay <- decay
ggplot()+xlab('lambda-parameter')+ylab('Krydsvalideringsscore')+geom_point(aes(Decay,CVfuldnn)) + geom_errorbar(aes(x=Decay, ymin=CVfuldnn-(1/(sqrt(10)))*sdfuldnn, ymax=CVfuldnn+(1/(sqrt(10)))*sdfuldnn),width=0.01) + geom_hline(yintercept=CVfuldnn[which.min(CVfuldnn)]+(1/(sqrt(10)))*sdfuldnn[which.min(CVfuldnn)], linetype='dashed')+guides(size=guide_legend(title="Antal vægte"))

size <- 1:10
CV <- rep(0,length(size))
sd <- rep(0,length(size))
for(j in 1:length(size)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hud), replace=T)
  for (i in 1:10){
    train <- hud[idx!=i,]
    test <- hud[idx==i,]
    fit <- nnet(pigmentering ~ sted + rs108 + rs122 + rs123 + rs129 + rs139 + rs140 + rs142 + rs168 + rs180 + rs242 + rs442 + rs611 + EUR + AFR,data=train,size=size[j],linout=TRUE,skip=FALSE,decay=0)
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$pigmentering)
  }
  CV[j] <- mean(CVnn)
  sd[j] <- sd(CVnn)
}

ggplot()+xlab('Antal knuder i skjult lag')+ylab('Krydsvalideringsscore')+geom_point(aes(factor(size),CV)) + geom_errorbar(aes(x=size, ymin=CV-(1/(sqrt(10)))*sd, ymax=CV+(1/(sqrt(10)))*sd),width=0.25) + geom_hline(yintercept=CV[which.min(CV)]+(1/(sqrt(10)))*sd[which.min(CV)], linetype='dashed')

decay <- seq(0,0.5,0.01)
CVfuldnnAIC <- rep(0,length(decay))
sdfuldnnAIC <- rep(0,length(decay))
for(j in 1:length(decay)){
  CVfuldnnnn <- rep(0,10)
  idx <- sample(1:10,nrow(hud), replace=T)
  for (i in 1:10){
    train <- hud[idx!=i,]
    test <- hud[idx==i,]
    fit <- nnet(pigmentering ~ sted + rs108 + rs122 + rs123 + rs129 + rs139 + rs140 + rs142 + rs168 + rs180 + rs242 + rs442 + rs611 + EUR + AFR,data=train,size=7,linout=TRUE,skip=FALSE,decay=decay[j])
    pred <- c(predict(fit,test))
    CVfuldnnnn[i] <- rmse(pred,test$pigmentering)
  }
  CVfuldnnAIC[j] <- mean(CVfuldnnnn)
  sdfuldnnAIC[j] <- sd(CVfuldnnnn)
}

Decay <- decay
ggplot()+xlab('lambda-parameter')+ylab('Krydsvalideringsscore')+geom_point(aes(Decay,CVfuldnnAIC)) + geom_errorbar(aes(x=Decay, ymin=CVfuldnnAIC-(1/(sqrt(10)))*sdfuldnnAIC, ymax=CVfuldnnAIC+(1/(sqrt(10)))*sdfuldnnAIC),width=0.01) + geom_hline(yintercept=CVfuldnnAIC[which.min(CVfuldnnAIC)]+(1/(sqrt(10)))*sdfuldnnAIC[which.min(CVfuldnnAIC)], linetype='dashed')+guides(size=guide_legend(title="Antal vægte"))


size <- 1:10
CV <- rep(0,length(size))
sd <- rep(0,length(size))
for(j in 1:length(size)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hud), replace=T)
  for (i in 1:10){
    train <- hud[idx!=i,]
    test <- hud[idx==i,]
    fit <- nnet(pigmentering ~ sted + rs129 + rs142 + rs442 + rs611 + EUR + AFR,data=train,size=size[j],linout=TRUE,skip=FALSE,decay=0)
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$pigmentering)
  }
  CV[j] <- mean(CVnn)
  sd[j] <- sd(CVnn)
}

ggplot()+xlab('Antal knuder i skjult lag')+ylab('Krydsvalideringsscore')+geom_point(aes(factor(size),CV)) + geom_errorbar(aes(x=size, ymin=CV-(1/(sqrt(10)))*sd, ymax=CV+(1/(sqrt(10)))*sd),width=0.25) + geom_hline(yintercept=CV[which.min(CV)]+(1/(sqrt(10)))*sd[which.min(CV)], linetype='dashed')

decay <- seq(0,0.5,0.01)
CVfuldnnBIC <- rep(0,length(decay))
sdfuldnnBIC <- rep(0,length(decay))
for(j in 1:length(decay)){
  CVfuldnnnn <- rep(0,10)
  idx <- sample(1:10,nrow(hud), replace=T)
  for (i in 1:10){
    train <- hud[idx!=i,]
    test <- hud[idx==i,]
    fit <- nnet(pigmentering ~ sted + rs129 + rs142 + rs442 + rs611 + EUR + AFR,data=train,size=4,linout=TRUE,skip=FALSE,decay=decay[j])
    pred <- c(predict(fit,test))
    CVfuldnnnn[i] <- rmse(pred,test$pigmentering)
  }
  CVfuldnnBIC[j] <- mean(CVfuldnnnn)
  sdfuldnnBIC[j] <- sd(CVfuldnnnn)
}

Decay <- decay
ggplot()+xlab('lambda-parameter')+ylab('Krydsvalideringsscore')+geom_point(aes(Decay,CVfuldnnBIC)) + geom_errorbar(aes(x=Decay, ymin=CVfuldnnBIC-(1/(sqrt(10)))*sdfuldnnBIC, ymax=CVfuldnnBIC+(1/(sqrt(10)))*sdfuldnnBIC),width=0.01) + geom_hline(yintercept=CVfuldnnBIC[which.min(CVfuldnnBIC)]+(1/(sqrt(10)))*sdfuldnnBIC[which.min(CVfuldnnBIC)], linetype='dashed')+guides(size=guide_legend(title="Antal vægte"))

#Mixed modeller
CVmixed <- function(fit,data,k=10){
  idx <- sample(1:k,nrow(data),replace = T)
  form <- formula(fit)
  response <- as.character(attributes(terms(fit))$variables[[2]])
  CV <- rep(NA,k)
  for (i in 1:k){
    train <- data[idx!=i,]
    test <- data[idx==i,]
    fit <- lmer(form,data = train)
    pred <- predict(fit,test, re.form=NA)
    obs <- test[,response]
    CV[i] <- rmse(pred,obs)
  }
  return(CV)
}

MMAIC <- lmer(pigmentering~EUR+AFR+rs108+rs122+rs123+rs129+rs139+rs140+rs142+rs168+rs180+rs242+rs442+rs611+sted+(1|Patient.ID),data=hud)
CVMMAIC <- CVmixed(MMAIC,hud)

MMBIC <- lmer(pigmentering~EUR+AFR+rs129+rs142+rs442+rs611+sted+(1|Patient.ID),data=hud)
CVBIC <- CVmixed(MMBIC,hud)

Model <- c('MMAIC','MMBIC','CART','NN','NNBIC','NNAIC')
Mean <- c(mean(CVMMAIC),mean(CVBIC),mean(CVracecart014),CVfuldnn[37],CVfuldnnBIC[19],CVfuldnnAIC[16])
sd <- (1/(sqrt(10)))*c(sd(CVMMAIC),sd(CVBIC),sd(CVracecart014),sdfuldnn[37],sdfuldnnBIC[19],sdfuldnnAIC[16])
Parametre <- c(27,11,9,307,57,211)
ggplot()+geom_point(aes(Model,Mean, size=Parametre))+geom_errorbar(aes(x=Model, ymin=Mean-sd, ymax=Mean+sd),width=0.25) + ylab('Krydsvalideringsscore')+geom_hline(yintercept=Mean[which.min(Mean)]+sd[which.min(Mean)], linetype='dashed')
