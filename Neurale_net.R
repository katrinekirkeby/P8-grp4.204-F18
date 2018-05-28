library(nnet)
library(NeuralNetTools)
library(hydroGOF)
library(ggplot2)

par(mar=c(0.2,0.2,0.2,0.2),mfrow=c(1,1))
nn<-nnet(arm ~ rs101 + rs107 + rs108 + rs112 + rs122 + rs123 + rs126 + rs128 + rs129 + rs132 + rs139 + rs140 + rs142 + rs168 + rs180 + rs203 + rs242 + rs267 + rs442 + rs491 + rs611 + rs674 + køn + EUR + AFR, data=hudcomplete,size=0, linout=TRUE,skip=TRUE)
plotnet(nn)
nnlille<- nnet(arm~rs142+AFR+EUR, data=hudcomplete,size=0,linout=TRUE,skip=TRUE)
plotnet(nnlille,skip=TRUE)

nnred4s2 <- nnet(arm ~rs142 + rs442 + EUR + AFR, data=hudcomplete,size=2, linout=TRUE,skip=FALSE)
plotnet(nnred4s2)
####Krydsvalidering med size 

#De bedste lineære modeller (med interaktion)
CVracelmstepAIC <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs108 + rs112 + rs129 + rs139 + rs140 +
               rs168 +rs180+ rs242 + rs442 + rs611+køn+EUR + AFR*rs142, data=train)
  pred <- predict(fit,test)
  CVracelmstepAIC[i] <- rmse(pred,test$arm)
}



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

lmfulddummy <- lm(arm~.+AFR*rs142G+AFR*rs142GA, data = dummydataarm)
stepdummyk8 <- step(lmfulddummy, k=8)
CVk8 <- CVlm(stepdummyk8,dummydataarm)


# Prøver at vælge decay og size for fuldnn
size <- 1:10
CV <- rep(0,length(size))
sd <- rep(0,length(size))
for(j in 1:length(size)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hudcomplete), replace=T)
  for (i in 1:10){
    train <- hudcomplete[idx!=i,]
    test <- hudcomplete[idx==i,]
    fit <- nnet(arm ~ rs101 + rs107 + rs108 + rs112 + rs122 + rs123 + rs126 + rs128 + rs129 + rs132 + rs139 + rs140 + rs142 + rs168 + rs180 + rs203 + rs242 + rs267 + rs442 + rs491 + rs611 + rs674 + køn + EUR + AFR,data=train,size=size[j],linout=TRUE,skip=FALSE,decay=0)
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$arm)
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
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- nnet(arm ~ rs101 + rs107 + rs108 + rs112 + rs122 + rs123 + rs126 + rs128 + rs129 + rs132 + rs139 + rs140 + rs142 + rs168 + rs180 + rs203 + rs242 + rs267 + rs442 + rs491 + rs611 + rs674 + køn + EUR + AFR,data=train,size=1,linout=TRUE,skip=FALSE,decay=decay[j])
  pred <- c(predict(fit,test))
  CVfuldnnnn[i] <- rmse(pred,test$arm)
}
CVfuldnn[j] <- mean(CVfuldnnnn)
sdfuldnn[j] <- sd(CVfuldnnnn)
}

Decay <- decay
ggplot()+xlab('lambda-parameter')+ylab('Krydsvalideringsscore')+geom_point(aes(Decay,CVfuldnn)) + geom_errorbar(aes(x=Decay, ymin=CVfuldnn-(1/(sqrt(10)))*sdfuldnn, ymax=CVfuldnn+(1/(sqrt(10)))*sdfuldnn),width=0.01) + geom_hline(yintercept=CVfuldnn[which.min(CVfuldnn)]+(1/(sqrt(10)))*sdfuldnn[which.min(CVfuldnn)], linetype='dashed')+guides(size=guide_legend(title="Antal vægte"))

#Konkludere at der kun skal være en knude i det skjulte lag og vælger decay=0.3.

##Prøver at vælge decay og size for nn med samme prædiktorer som AICarm modellen
size <- 1:10
CV <- rep(0,length(size))
sd <- rep(0,length(size))
for(j in 1:length(size)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hudcomplete), replace=T)
  for (i in 1:10){
    train <- hudcomplete[idx!=i,]
    test <- hudcomplete[idx==i,]
    fit <- nnet(arm ~ rs108 + rs112 + rs123 + rs129 + rs132 + rs139 + rs140 + rs142 + rs168 + rs242 + rs442 + rs611 + AFR,data=train,size=size[j],linout=TRUE,skip=FALSE,decay=0)
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$arm)
  }
  CV[j] <- mean(CVnn)
  sd[j] <- sd(CVnn)
}

ggplot()+xlab('Antal knuder i skjult lag')+ylab('Krydsvalideringsscore')+geom_point(aes(size,CV)) + geom_errorbar(aes(x=size, ymin=CV-(1/(sqrt(10)))*sd, ymax=CV+(1/(sqrt(10)))*sd),width=0.25) + geom_hline(yintercept=CV[which.min(CV)]+(1/(sqrt(10)))*sd[which.min(CV)], linetype='dashed')

# to knuder


decay <- seq(0,0.5,0.01)
CVAICnn <- rep(0,length(decay))
sdAICnn <- rep(0,length(decay))
for(j in 1:length(decay)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hudcomplete), replace=T)
  for (i in 1:10){
    train <- hudcomplete[idx!=i,]
    test <- hudcomplete[idx==i,]
    fit <- nnet(arm ~ rs108 + rs112 + rs123 + rs129 + rs132 + rs139 + rs140 + rs142 + rs168 + rs242 + rs442 + rs611 + AFR,data=train,size=2,linout=TRUE,skip=FALSE,decay=decay[j])
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$arm)
  }
  CVAICnn[j] <- mean(CVnn)
  sdAICnn[j] <- sd(CVnn)
}

Decay <- decay
ggplot()+xlab('Decay parameter')+ylab('Krydsvalideringsscore')+geom_point(aes(Decay,CVAICnn)) + geom_errorbar(aes(x=Decay, ymin=CVAICnn-(1/(sqrt(10)))*sdAICnn, ymax=CVAICnn+(1/(sqrt(10)))*sdAICnn),width=0.01) + geom_hline(yintercept=CVAICnn[which.min(CVAICnn)]+(1/(sqrt(10)))*sdAICnn[which.min(CVAICnn)], linetype='dashed')+guides(size=guide_legend(title="Antal vægte"))
#Kan bare vælge 0.3 igen


##Prøver at vælge decay og size for nn med samme prædiktorer som BICarm modellen
size <- 1:10
CV <- rep(0,length(size))
sd <- rep(0,length(size))
for(j in 1:length(size)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hudcomplete), replace=T)
  for (i in 1:10){
    train <- hudcomplete[idx!=i,]
    test <- hudcomplete[idx==i,]
    fit <- nnet(arm ~ rs129 + rs139 + rs142 + rs442 + rs611 + AFR,data=train,size=size[j],linout=TRUE,skip=FALSE,decay=0)
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$arm)
  }
  CV[j] <- mean(CVnn)
  sd[j] <- sd(CVnn)
}

ggplot()+xlab('Antal knuder i skjult lag')+ylab('Krydsvalideringsscore')+geom_point(aes(size,CV)) + geom_errorbar(aes(x=size, ymin=CV-sd, ymax=CV+sd),width=0.25) + geom_hline(yintercept=CV[which.min(CV)]+sd[which.min(CV)], linetype='dashed')

# Ikke mere end en knude


decay <- seq(0,0.5,0.01)
CVBICnn <- rep(0,length(decay))
sdBICnn <- rep(0,length(decay))
for(j in 1:length(decay)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hudcomplete), replace=T)
  for (i in 1:10){
    train <- hudcomplete[idx!=i,]
    test <- hudcomplete[idx==i,]
    fit <- nnet(arm ~ rs129+rs139 + rs142 + rs442 + rs611 + AFR,data=train,size=1,linout=TRUE,skip=FALSE,decay=decay[j])
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$arm)
  }
  CVBICnn[j] <- mean(CVnn)
  sdBICnn[j] <- sd(CVnn)
}

Decay <- decay
ggplot()+xlab('Decay parameter')+ylab('Krydsvalideringsscore')+geom_point(aes(Decay,CVBICnn)) + geom_errorbar(aes(x=Decay, ymin=CVBICnn-sdBICnn, ymax=CVBICnn+sdBICnn),width=0.01) + geom_hline(yintercept=CVBICnn[which.min(CVBICnn)]+sdBICnn[which.min(CVBICnn)], linetype='dashed')+guides(size=guide_legend(title="Antal vægte"))
#Kan bare vælge 0.11 igen

#AIC inter
size <- 1:10
CV <- rep(0,length(size))
sd <- rep(0,length(size))
for(j in 1:length(size)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hudcomplete), replace=T)
  for (i in 1:10){
    train <- hudcomplete[idx!=i,]
    test <- hudcomplete[idx==i,]
    fit <- nnet(arm ~ rs108+rs112+rs129+rs139+rs140+rs168+rs180+rs242+rs442 +rs611+køn+EUR + AFR*rs142,data=train,size=size[j],linout=TRUE,skip=FALSE,decay=0)
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$arm)
  }
  CV[j] <- mean(CVnn)
  sd[j] <- sd(CVnn)
}

ggplot()+xlab('Antal knuder i skjult lag')+ylab('Krydsvalideringsscore')+geom_point(aes(size,CV)) + geom_errorbar(aes(x=size, ymin=CV-(1/(sqrt(10)))*sd, ymax=CV+(1/(sqrt(10)))*sd),width=0.25) + geom_hline(yintercept=CV[which.min(CV)]+(1/(sqrt(10)))*sd[which.min(CV)], linetype='dashed')

# Ikke mere end en knude


decay <- seq(0,0.5,0.01)
CVAICinternn <- rep(0,length(decay))
sdAICinternn <- rep(0,length(decay))
for(j in 1:length(decay)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hudcomplete), replace=T)
  for (i in 1:10){
    train <- hudcomplete[idx!=i,]
    test <- hudcomplete[idx==i,]
    fit <- nnet(arm ~ rs108+rs112+rs129+rs139+rs140+rs168+rs180+rs242+rs442 +rs611+køn+EUR + AFR*rs142,data=train,size=1,linout=TRUE,skip=FALSE,decay=decay[j])
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$arm)
  }
  CVAICinternn[j] <- mean(CVnn)
  sdAICinternn[j] <- sd(CVnn)
}

Decay <- decay
ggplot()+xlab('Decay parameter')+ylab('Krydsvalideringsscore')+geom_point(aes(Decay,CVAICinternn)) + geom_errorbar(aes(x=Decay, ymin=CVAICinternn-(1/(sqrt(10)))*sdAICinternn, ymax=CVAICinternn+(1/(sqrt(10)))*sdAICinternn),width=0.01) + geom_hline(yintercept=CVAICinternn[which.min(CVAICinternn)]+(1/(sqrt(10)))*sdAICinternn[which.min(CVAICinternn)], linetype='dashed')+guides(size=guide_legend(title="Antal vægte"))
#Vælger decay=0.3


#Armk9
size <- 1:10
CV <- rep(0,length(size))
sd <- rep(0,length(size))
for(j in 1:length(size)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hudcomplete), replace=T)
  for (i in 1:10){
    train <- hudcomplete[idx!=i,]
    test <- hudcomplete[idx==i,]
    fit <- nnet(arm ~ rs139+rs142+rs442+rs611+ AFR,data=train,size=size[j],linout=TRUE,skip=FALSE,decay=0)
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$arm)
  }
  CV[j] <- mean(CVnn)
  sd[j] <- sd(CVnn)
}

ggplot()+xlab('Antal knuder i skjult lag')+ylab('Krydsvalideringsscore')+geom_point(aes(size,CV)) + geom_errorbar(aes(x=size, ymin=CV-(1/(sqrt(10)))*sd, ymax=CV+(1/(sqrt(10)))*sd),width=0.25) + geom_hline(yintercept=CV[which.min(CV)]+(1/(sqrt(10)))*sd[which.min(CV)], linetype='dashed')

# Vælger 3 knuder


decay <- seq(0,0.5,0.01)
CVk10 <- rep(0,length(decay))
sdk10 <- rep(0,length(decay))
for(j in 1:length(decay)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hudcomplete), replace=T)
  for (i in 1:10){
    train <- hudcomplete[idx!=i,]
    test <- hudcomplete[idx==i,]
    fit <- nnet(arm ~ rs139+rs142+rs442+rs611+ AFR,data=train,size=3,linout=TRUE,skip=FALSE,decay=decay[j])
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$arm)
  }
  CVk10[j] <- mean(CVnn)
  sdk10[j] <- sd(CVnn)
}

Decay <- decay
ggplot()+xlab('Decay parameter')+ylab('Krydsvalideringsscore')+geom_point(aes(Decay,CVk10)) + geom_errorbar(aes(x=Decay, ymin=CVk10-(1/(sqrt(10)))*sdk10, ymax=CVk10+(1/(sqrt(10)))*sdk10),width=0.01) + geom_hline(yintercept=CVk10[which.min(CVk10)]+(1/(sqrt(10)))*sdk10[which.min(CVk10)], linetype='dashed')+guides(size=guide_legend(title="Antal vægte"))

#interk8
size <- 1:10
CV <- rep(0,length(size))
sd <- rep(0,length(size))
for(j in 1:length(size)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hudcomplete), replace=T)
  for (i in 1:10){
    train <- hudcomplete[idx!=i,]
    test <- hudcomplete[idx==i,]
    fit <- nnet(arm ~ rs129 + rs139+rs442+rs611 + AFR*rs142,data=train,size=size[j],linout=TRUE,skip=FALSE,decay=0)
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$arm)
  }
  CV[j] <- mean(CVnn)
  sd[j] <- sd(CVnn)
}

ggplot()+xlab('Antal knuder i skjult lag')+ylab('Krydsvalideringsscore')+geom_point(aes(size,CV)) + geom_errorbar(aes(x=size, ymin=CV-(1/(sqrt(10)))*sd, ymax=CV+(1/(sqrt(10)))*sd),width=0.25) + geom_hline(yintercept=CV[which.min(CV)]+(1/(sqrt(10)))*sd[which.min(CV)], linetype='dashed')

# Vælger 2 knuder


decay <- seq(0,0.5,0.01)
CVinterk8 <- rep(0,length(decay))
sdinterk8 <- rep(0,length(decay))
for(j in 1:length(decay)){
  CVnn <- rep(0,10)
  idx <- sample(1:10,nrow(hudcomplete), replace=T)
  for (i in 1:10){
    train <- hudcomplete[idx!=i,]
    test <- hudcomplete[idx==i,]
    fit <- nnet(arm ~rs129+rs139+rs442+rs611 + AFR*rs142,data=train,size=2,linout=TRUE,skip=FALSE,decay=decay[j])
    pred <- c(predict(fit,test))
    CVnn[i] <- rmse(pred,test$arm)
  }
  CVinterk8[j] <- mean(CVnn)
  sdinterk8[j] <- sd(CVnn)
}

Decay <- decay
ggplot()+xlab('Decay parameter')+ylab('Krydsvalideringsscore')+geom_point(aes(Decay,CVinterk8)) + geom_errorbar(aes(x=Decay, ymin=CVinterk8-(1/(sqrt(10)))*sdinterk8, ymax=CVinterk8+(1/(sqrt(10)))*sdinterk8),width=0.01) + geom_hline(yintercept=CVinterk8[which.min(CVinterk8)]+(1/(sqrt(10)))*sdinterk8[which.min(CVinterk8)], linetype='dashed')+guides(size=guide_legend(title="Antal vægte"))

# Sammenligning med bedste modeller
Model <- factor(c('interarmAIC','interarmdummyk8','fuldNNarm','NNarmAIC','NNinterarmAIC','NNarmk9','NNinterarmk8'),levels = c('interarmAIC','interarmdummyk8','fuldNNarm','NNarmAIC','NNinterarmAIC','NNarmk9','NNinterarmk8'))
Mean <- c(mean(CVracelmstepAIC),mean(CVk8),CVfuldnn[31],CVAICnn[31],CVAICinternn[31],CVk10[31],CVinterk8[31])
sd <- (1/(sqrt(10)))*c(sd(CVracelmstepAIC),sd(CVk8),sdfuldnn[31],sdAICnn[31],sdAICinternn[31],sdk10[31],sdinterk8[31])
Parametre <- c(28,12,50,55,30,34,31)
ggplot()+ylab('Krydsvalideringsscore')+geom_point(aes(Model,Mean, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Mean-sd, ymax=Mean+sd),width=0.25) + geom_hline(yintercept=Mean[which.min(Mean)]+sd[which.min(Mean)], linetype='dashed')+guides(size=guide_legend(title="Parametre"))
