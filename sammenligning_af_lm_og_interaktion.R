library(glmnet)
library(hydroGOF)
library(rpart)
library(rpart.plot)
library(car)
library(xtable)
library(ggplot2)
######################## Almindelig lineære modeller ########################
lmfit <- lm(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=hudcomplete)


lmstepAIC <- step(lmfit)
lmstepAIC$call

CVracelmstepAIC <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs108 + rs112 + rs123 + rs129 + rs132 + rs139 + rs140 +
              rs142 + rs168 + rs242 + rs442 + rs611 + AFR, data=train)
  pred <- predict(fit,test)
  CVracelmstepAIC[i] <- rmse(pred,test$arm)
}


#Bedste dummy
lmfit <- lm(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR, data=hudcomplete)

x <- model.matrix(lmfit)
dummydataarm <- data.frame(cbind(x[,-1],hudcomplete[,'arm']))
names(dummydataarm)[48] <- 'arm'

lmarmdummy8 <- lm(arm~rs129GA+rs139CT+rs142G+rs142GA+rs442T+rs611G+rs611GC+AFR, data = dummydataarm)

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

CVdummy8 <- CVlm(lmarmdummy8,dummydataarm)

######################## Interaktions modeller ##############################
lm.inter<- lm(arm~rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+køn+EUR+AFR*rs142, data=hudcomplete)


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

stepBIC <- step(lm.inter,k=log(nrow(hudcomplete)))
stepBIC$call

stepk7 <- step(lm.inter,k=7)
stepk8 <- step(lm.inter,k=8)
stepk9 <- step(lm.inter,k=9)
stepk11 <- step(lm.inter,k=11)


CVinterBIC <- CVlm(stepBIC,hudcomplete)
CVinterk7 <- CVlm(stepk7,hudcomplete)
CVinterk8 <- CVlm(stepk8,hudcomplete)
CVinterk9 <- CVlm(stepk9,hudcomplete)
CVinterk11 <- CVlm(stepk11,hudcomplete)



######################## CV Plot ###################################

Model <- c('interarmAIC','interarmk9','lmarmAIC','lmarmdummy8','interarmBIC','interarmk8','interarmk11')
Krydsvalideringsscore <- c(mean(CVlminterstepAIC),mean(CVinterk9),mean(CVracelmstepAIC),mean(CVdummy8),mean(CVinterBIC),mean(CVinterk8),mean(CVinterk11))
sd <- (1/sqrt(10))*c(sd(CVlminterstepAIC),sd(CVinterk9),sd(CVracelmstepAIC),sd(CVdummy8),sd(CVinterBIC),sd(CVinterk8),sd(CVinterk11))
Parametre <- c(28,12,26,9,15,14,10)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')

coef <- summary(stepk8)$coefficients
konf <- confint(stepk8)
estimater <- cbind(coef[,1],konf)
print(xtable(estimater),format.args=list(decimal.mark=','))
########################Dummy#######################################

summary(lmstepAIC)

lmfulddummy <- lm(arm~.+AFR*rs142G+AFR*rs142GA, data = dummydataarm)
summary(lmfulddummy)
stepdummyAIC <- step(lmfulddummy)
stepdummyAIC$call

stepdummyBIC <- step(lmfulddummy, k=log(nrow(dummydataarm)))
stepdummyBIC$call

stepdummyk7 <- step(lmfulddummy, k=7)
stepdummyk7$call

stepdummyk8 <- step(lmfulddummy, k=8)
stepdummyk8$call

stepdummyk9 <- step(lmfulddummy, k=9)
stepdummyk9$call

CVAIC <- CVlm(stepdummyAIC,dummydataarm)
CVBIC <- CVlm(stepdummyBIC,dummydataarm)
CVk7 <- CVlm(stepdummyk7,dummydataarm)
CVk8 <- CVlm(stepdummyk8,dummydataarm)
CVk9 <- CVlm(stepdummyk9,dummydataarm)

Model <- c('interarmAIC','interarmk8','interarmdummyAIC','interarmdummyBIC','interarmdummyk7','interarmdummyk8','interarmdummyk9')
Krydsvalideringsscore <- c(mean(CVlminterstepAIC),mean(CVinterk8),mean(CVAIC),mean(CVBIC),mean(CVk7),mean(CVk8),mean(CVk9))
sd <- (1/sqrt(10))*c(sd(CVlminterstepAIC),sd(CVinterk8),sd(CVAIC),sd(CVBIC),sd(CVk7),sd(CVk8),sd(CVk9))
Parametre <- c(28,14,28,18,15,12,11)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')


coef <- summary(stepdummyk8)$coefficients
konf <- confint(stepdummyk8)
estimater <- cbind(coef[,1],konf)
print(xtable(estimater),format.args=list(decimal.mark=','))
