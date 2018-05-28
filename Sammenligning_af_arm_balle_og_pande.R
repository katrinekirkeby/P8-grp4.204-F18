
## Arm
CVarmlmstepAIC <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs108 + rs112 + rs123 + rs129 + rs132 + rs139 + rs140+ 
              rs142 + rs168 + rs242 + rs442 + rs611 + AFR, data=train)
  pred <- predict(fit,test)
  CVarmlmstepAIC[i] <- rmse(pred,test$arm)
}

CVarmlmstepk09 <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(arm ~ rs139 + rs142 + rs442 + rs611 + AFR, data=train)
  pred <- predict(fit,test)
  CVarmlmstepk09[i] <- rmse(pred,test$arm)
}


## Pande


CVpandelmstepAIC <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(pande ~ rs108 + rs123 + rs129 + rs132 + rs139 + 
              rs140 + rs142 + rs242 + rs442 + rs611 + køn + EUR + AFR, data=train)
  pred <- predict(fit,test)
  CVpandelmstepAIC[i] <- rmse(pred,test$pande)
}


CVpandelmstep <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(pande ~ rs108 + rs129 + rs142 + rs442 + rs611 + AFR, data=train)
  pred <- predict(fit,test)
  CVpandelmstep[i] <- rmse(pred,test$pande)
}
## Balle

CVballelmstepAIC <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle ~ rs108 + rs122 + rs123 + 
              rs129 + rs139 + rs140 + rs142 + rs168 + rs180 + 
              rs242 + rs267 + rs442 + rs611 + køn +
              EUR + AFR, data=train)
  pred <- predict(fit,test)
  CVballelmstepAIC[i] <- rmse(pred,test$balle)
}



CVballelmstep <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete), replace=T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle ~ rs122 + rs123 + 
              rs129 + rs142 + rs442 + rs611 + EUR + AFR, data=train)
  pred <- predict(fit,test)
  CVballelmstep[i] <- rmse(pred,test$balle)
}




Model <- c('lmarmAIC','lmarmk09','lmballeAIC','lmballeBIC','lmpandeAIC','lmpandeBIC')
Krydsvalideringsscore <- c(mean(CVarmlmstepAIC),mean(CVarmlmstepk09),mean(CVballelmstepAIC),mean(CVballelmstep),mean(CVpandelmstepAIC),mean(CVpandelmstep))
sd <- (1/sqrt(10))*c(sd(CVarmlmstepAIC),sd(CVarmlmstepk09),sd(CVballelmstepAIC),sd(CVballelmstep),sd(CVpandelmstepAIC),sd(CVpandelmstep))
Parametre <- c(26,10,30,15,24,12)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')
