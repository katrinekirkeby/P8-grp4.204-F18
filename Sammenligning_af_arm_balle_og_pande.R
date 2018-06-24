library(hydroGOF)


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

## Arm
set.seed(50)
lmarmAIC <- lm(arm ~ rs108 + rs112 + rs123 + rs129 + rs132 + rs139 + rs140+ 
                 rs142 + rs168 + rs242 + rs442 + rs611 + AFR, data = hudcomplete)
CVarmlmstepAIC <- CVlm(lmarmAIC,hudcomplete)

set.seed(50)
lmarmk09 <- lm(arm ~ rs139 + rs142 + rs442 + rs611 + AFR, data=hudcomplete)
CVarmlmstepk09 <- CVlm(lmarmk09,hudcomplete)

set.seed(50)
lmarmnul <- lm(arm~1,data = hudcomplete)
CVarmnul <- CVlm(lmarmnul,hudcomplete)

## Pande
set.seed(50)
lmpandeAIC <- lm(pande ~ rs108 + rs123 + rs129 + rs132 + rs139 + 
                   rs140 + rs142 + rs242 + rs442 + rs611 + køn + EUR + AFR, data=hudcomplete)
CVpandelmstepAIC <- CVlm(lmpandeAIC,hudcomplete)

set.seed(50)
lmpandeBIC <- lm(pande ~ rs108 + rs129 + rs142 + rs442 + rs611 + AFR, data=hudcomplete)
CVpandelmstep <- CVlm(lmpandeBIC,hudcomplete)

set.seed(50)
lmpandenul <- lm(pande~1,data = hudcomplete)
CVpandenul <- CVlm(lmpandenul,hudcomplete)
## Balle

set.seed(50)
lmballeAIC <- lm(balle ~ rs108 + rs122 + rs123 + 
                   rs129 + rs139 + rs140 + rs142 + rs168 + rs180 + 
                   rs242 + rs267 + rs442 + rs611 + køn +
                   EUR + AFR, data=hudcomplete)
CVballelmstepAIC <- CVlm(lmballeAIC,hudcomplete)

set.seed(50)
lmballeBIC <- lm(balle ~ rs122 + rs123 + 
                   rs129 + rs142 + rs442 + rs611 + EUR + AFR, data=hudcomplete)
CVballelmstep <- CVlm(lmballeBIC,hudcomplete)

set.seed(50)
lmballenul <- lm(balle~1,data = hudcomplete)
CVballenul <- CVlm(lmballenul,data = hudcomplete)



Model <- c('lmarmAIC','lmarmk09','lmballeAIC','lmballeBIC','lmpandeAIC','lmpandeBIC')
Krydsvalideringsscore <- c(mean(CVarmlmstepAIC),mean(CVarmlmstepk09),mean(CVballelmstepAIC),mean(CVballelmstep),mean(CVpandelmstepAIC),mean(CVpandelmstep))
sd <- (1/sqrt(10))*c(sd(CVarmlmstepAIC),sd(CVarmlmstepk09),sd(CVballelmstepAIC),sd(CVballelmstep),sd(CVpandelmstepAIC),sd(CVpandelmstep))
Parametre <- c(26,10,30,15,24,12)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')

Model <- c('lmarmAIC','lmarmk09','lmarmnul','lmballeAIC','lmballeBIC','lmballenul','lmpandeAIC','lmpandeBIC','lmpandenul')
Krydsvalideringsscore <- c(mean(CVarmlmstepAIC),mean(CVarmlmstepk09),mean(CVarmnul),mean(CVballelmstepAIC),mean(CVballelmstep),mean(CVballenul),mean(CVpandelmstepAIC),mean(CVpandelmstep),mean(CVpandenul))
sd <- (1/sqrt(10))*c(sd(CVarmlmstepAIC),sd(CVarmlmstepk09),sd(CVarmnul),sd(CVballelmstepAIC),sd(CVballelmstep),sd(CVballenul),sd(CVpandelmstepAIC),sd(CVpandelmstep),sd(CVpandenul))
Parametre <- c(26,10,1,30,15,1,24,12,1)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')

Model <- c('lmarmAIC','lmarmk09','lmballeAIC','lmballeBIC','lmpandeAIC','lmpandeBIC')
Krydsvalideringsscore <- c(mean(CVarmlmstepAIC/CVarmnul),mean(CVarmlmstepk09/CVarmnul),mean(CVballelmstepAIC/CVballenul),mean(CVballelmstep/CVballenul),mean(CVpandelmstepAIC/CVpandenul),mean(CVpandelmstep/CVpandenul))
sd <- (1/sqrt(10))*c(sd(CVarmlmstepAIC/CVarmnul),sd(CVarmlmstepk09/CVarmnul),sd(CVballelmstepAIC/CVballenul),sd(CVballelmstep/CVballenul),sd(CVpandelmstepAIC/CVpandenul),sd(CVpandelmstep/CVpandenul))
Parametre <- c(26,10,30,15,24,12)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')

