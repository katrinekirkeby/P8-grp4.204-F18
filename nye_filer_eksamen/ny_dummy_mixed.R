library(lme4)
library(pbkrtest)
library(hydroGOF)
library(ggplot2)
library(xtable)

MMfuldsys <- lm(pigmentering~EUR+AFR+køn+rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+sted,data=hud)

x <- model.matrix(MMfuldsys)
MMdummydataarm <- data.frame(cbind(x[,-1],hud[,c('pigmentering','Patient.ID')]))
names(MMdummydataarm)

##### Fuld
mixedmodelfull <- lmer(pigmentering~EUR+AFR+kønM+rs101C+rs101T+rs107C+rs107T+rs108C+rs108T+rs112G+rs112A+rs122C+rs122T+rs123G+rs123A+rs126A+rs126T+rs128C+rs128A+rs129G+rs129A+rs132G+rs132C+rs139C+rs139T+rs140G+rs140A+rs142G+rs142A+rs168G+rs168C+rs180C+rs180T+rs203C+rs203T+rs242C+rs242T+rs267G+rs267A+rs442C+rs442T+rs491G+rs491T+rs611G+rs611C+rs674G+rs674T+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)

#Stepvis baglæns baseret på AIC
drop <- drop1(mixedmodelfull,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelfull,~.-rs107C)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs101C)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs674G)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs491G)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs107T)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs129G)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs242T)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs101T)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs203T)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs122T)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs112G)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs128C)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs128A)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs203C)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs126A)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs674T)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs491T)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs442C)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs126T)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs140A)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs267G)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs139T)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs132C)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-kønM)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs180T)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]

summary(mixedmodelAIC)




#Stepvis baglæns baseret på BIC

drop <- drop1(mixedmodelfull,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelfull,~.-rs107C)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs101C)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs674G)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs491G)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs107T)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs129G)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs242T)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs101T)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs203T)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs122T)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs112G)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs128C)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs128A)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs203C)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs126A)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs674T)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs491T)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs442C)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs126T)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs140A)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs267G)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs139T)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs132C)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-kønM)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs180T)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs267A)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs123A)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs132G)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs112A)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs108C)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs168G)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs180C)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-EUR)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs122C)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs108T)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]

summary(mixedmodelBIC)


###Gammel
hudgammelarm <- hudcompletegammel[,-c(28,30)]
hudgammelarm$sted <- 'arm'
names(hudgammelarm)[28] <- 'pigmentering'

hudgammelballe <- hudcompletegammel[,-c(29,30)]
hudgammelballe$sted <- 'balle'
names(hudgammelballe)[28] <- 'pigmentering'

hudgammelpande <- hudcompletegammel[,-c(28,29)]
hudgammelpande$sted <- 'pande'
names(hudgammelpande)[28] <- 'pigmentering'

hudgammel <- rbind(hudgammelarm,hudgammelballe,hudgammelpande)
hudgammel$sted <- factor(hudgammel$sted)

MMfuldsys <- lm(pigmentering~EUR+AFR+køn+rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+sted,data=hudgammel)

x <- model.matrix(MMfuldsys)
MMdummydataarmgammel <- data.frame(cbind(x[,-1],hudgammel[,c('pigmentering','Patient.ID')]))
names(MMdummydataarmgammel)

mixedmodelfulldummy <- lmer(pigmentering~EUR+AFR+kønM+rs101CT+rs101T+rs107CT+rs107T+rs108CT+rs108T+rs112G+rs112GA+rs122CT+rs122T+rs123G+rs123GA+rs126AT+rs126T+rs128C+rs128CA+rs129G+rs129GA+rs132G+rs132GC+rs139CT+rs139T+rs140G+rs140GA+rs142G+rs142GA+rs168G+rs168GC+rs180CT+rs180T+rs203CT+rs203T+rs242CT+rs242T+rs267G+rs267GA+rs442CT+rs442T+rs491GT+rs491T+rs611G+rs611GC+rs674GT+rs674T+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarmgammel)
mixedmodelp01dummy <- lmer(pigmentering~AFR+rs123G+rs129G+rs129GA+rs139CT+rs140G+rs142G+rs142GA+rs168GC+rs442T+rs611G+rs611GC+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarmgammel)
mixedmodelAICdummy <- lmer(pigmentering~EUR+AFR+rs108CT+rs108T+rs112G+rs112GA+rs122CT+rs123G+rs123GA+rs129G+rs129GA+rs132G+rs139CT+rs140G+rs142G+rs142GA+rs168G+rs168GC+rs180CT+rs242CT+rs242T+rs267G+rs267GA+rs442T+rs611G+rs611GC+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarmgammel)
mixedmodelBICdummy <- lmer(pigmentering~AFR+rs123G+rs129G+rs129GA+rs139CT+rs140G+rs142G+rs142GA+rs168GC+rs242CT+rs442T+rs611G+rs611GC+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarmgammel)
mixedmodelk8dummy <- lmer(pigmentering~AFR+rs123G+rs129GA+rs139CT+rs142G+rs142GA+rs442T+rs611G+rs611GC+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarmgammel)

#GAmmel slut

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

CVfulldummygammel <- CVmixed(mixedmodelfulldummy,MMdummydataarmgammel)
CVp01dummygammel <- CVmixed(mixedmodelp01dummy,MMdummydataarmgammel)
CVAICdummygammel <- CVmixed(mixedmodelAICdummy,MMdummydataarmgammel)
CVBICdummygammel <- CVmixed(mixedmodelBICdummy,MMdummydataarmgammel)
CVk8dummygammel <- CVmixed(mixedmodelk8dummy,MMdummydataarmgammel)

##Uden dummy
mixedmodelAICgammel <- lmer(pigmentering~EUR + AFR + rs108 + rs122 + rs123 + rs129 + rs139 + rs140 + rs142 + rs168 + rs180 + rs242 + rs442 + rs611 + sted + (1 | Patient.ID),data=hudgammel)
mixedmodelBICgammel <- lmer(pigmentering~EUR + AFR + rs129 + rs142 + rs442 + rs611 + sted + (1 | Patient.ID),data=hudgammel)
CVAICgammel <- CVmixed(mixedmodelAICgammel,hudgammel)
CVBICgammel <- CVmixed(mixedmodelBICgammel,hudgammel)

##Nye
CVAICNy <- CVmixed(mixedmodelAIC,MMdummydataarm)
CVBICNy <- CVmixed(mixedmodelBIC,MMdummydataarm)

Model <- c('MMdfuld', 'MMdp01','MMdAIC','MMdBIC','MMdk8','MMAIC','MMBIC')
modelny <- c('MMdAICny','MMdBICny')
Mean <- c(mean(CVfulldummygammel),mean(CVp01dummygammel),mean(CVAICdummygammel),mean(CVBICdummygammel),mean(CVk8dummygammel),mean(CVAICgammel),mean(CVBICgammel))
krydsny <- c(mean(CVAICNy),mean(CVBICNy))
sd <- (1/(sqrt(10)))*c(sd(CVfulldummygammel),sd(CVp01dummygammel),sd(CVAICdummygammel),sd(CVBICdummygammel),sd(CVk8dummygammel),sd(CVAICgammel),sd(CVBICgammel))
sdny <- (1/sqrt(10))*c(sd(CVAICNy),sd(CVBICNy))
Parametre <- c(50,15,29,16,12,29,13)
parny <- c(26,16)
ggplot()+geom_point(aes(Model,Mean, size=Parametre))+
  geom_errorbar(aes(x=Model, ymin=Mean-sd, ymax=Mean+sd),width=0.25) + ylab('Krydsvalideringsscore')+
  geom_point(aes(modelny,krydsny,size=parny), col='blue')+
  geom_errorbar(aes(x=modelny, ymin=krydsny-sdny, ymax=krydsny+sdny),width=0.25, col='blue')+
  geom_hline(yintercept=c(Mean,krydsny)[which.min(c(Mean,krydsny))]+c(sd,sdny)[which.min(c(Mean,krydsny))], linetype='dashed')

###############################################
#################Interaktion###################
###############################################

MMinterAIC <- lmer(pigmentering ~ EUR + AFR + rs108C + rs108T + rs112A + rs122C +  
                  rs123G + rs123A + rs129A + rs132G + rs139C + rs140G + rs142G*AFR +  
                  rs142A*AFR + rs168G + rs168C + rs180C + rs242C + rs267A + rs442T +  
                  rs611G + rs611C + stedballe + stedpande + (1 | Patient.ID), data = MMdummydataarm)
KRmodcomp(MMinterAIC,mixedmodelAIC)
CVinterAIC <- CVmixed(MMinterAIC,MMdummydataarm)

MMinterBIC <- lmer(pigmentering ~ AFR + rs123G + rs129A + rs139C + rs140G + rs142G*AFR +  
                     rs142A*AFR + rs168C + rs242C + rs442T + rs611G + rs611C + stedballe +  
                     stedpande + (1 | Patient.ID),data=MMdummydataarm)
KRmodcomp(MMinterBIC,mixedmodelBIC)
CVinterBIC <- CVmixed(MMinterBIC,MMdummydataarm)

Model <- c('MMdAICny','MMdBICny','MMdinterAIC','MMdinterBIC')
Mean <- c(mean(CVAICNy),mean(CVBICNy),mean(CVinterAIC),mean(CVinterBIC))
sd <- (1/sqrt(10))*c(sd(CVAICNy),sd(CVBICNy),sd(CVinterAIC),sd(CVinterBIC))
Parametre <- c(26,16,28,18)
ggplot()+geom_point(aes(Model,Mean, size=Parametre))+geom_errorbar(aes(x=Model, ymin=Mean-sd, ymax=Mean+sd),width=0.25) + ylab('Krydsvalideringsscore')+geom_hline(yintercept=Mean[which.min(Mean)]+sd[which.min(Mean)], linetype='dashed')
