library(lme4)
library(pbkrtest)
library(hydroGOF)
library(ggplot2)
####----sys
lmsys<-lm(Pigmentering~EUR+AFR+køn+rs101+rs107+rs108+rs112+rs122+rs123+rs126
  +rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442
  +rs491+rs611+rs674+Sted,data=New.Data)
####----koef
lmsys$coefficients
####----lmmodel
lm<- lm(Pigmentering~EUR+AFR+køn+rs101+rs107+rs108+rs112+rs122+rs123+rs126
  +rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442
  +rs491+rs611+rs674+Sted*Patient.ID,data=New.Data)
anova(lm)
####----mmfuld
mmfuld <-lmer(Pigmentering~EUR+AFR+køn+rs101+rs107+rs108+rs112+rs122+rs123
  +rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267
  +rs442+rs491+rs611+rs674+Sted+(1|Sted:Patient.ID)+(1|Patient.ID),data=New.Data)
summary(mmfuld)
####----
mmupdate <- mmfuld
mmupdate <- update(mmupdate,~.-rs674)
mmupdate <- update(mmupdate,~.-rs107)
mmupdate <- update(mmupdate,~.-rs128)
mmupdate <- update(mmupdate,~.-rs101)
mmupdate <- update(mmupdate,~.-rs203)
mmupdate <- update(mmupdate,~.-rs491)
mmupdate <- update(mmupdate,~.-rs126)
mmupdate <- update(mmupdate,~.-køn)
mmupdate <- update(mmupdate,~.-rs267)
mmupdate <- update(mmupdate,~.-rs112)
mmupdate <- update(mmupdate,~.-rs132)
mmupdate <- update(mmupdate,~.-rs139)
mmupdate <- update(mmupdate,~.-rs180)
mmupdate <- update(mmupdate,~.-rs242)
mmupdate <- update(mmupdate,~.-rs122)
mmupdate <- update(mmupdate,~.-rs168)
mmupdate <- update(mmupdate,~.-rs140)
mmupdate <- update(mmupdate,~.-EUR)

## Tester systematiske effekter

fit3 <- update(mmupdate,~.-AFR)
kr2 <-KRmodcomp(mmupdate,fit3)

fit7 <- update(mmupdate,~.-rs108)
kr6 <-KRmodcomp(mmupdate,fit7)

fit10 <- update(mmupdate,~.-rs123)
kr9 <-KRmodcomp(mmupdate,fit10)

fit13 <- update(mmupdate,~.-rs129)
kr12 <-KRmodcomp(mmupdate,fit13)

fit17 <- update(mmupdate,~.-rs142)
kr16 <-KRmodcomp(mmupdate,fit17)

fit23 <- update(mmupdate,~.-rs442)
kr22 <-KRmodcomp(mmupdate,fit23)

fit25 <- update(mmupdate,~.-rs611)
kr24 <-KRmodcomp(mmupdate,fit25)

fit27 <- update(mmupdate,~.-Sted)
kr26 <-KRmodcomp(mmupdate,fit27)

P <- rep(NA,26)

P[2]<- kr2$stats$p.value
P[6]<- kr6$stats$p.value
P[9]<- kr9$stats$p.value
P[12]<- kr12$stats$p.value
P[16]<- kr16$stats$p.value
P[22]<- kr22$stats$p.value
P[24]<- kr24$stats$p.value
P[26]<- kr26$stats$p.value

which.max(P)

summary(mmupdate)

mmupdate

## Hermed er alle p-værdier under 0,01

#Stepvis baglæns baseret på AIC
drop <- drop1(mmfuld,k=2)
drop[which.min(drop$AIC),]
mmAIC <- update(mmfuld,~.-rs674)

drop <- drop1(mmAIC,k=2)
drop[which.min(drop$AIC),]
mmAIC <- update(mmAIC,~.-rs107)

drop <- drop1(mmAIC,k=2)
drop[which.min(drop$AIC),]
mmAIC <- update(mmAIC,~.-rs128)

drop <- drop1(mmAIC,k=2)
drop[which.min(drop$AIC),]
mmAIC <- update(mmAIC,~.-rs101)

drop <- drop1(mmAIC,k=2)
drop[which.min(drop$AIC),]
mmAIC <- update(mmAIC,~.-rs203)

drop <- drop1(mmAIC,k=2)
drop[which.min(drop$AIC),]
mmAIC <- update(mmAIC,~.-rs491)

drop <- drop1(mmAIC,k=2)
drop[which.min(drop$AIC),]
mmAIC <- update(mmAIC,~.-rs126)

drop <- drop1(mmAIC,k=2)
drop[which.min(drop$AIC),]
mmAIC <- update(mmAIC,~.-rs267)

drop <- drop1(mmAIC,k=2)
drop[which.min(drop$AIC),]
mmAIC <- update(mmAIC,~.-køn)

drop <- drop1(mmAIC,k=2)
drop[which.min(drop$AIC),]
mmAIC <- update(mmAIC,~.-rs112)

drop <- drop1(mmAIC,k=2)
drop[which.min(drop$AIC),]
mmAIC <- update(mmAIC,~.-rs132)

drop <- drop1(mmAIC,k=2)
drop[which.min(drop$AIC),]

summary(mmAIC)


#Stepvis baglæns baseret på BIC
drop <- drop1(mmfuld,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmfuld,~.-rs674)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs107)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs128)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs101)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs203)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs491)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs126)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs267)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs112)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs132)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs180)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs242)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs139)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs168)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs140)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs122)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-køn)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-rs123)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]
mmBIC <- update(mmBIC,~.-EUR)

drop <- drop1(mmBIC,k=log(nrow(New.Data)))
drop[which.min(drop$AIC),]

summary(mmBIC)


##K=3
drop <- drop1(mmfuld,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmfuld,~.-rs674)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs107)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs128)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs101)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs203)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs491)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs126)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs267)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs112)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-køn)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs132)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs139)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs180)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]
mmk3 <- update(mmk3,~.-rs242)

drop <- drop1(mmk3,k=3)
drop[which.min(drop$AIC),]

summary(mmk3)

nrow(summary(mmBIC)$coefficients)
nrow(summary(mmfuld)$coefficients)
nrow(summary(mmAIC)$coefficients)
nrow(summary(mmupdate)$coefficients)
nrow(summary(mmk3)$coefficients)

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

CVfull <- CVmixed(mmfuld,New.Data)
CVupdate <- CVmixed(mmupdate,New.Data)
CVAIC <- CVmixed(mmAIC,New.Data)
CVBIC <- CVmixed(mmBIC,New.Data)
CVk3 <- CVmixed(mmk3,New.Data)



Model <- c('MMfuld', 'MMp01','MMAIC','MMBIC','MMk3')
Krydsvalideringsscore <- c(mean(CVfull),mean(CVupdate),mean(CVAIC),mean(CVBIC),mean(CVk3))
sd <- c(sd(CVfull)*(1/sqrt(10)),sd(CVupdate)*(1/sqrt(10)),sd(CVAIC)*(1/sqrt(10)),sd(CVBIC)*(1/sqrt(10)),sd(CVk3)*(1/sqrt(10)))
Parametre <- c(50,16,29,14,23)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')


###################### Estimater og konfidensintervaller ############################

coef <- summary(mmBIC)$coefficients
konf <- confint.merMod(mmBIC,method = 'boot')
konf <- konf[-(1:3),]
estimater <- cbind(coef,konf)
estimater <- estimater[,-(2:3)]
print(xtable(estimater),format.args=list(decimal.mark=','))

###################

# Mulig F-test
1-pf((10348.6/376)/(3565.5/846),376,846)

1-pf(27.5/4.2,376,846)

1-pf(9.1/1.5,397,888)

########### residual test ################

par(mfrow=c(2,2))
res <- residuals(mmBIC)
plot(res,main = 'Plot af residualer',ylab='Residualer', xlab='Indeks')
hist(res,xlab = 'Residualer',ylab='Frekvens',main = 'Histogram af residualer')
qqnorm(res, ylab = 'Empiriske fraktiler', xlab = 'Teoretiske fraktiler')
qqline(res)
plot(fitted(mmAIC),res, ylab = 'Residualer',xlab = 'Fittede værdier',main = 'Residualer mod fittede værdier')

par(mfrow=c(2,1))
raneffects=ranef(mmBIC)
#qqplot of random intercepts
ran <-raneffects$Patient.ID
qqnorm(ran$`(Intercept)`, ylab = 'Empiriske fraktiler', xlab = 'Teoretiske fraktiler')
qqline(ran$`(Intercept)`)
hist(ran$`(Intercept)`,main = 'Histogram af tilfældige effekter',xlab = 'Tilfældige effekter',ylab = 'Frekvens')

par(mfrow=c(2,1))
raneffects=ranef(mmBIC)
#qqplot of random intercepts
  ran <-raneffects$`Sted:Patient.ID`
qqnorm(ran$`(Intercept)`, ylab = 'Empiriske fraktiler', xlab = 'Teoretiske fraktiler')
qqline(ran$`(Intercept)`)
hist(ran$`(Intercept)`,main = 'Histogram af tilfældige effekter',xlab = 'Tilfældige effekter',ylab = 'Frekvens')
