library(lme4)
library(pbkrtest)
library(hydroGOF)
library(ggplot2)
library(xtable)

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

lmmodelsys <- lm(pigmentering~EUR+AFR+køn+rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+sted,data=hud)
res <- residuals(lmmodelsys)
ggplot()+geom_point(aes(x=hud$Patient.ID[c(1:20,446:465,891:910)],y=res[c(1:20,446:465,891:910)]))
ggplot()+geom_point(aes(x=hud$Patient.ID[c(1:20,446:465,891:910)],y=rnorm(60)))


mixedmodelfull <- lmer(pigmentering~EUR+AFR+køn+rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+sted+(1|Patient.ID),data=hud)
summary(mixedmodelfull)

lmmodel <- lm(pigmentering~EUR+AFR+køn+rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+sted+Patient.ID,data=hud)
lmvarians <- lm(pigmentering~Patient.ID,data=hud)
anova(lmmodel)
anova(lmvarians)

anova(mixedmodelfull,lmmodelsys)

mixedmodelfull <- lmer(pigmentering~EUR+AFR+køn+rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+(1|Patient.ID),data=hud,REML=F)
Qobs <- -2*(logLik(lmmodelsys)-logLik(mixedmodelfull))
Q=rep(0,1000)
for (i in 1:1000){
  y=simulate(lmmodelsys)[[1]]
  fitH0star=lm(y~EUR + AFR + køn + rs101 + rs107 + rs108 + rs112 + 
                 rs122 + rs123 + rs126 + rs128 + rs129 + rs132 + rs139 + rs140 + 
                 rs142 + rs168 + rs180 + rs203 + rs242 + rs267 + rs442 + rs491 + 
                 rs611 + rs674 + sted, data = hud)
  fitstar=lmer(y~EUR + AFR + køn + rs101 + rs107 + rs108 + rs112 + 
                 rs122 + rs123 + rs126 + rs128 + rs129 + rs132 + rs139 + rs140 + 
                 rs142 + rs168 + rs180 + rs203 + rs242 + rs267 + rs442 + rs491 + 
                 rs611 + rs674 + sted + (1 | Patient.ID), data = hud, REML=F)
  Q[i]=-2*(logLik(fitH0star)-logLik(fitstar))
}

mean(Q>Qobs)
hist(Q)
Qobs
qqplot(Q,rchisq(1000,1))
plot.qqline(Q,rchisq(1000,1))
abline(c(0,1))

#Variansen er ikke nul. ALtså skal man bruge en mixed model

#Tester systematiske effekter

#Laver den valgte mixed model valgt med stepvis udvælgelse via KRmodcomp 
mixedmodelupdate <- lmer(pigmentering~AFR+rs108+rs129+rs142+rs168+rs442+rs611+sted+(1|Patient.ID),data=hud) 
summary(mixedmodelupdate)



#Stepvis baglæns baseret på AIC
drop <- drop1(mixedmodelfull,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelfull,~.-rs101)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs107)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs128)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs203)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs674)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs491)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs126)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs267)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-køn)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs132)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]
mixedmodelAIC <- update(mixedmodelAIC,~.-rs112)

drop <- drop1(mixedmodelAIC,k=2)
drop[which.min(drop$AIC),]

summary(mixedmodelAIC)


#Stepvis baglæns baseret på BIC
drop <- drop1(mixedmodelfull,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelfull,~.-rs101)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs107)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs128)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs203)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs674)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs491)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs126)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs267)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs132)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs112)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs242)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs180)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs122)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs140)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs168)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs108)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-køn)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs123)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]
mixedmodelBIC <- update(mixedmodelBIC,~.-rs139)

drop <- drop1(mixedmodelBIC,k=log(nrow(hud)))
drop[which.min(drop$AIC),]

summary(mixedmodelBIC)$coefficients
confint(mixedmodelBIC)


#Stepvis baglæns baseret på k=3
drop <- drop1(mixedmodelfull,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelfull,~.-rs101)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-rs107)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-rs128)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-rs203)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-rs674)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-rs491)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-rs126)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-rs267)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-rs132)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-rs112)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-køn)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-rs122)

drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]
mixedmodelk3 <- update(mixedmodelk3,~.-rs180)


drop <- drop1(mixedmodelk3,k=3)
drop[which.min(drop$AIC),]


#Stepvis baglæns baseret på k=4
drop <- drop1(mixedmodelfull,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelfull,~.-rs101)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs107)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs128)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs203)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs674)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs491)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs126)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs267)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs132)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs112)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-køn)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs122)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs180)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs242)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs139)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]
mixedmodelk4 <- update(mixedmodelk4,~.-rs140)

drop <- drop1(mixedmodelk4,k=4)
drop[which.min(drop$AIC),]




#Stepvis baglæns baseret på k=5
drop <- drop1(mixedmodelfull,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelfull,~.-rs101)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs107)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs128)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs203)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs674)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs491)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs126)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs267)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs132)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs112)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs242)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs180)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs122)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-køn)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs139)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs140)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs123)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]
mixedmodelk5 <- update(mixedmodelk5,~.-rs168)

drop <- drop1(mixedmodelk5,k=5)
drop[which.min(drop$AIC),]




##K=12
drop <- drop1(mixedmodelfull,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelfull,~.-rs101)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs107)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs128)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs203)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs674)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs491)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs126)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs267)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs132)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs112)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs242)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs180)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs122)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs140)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs168)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs108)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs123)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs139)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-køn)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-rs129)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]
mixedmodelk12 <- update(mixedmodelk12,~.-EUR)

drop <- drop1(mixedmodelk12,k=12)
drop[which.min(drop$AIC),]

summary(mixedmodelk12)

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

CVfull <- CVmixed(mixedmodelfull,hud)
CVupdate <- CVmixed(mixedmodelupdate,hud)
CVAIC <- CVmixed(mixedmodelAIC,hud)
CVBIC <- CVmixed(mixedmodelBIC,hud)
CVk3 <- CVmixed(mixedmodelk3,hud)
CVk4 <- CVmixed(mixedmodelk4,hud)
CVk5 <- CVmixed(mixedmodelk5,hud)
CVk12 <- CVmixed(mixedmodelk12,hud)



Model <- c('MMfuld', 'MMupdate','MMAIC','MMBIC','MMk3','MMk4')
Mean <- c(mean(CVfull),mean(CVupdate),mean(CVAIC),mean(CVBIC),mean(CVk3),mean(CVk4))
sd <- (1/(sqrt(10)))*c(sd(CVfull),sd(CVupdate),sd(CVAIC),sd(CVBIC),sd(CVk3),sd(CVk4))
Parametre <- c(48,16,27,11,25,19)
ggplot()+geom_point(aes(Model,Mean, size=Parametre))+geom_errorbar(aes(x=Model, ymin=Mean-sd, ymax=Mean+sd),width=0.25) + ylab('Krydsvalideringsscore')+geom_hline(yintercept=Mean[which.min(Mean)]+sd[which.min(Mean)], linetype='dashed')


##Modeltjek
par(mfrow=c(2,2),mar=c(4,4,2,2))
res <- residuals(mixedmodelBIC)
plot(res,main = 'Plot af residualer',ylab='Residualer', xlab='Indeks')
hist(res,xlab = 'Residualer',ylab='Frekvens',main = 'Histogram af residualer')
qqnorm(res, xlab = 'Teoretiske fraktiler',ylab = 'Empiriske fraktiler')
qqline(res)
plot(fitted(mixedmodelBIC),res, ylab = 'Residualer',xlab = 'Fittede værdier',main = 'Residualer mod fittede værdier')

par(mfrow=c(2,1))
raneffects=ranef(mixedmodelBIC)
#qqplot of random intercepts
ran <- raneffects[[1]]
qqnorm(ran$`(Intercept)`, xlab = 'Teoretiske fraktiler',ylab = 'Empiriske fraktiler')
qqline(ran$`(Intercept)`)
hist(ran$`(Intercept)`,main = 'Histogram af tilfældige effekter',xlab = 'Tilfældige effekter',ylab = 'Frekvens')

par(mfrow=c(1,1))
mixedmodelBIC
lmBICsys <- lm(pigmentering~EUR+AFR+rs129+rs142+rs442+rs611+sted,data = hud)
ressys <- residuals(lmBICsys)
plot(ressys~hud$sted,xlab='Sted',ylab='Samlet residual')
ressusarm <- ressys[hud$sted=='arm']
ressusballe <- ressys[hud$sted=='balle']
ressuspande <- ressys[hud$sted=='pande']

par(mfrow=c(1,3))
plot(ressusarm,ressusballe, xlim = c(-6,8),ylim = c(-6,8),xlab='Samlet residual arm',ylab = 'Samlet residual balle')
abline(lm(ressusarm~ressusballe),col='red')
plot(ressusarm,ressuspande, xlim = c(-6,8),ylim = c(-6,8),xlab='Samlet residual arm',ylab = 'Samlet residual pande')
abline(lm(ressusarm~ressuspande),col='red')
plot(ressusballe,ressuspande, xlim = c(-6,8),ylim = c(-6,8),xlab='Samlet residual balle',ylab = 'Samlet residual pande')
abline(lm(ressusballe~ressuspande),col='red')

print(xtable(cov(cbind(ressusarm,ressusballe,ressuspande))), format.args=list(decimal.mark=','))

coef <- summary(mixedmodelk3)$coefficients
konf <- confint(mixedmodelk3,method='boot')
konf <- konf[-(1:2),]
estimater <- cbind(coef,konf)
estimater <- estimater[,-(2:3)]
print(xtable(estimater),format.args=list(decimal.mark=','))

coef <- summary(mixedmodelBIC)$coefficients
konf <- confint(mixedmodelBIC, method='boot')
konf <- konf[-(1:2),]
estimater <- cbind(coef,konf)
estimater <- estimater[,-(2:3)]
print(xtable(estimater),format.args=list(decimal.mark=','))
