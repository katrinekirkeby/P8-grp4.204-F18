library(lme4)
library(pbkrtest)
library(hydroGOF)
library(ggplot2)
library(xtable)
####----sys
lmsysdummy<-lm(Pigmentering~EUR+AFR+køn+rs101+rs107+rs108+rs112+rs122+rs123+rs126
          +rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442
          +rs491+rs611+rs674+Sted,data=New.Data)
x <- model.matrix(lmsysdummy)
Dummy <- data.frame(cbind(x[,-1],New.Data[,c('Pigmentering','Sted','Patient.ID')]))
names(Dummy)[49] <- 'Stedpande'
names(Dummy)[48] <- 'Stedballe'
names(Dummy)[3] <- 'køn'
####----koef
lmsysdummy$coefficients

####----mmfuld
mmfulddummy <-lmer(Pigmentering~EUR + AFR + køn + rs101CT+ rs101T+ rs107CT + rs107T + rs108CT + rs108T + rs112G + rs112GA + rs122CT + rs122T + rs123G + rs123GA + rs126AT + rs126T + rs128C + rs128CA + rs129G + rs129GA + rs132G + rs132GC + rs139CT + rs139T + rs140G + rs140GA + rs142G + rs142GA + rs168G + rs168GC + rs180CT + rs180T + rs203CT + rs203T + rs242CT + rs242T + rs267G + rs267GA + rs442CT + rs442T + rs491GT + rs491T + rs611G + rs611GC + rs674GT + rs674T + Stedballe + Stedpande +(1|Sted:Patient.ID)+(1|Patient.ID),data=Dummy)
summary(mmfulddummy)

####----
mmupdatedummy <- mmfulddummy
mmupdatedummy <- update(mmupdatedummy,~.-rs674GT)
mmupdatedummy <- update(mmupdatedummy,~.-rs107T)
mmupdatedummy <- update(mmupdatedummy,~.-rs139T)
mmupdatedummy <- update(mmupdatedummy,~.-rs674T)
mmupdatedummy <- update(mmupdatedummy,~.-rs180T)
mmupdatedummy <- update(mmupdatedummy,~.-rs128C)
mmupdatedummy <- update(mmupdatedummy,~.-rs101CT)
mmupdatedummy <- update(mmupdatedummy,~.-rs107CT)
mmupdatedummy <- update(mmupdatedummy,~.-rs203CT)
mmupdatedummy <- update(mmupdatedummy,~.-rs203T)
mmupdatedummy <- update(mmupdatedummy,~.-rs491GT)
mmupdatedummy <- update(mmupdatedummy,~.-rs101T)
mmupdatedummy <- update(mmupdatedummy,~.-rs128CA)
mmupdatedummy <- update(mmupdatedummy,~.-rs126T)
mmupdatedummy <- update(mmupdatedummy,~.-rs442CT)
mmupdatedummy <- update(mmupdatedummy,~.-rs491T)
mmupdatedummy <- update(mmupdatedummy,~.-rs126AT)
mmupdatedummy <- update(mmupdatedummy,~.-rs122T)
mmupdatedummy <- update(mmupdatedummy,~.-rs140GA)
mmupdatedummy <- update(mmupdatedummy,~.-rs132GC)
mmupdatedummy <- update(mmupdatedummy,~.-køn)
mmupdatedummy <- update(mmupdatedummy,~.-rs267GA)
mmupdatedummy <- update(mmupdatedummy,~.-rs267G)
mmupdatedummy <- update(mmupdatedummy,~.-rs112G)
mmupdatedummy <- update(mmupdatedummy,~.-rs112GA)
mmupdatedummy <- update(mmupdatedummy,~.-rs132G)
mmupdatedummy <- update(mmupdatedummy,~.-rs123GA)
mmupdatedummy <- update(mmupdatedummy,~.-rs108CT)
mmupdatedummy <- update(mmupdatedummy,~.-rs168G)
mmupdatedummy <- update(mmupdatedummy,~.-rs242T)
mmupdatedummy <- update(mmupdatedummy,~.-rs242CT)
mmupdatedummy <- update(mmupdatedummy,~.-rs180CT)
mmupdatedummy <- update(mmupdatedummy,~.-EUR)
mmupdatedummy <- update(mmupdatedummy,~.-rs168GC)


#Tester systematiske effekter
fit2 <- update(mmupdatedummy,~.-AFR)
kr2<- KRmodcomp(mmupdatedummy,fit2)

fit9 <- update(mmupdatedummy,~.-rs108T)
kr9<- KRmodcomp(mmupdatedummy,fit9)

fit12 <- update(mmupdatedummy,~.-rs122CT)
kr12<- KRmodcomp(mmupdatedummy,fit12)

fit14 <- update(mmupdatedummy,~.-rs123G)
kr14<- KRmodcomp(mmupdatedummy,fit14)

fit20 <- update(mmupdatedummy,~.-rs129G)
kr20<- KRmodcomp(mmupdatedummy,fit20)

fit21 <- update(mmupdatedummy,~.-rs129GA)
kr21<- KRmodcomp(mmupdatedummy,fit21)

fit24 <- update(mmupdatedummy,~.-rs139CT)
kr24<- KRmodcomp(mmupdatedummy,fit24)

fit26 <- update(mmupdatedummy,~.-rs140G)
kr26<- KRmodcomp(mmupdatedummy,fit26)

fit28 <- update(mmupdatedummy,~.-rs142G)
kr28<- KRmodcomp(mmupdatedummy,fit28)

fit29 <- update(mmupdatedummy,~.-rs142GA)
kr29<- KRmodcomp(mmupdatedummy,fit29)

fit41 <- update(mmupdatedummy,~.-rs442T)
kr41<- KRmodcomp(mmupdatedummy,fit41)

fit44 <- update(mmupdatedummy,~.-rs611G)
kr44<- KRmodcomp(mmupdatedummy,fit44)

fit45 <- update(mmupdatedummy,~.-rs611GC)
kr45<- KRmodcomp(mmupdatedummy,fit45)

fit48 <- update(mmupdatedummy,~.-Stedballe)
kr48<- KRmodcomp(mmupdatedummy,fit48)

fit49 <- update(mmupdatedummy,~.-Stedpande)
kr49<- KRmodcomp(mmupdatedummy,fit49)

P <- rep(NA,49)

P[2]<- kr2$stats$p.value
P[9]<- kr9$stats$p.value
P[12]<- kr12$stats$p.value
P[14]<- kr14$stats$p.value
P[20]<- kr20$stats$p.value
P[21]<- kr21$stats$p.value
P[24]<- kr24$stats$p.value
P[26]<- kr26$stats$p.value
P[28]<- kr28$stats$p.value
P[29]<- kr29$stats$p.value
P[41]<- kr41$stats$p.value
P[44]<- kr44$stats$p.value
P[45]<- kr45$stats$p.value
P[48]<- kr48$stats$p.value
P[49]<- kr49$stats$p.value

which.max(P) 



KRmodcomp(mmfulddummy,mmupdatedummy)

## Hermed er alle p-værdier under 0,01

#Stepvis baglæns baseret på AIC
drop <- drop1(mmfulddummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmfulddummy,~.-rs674GT)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs107T)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs139T)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs674T)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs180T)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs128C)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs101CT)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs107CT)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs203CT)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs203T)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs491GT)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs101T)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs128CA)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs126T)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs442CT)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs491T)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs126AT)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs122T)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs140GA)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs132GC)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-køn)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs267GA)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]
mmAICdummy <- update(mmAICdummy,~.-rs267G)

drop <- drop1(mmAICdummy,k=2)
drop[which.min(drop$AIC),]


summary(mmAICdummy)


#Stepvis baglæns baseret på BIC
drop <- drop1(mmfulddummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmfulddummy,~.-rs674GT)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs107T)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs139T)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs674T)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs180T)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs128C)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs101CT)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs107CT)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs203CT)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs203T)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs491GT)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs101T)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs128CA)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs126T)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs442CT)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs491T)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs126AT)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs122T)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs140GA)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs132GC)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-køn)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs267GA)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs267G)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs112G)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs112GA)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs132G)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs123GA)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs108CT)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs168G)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs242T)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs242CT)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs180CT)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-EUR)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs168GC)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs122CT)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs108T)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs140G)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]
mmBICdummy <- update(mmBICdummy,~.-rs129G)

drop <- drop1(mmBICdummy,k=log(nrow(Dummy)))
drop[which.min(drop$AIC),]

summary(mmBICdummy)

##k=12
drop <- drop1(mmfulddummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmfulddummy,~.-rs674GT)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs107T)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs139T)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs674T)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs180T)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs128C)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs101CT)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs107CT)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs203CT)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs203T)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs491GT)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs101T)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs128CA)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs126T)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs442CT)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs491T)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs126AT)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs122T)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs140GA)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs132GC)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-køn)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs267GA)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs267G)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs112G)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs112GA)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs132G)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs123GA)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs108CT)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs168G)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs242T)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs242CT)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs180CT)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-EUR)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs168GC)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs122CT)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs108T)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs140G)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs129G)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs129GA)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]
mmk12dummy <- update(mmk12dummy,~.-rs611GC)

drop <- drop1(mmk12dummy,k=12)
drop[which.min(drop$AIC),]

summary(mmk12dummy)


nrow(summary(mmfuld)$coefficients)
nrow(summary(mmupdatedummy)$coefficients)
nrow(summary(mmAICdummy)$coefficients)
nrow(summary(mmBICdummy)$coefficients)
nrow(summary(mmk12dummy)$coefficients)

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

CVfulldummy <- CVmixed(mmfulddummy,Dummy)
CVupdatedummy <- CVmixed(mmupdatedummy,Dummy)
CVAICdummy <- CVmixed(mmAICdummy,Dummy)
CVBICdummy <- CVmixed(mmBICdummy,Dummy)
CVk12dummy <- CVmixed(mmk12dummy,Dummy)



Model <- c( 'MMdummyp01','MMdummyAIC','MMdummyBIC','MMdummyk12','MMfuld', 'MMBIC')
Krydsvalideringsscore <- c(mean(CVupdatedummy),mean(CVAICdummy),mean(CVBICdummy),mean(CVk12dummy),mean(CVfull), mean(CVBIC))
sd <- c(sd(CVupdatedummy)*(1/sqrt(10)),sd(CVAICdummy)*(1/sqrt(10)),sd(CVBICdummy)*(1/sqrt(10)),sd(CVk12dummy)*(1/sqrt(10)),sd(CVfull)*(1/sqrt(10)),sd(CVBIC)*(1/sqrt(10)))
Parametre <- c(16,27,12,10,50,12)
ggplot()+geom_point(aes(Model,Krydsvalideringsscore, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=Krydsvalideringsscore-sd, ymax=Krydsvalideringsscore+sd),width=0.25) + geom_hline(yintercept=Krydsvalideringsscore[which.min(Krydsvalideringsscore)]+sd[which.min(Krydsvalideringsscore)], linetype='dashed')



############## Residual plot ##################

par(mfrow=c(2,2))
res <- residuals(mmupdatedummy)
plot(res,main = 'Plot af residualer',ylab='Residualer', xlab='Indeks')
hist(res,xlab = 'Residualer',ylab='Frekvens',main = 'Histogram af residualer')
qqnorm(res, ylab = 'Empiriske fraktiler', xlab = 'Teoretiske fraktiler')
qqline(res)
plot(fitted(mmAICdummy),res, ylab = 'Residualer',xlab = 'Fittede værdier',main = 'Residualer mod fittede værdier')

par(mfrow=c(2,1))
raneffects=ranef(mmupdatedummy)
#qqplot of random intercepts
ran <-raneffects$Patient.ID
qqnorm(ran$`(Intercept)`, ylab = 'Empiriske fraktiler', xlab = 'Teoretiske fraktiler')
qqline(ran$`(Intercept)`)
hist(ran$`(Intercept)`,main = 'Histogram af tilfældige effekter',xlab = 'Tilfældige effekter',ylab = 'Frekvens')

par(mfrow=c(2,1))
raneffects=ranef(mmupdatedummy)
#qqplot of random intercepts
ran <-raneffects$`Sted:Patient.ID`
qqnorm(ran$`(Intercept)`, ylab = 'Empiriske fraktiler', xlab = 'Teoretiske fraktiler')
qqline(ran$`(Intercept)`)
hist(ran$`(Intercept)`,main = 'Histogram af tilfældige effekter',xlab = 'Tilfældige effekter',ylab = 'Frekvens')


################### Estimat og konfidensintervals tabel #####################

coef <- summary(mmupdatedummy)$coefficients
konf <- confint(mmupdatedummy,method = 'boot')
konf <- konf[-(1:3),]
estimater <- cbind(coef,konf)
estimater <- estimater[,-(2:3)]
print(xtable(estimater),format.args=list(decimal.mark=','))
