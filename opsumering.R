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


MMfuldsys <- lm(pigmentering~EUR+AFR+køn+rs101+rs107+rs108+rs112+rs122+rs123+rs126+rs128+rs129+rs132+rs139+rs140+rs142+rs168+rs180+rs203+rs242+rs267+rs442+rs491+rs611+rs674+sted,data=hud)

x <- model.matrix(MMfuldsys)
MMdummydataarm <- data.frame(cbind(x[,-1],hud[,c('pigmentering','Patient.ID')]))
names(MMdummydataarm)

MMnul <- lmer(pigmentering~1+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)
MM1 <- lmer(pigmentering~AFR+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)
MM2 <- lmer(pigmentering~AFR+rs142G+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)
MM3 <- lmer(pigmentering~AFR+rs142G+rs142GA+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)
MM4 <- lmer(pigmentering~AFR+rs142G+rs142GA+rs442T+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)
MM5 <- lmer(pigmentering~AFR+rs142G+rs142GA+rs442T+rs611G+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)
MM6 <- lmer(pigmentering~AFR+rs142G+rs142GA+rs442T+rs611G+rs139CT+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)
MM7 <- lmer(pigmentering~AFR+rs142G+rs142GA+rs442T+rs611G+rs139CT+rs129GA+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)
MM8 <- lmer(pigmentering~AFR+rs142G+rs142GA+rs442T+rs611G+rs139CT+rs611GC+rs129GA+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)
MMdp01 <- lmer(pigmentering~AFR+rs142G+rs142GA+rs442T+rs611G+rs139CT+rs611GC+rs129GA+rs140G+rs168GC+rs123G+rs129G+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)
MMAIC <-  lmer(pigmentering~EUR+AFR+rs108CT+rs108T+rs112G+rs112GA+rs122CT+rs123G+rs123GA+rs129G+rs129GA+rs132G+rs139CT+rs140G+rs142G+rs142GA+rs168G+rs168GC+rs180CT+rs242CT+rs242T+rs267G+rs267GA+rs442T+rs611G+rs611GC+stedballe+stedpande+(1|Patient.ID),data=MMdummydataarm)

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

CVnul <- CVmixed(MMnul,MMdummydataarm)
CV1 <- CVmixed(MM1,MMdummydataarm)
CV2 <- CVmixed(MM2,MMdummydataarm)
CV3 <- CVmixed(MM3,MMdummydataarm)
CV4 <- CVmixed(MM4,MMdummydataarm)
CV5 <- CVmixed(MM5,MMdummydataarm)
CV6 <- CVmixed(MM6,MMdummydataarm)
CV7 <- CVmixed(MM7,MMdummydataarm)
CV8 <- CVmixed(MM8,MMdummydataarm)
CVdp01 <- CVmixed(MMdp01,MMdummydataarm)
CVAIC <- CVmixed(MMAIC,MMdummydataarm)

Mean <- c(mean(CVnul),mean(CV1),mean(CV2),mean(CV3),mean(CV4),mean(CV5),mean(CV6),mean(CV7),mean(CV8),mean(CVdp01),mean(CVAIC))
sd <- (1/sqrt(10))*c(sd(CVnul),sd(CV1),sd(CV2),sd(CV3),sd(CV4),sd(CV5),sd(CV6),sd(CV7),sd(CV8),sd(CVdp01),sd(CVAIC))
ggplot()+geom_point(aes(c(0:8,12,26),Mean))+geom_errorbar(aes(x=c(0:8,12,26), ymin=Mean-sd, ymax=Mean+sd),width=0.25) + ylab('Krydsvalideringsscore')+xlab('Antal prædiktorer eksklusiv sted')+scale_y_continuous(breaks = seq(1.9,3.3,0.1))+scale_x_continuous(breaks = c(0:8,12,26))+geom_line(aes(c(0:8,12,26),Mean))

AIC <- AIC(MMnul,MM1,MM2,MM3,MM4,MM5,MM6,MM7,MM8,MMdp01,MMAIC)
AIC <- AIC[,2]
BIC <- AIC(MMnul,MM1,MM2,MM3,MM4,MM5,MM6,MM7,MM8,MMdp01,MMAIC, k=log(nrow(MMdummydataarm)))
BIC <- BIC[,2]

ggplot()+geom_point(aes(c(0:8,12,26),AIC,col='AIC'))+geom_line(aes(c(0:8,12,26),AIC,col='AIC'))+geom_point(aes(c(0:8,12,26),BIC,col='BIC'))+geom_line(aes(c(0:8,12,26),BIC,col='BIC'))+xlab('Antal prædiktorer eksklusiv sted')+scale_x_continuous(breaks = c(0:8,12,26))+ylab('Værdi')+guides(colour=guide_legend(title=''))+theme(legend.position = c(0.8, 0.8))
