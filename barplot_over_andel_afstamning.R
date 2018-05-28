library(ggplot2)
datAFR <-hudcomplete[,c("Patient.ID",'AFR')] 
names(datAFR)[2] <- 'afstamning'
datAFR$nat <- 'AFR'
datEUR <-hudcomplete[,c("Patient.ID",'EUR')] 
names(datEUR)[2] <- 'afstamning'
datEUR$nat <- 'EUR'
datNAM <-hudcomplete[,c("Patient.ID",'NAM')] 
names(datNAM)[2] <- 'afstamning'
datNAM$nat <- 'NAM'
dat <- rbind(datAFR,datEUR,datNAM)
dat$nat <- factor(dat$nat,levels = c('NAM','AFR','EUR'))
ord <- order(datEUR$afstamning,decreasing = T)
lev <- datEUR$Patient.ID[ord]
dat$Patient.ID <- factor(dat$Patient.ID,levels = lev)
dat$Patient.ID
ggplot()+geom_col(aes(x=dat$Patient.ID,y=dat$afstamning,fill=dat$nat))+xlab('Person')+ylab('Andel afstamning')+theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())+guides(fill=guide_legend(title="Afstamning"))+scale_fill_discrete(labels=c('Indianer','Afrikaner','Europæer'))
