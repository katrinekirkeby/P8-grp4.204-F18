Ind <- hudcomplete
class(Ind$GENDER)
colnames(Ind)[28] <- 'Balle'
colnames(Ind)[29] <- 'Arm'
colnames(Ind)[30] <- 'Pande'
colnames(Ind)[25] <- 'Afrikaner'
colnames(Ind)[26] <- 'Europæer'
colnames(Ind)[27] <- 'Indianer'
Ind$GENDER[hudcomplete$GENDER=="F"] <- 'Kvinde'
Ind$GENDER[Ind$GENDER=="M"] <- 'Mand'

boxplot(Ind[,25:27],ylab='Andel afstamning')
boxplot(Ind[,28:30], ylab='Pigmentering')
par(mfrow=c(1,3))
boxplot(Balle~GENDER, data=Ind, main='Balle')
boxplot(Arm~GENDER, data=Ind, main='Arm')
boxplot(Pande~GENDER, data=Ind, main='Pande')

barplot(table(Ind$GENDER)/sum(table(Ind$GENDER)), legend.text = c('K = 62,1 %','M = 37,9 %'), col = c('mistyrose','lightblue'))
table(Ind$GENDER)/sum(table(Ind$GENDER))

barplot(table(Ind$rs1015362)/sum(table(Ind$rs1015362)))
barplot(table(Ind$rs10777129)/sum(table(Ind$rs10777129)))
barplot(table(Ind$rs10831496)/sum(table(Ind$rs10831496)))
barplot(table(Ind$rs11238349)/sum(table(Ind$rs11238349)))
barplot(table(Ind$rs12203592)/sum(table(Ind$rs12203592)))
barplot(table(Ind$rs12350739)/sum(table(Ind$rs12350739)))
barplot(table(Ind$rs12668421)/sum(table(Ind$rs12668421)))
barplot(table(Ind$rs12896399)/sum(table(Ind$rs12896399)))
barplot(table(Ind$rs12913832)/sum(table(Ind$rs12913832)))
barplot(table(Ind$rs13289)/sum(table(Ind$rs13289)))
barplot(table(Ind$rs13933350)/sum(table(Ind$rs13933350)))
barplot(table(Ind$rs1408799)/sum(table(Ind$rs1408799)))
barplot(table(Ind$rs1426654)/sum(table(Ind$rs1426654)))
barplot(table(Ind$rs16891982)/sum(table(Ind$rs16891982)))
barplot(table(Ind$rs1800407)/sum(table(Ind$rs1800407)))
barplot(table(Ind$rs2031526)/sum(table(Ind$rs2031526)))
barplot(table(Ind$rs2424984)/sum(table(Ind$rs2424984)))
barplot(table(Ind$rs2470102)/sum(table(Ind$rs2470102)))
barplot(table(Ind$rs26722)/sum(table(Ind$rs26722)))
barplot(table(Ind$rs4424881)/sum(table(Ind$rs4424881)))
barplot(table(Ind$rs4911414)/sum(table(Ind$rs4911414)))
barplot(table(Ind$rs6119471)/sum(table(Ind$rs6119471)))
barplot(table(Ind$rs6742078)/sum(table(Ind$rs6742078)))
