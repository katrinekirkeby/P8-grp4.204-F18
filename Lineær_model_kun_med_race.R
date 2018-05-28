#Lineære modeller kun med race
##----PakkeKryds
library(hydroGOF)
##----lmfitkunrace
lmfitrace <- lm(balle~EUR+AFR, data=hudcomplete)
sum <- summary(lmfitrace)
sum$coefficients

##----krydsvalideringlmrace
#Krydsvalidering
CVlmkunrace <- rep(0,10)
idx <- sample(1:10,nrow(hudcomplete),replace = T)
for (i in 1:10){
  train <- hudcomplete[idx!=i,]
  test <- hudcomplete[idx==i,]
  fit <- lm(balle~EUR+AFR,data=train)
  pred <- predict(fit,test)
  CVlmkunrace[i] <- rmse(pred, test$balle)
}
