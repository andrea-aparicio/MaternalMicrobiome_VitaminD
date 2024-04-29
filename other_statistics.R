set.seed(76)

#subject characteristics ----
print("######################### subject characteristics #########################")
#income with education and race
print(chisq.test(dataM$hhincC, dataM$educC,simulate.p.value = T))
print(chisq.test(dataM$hhincC, dataM$mraceth,simulate.p.value = T))
#collection site with baseline and race
print(c("anova basevitdng~sitename",summary(aov(data=dataM, basevitdng~sitename))))
print(chisq.test(dataM$sitename, dataM$mraceth,simulate.p.value = T))
#mean baseline
print(c("mean baseline",mean(dataM$basevitdng)))
#mean difference
print(c("mean difference",mean(dataM$vitDdiff)))
#vit D increase by treatment
print(c("vit D increase treatment High",mean(dataM$vitDdiff[dataM$trmt=="High"])))
print(c("vit D increase treatment Low",mean(dataM$vitDdiff[dataM$trmt=="Low"])))
#treatment and change
print(chisq.test(dataM$trmt,dataM$vitDch))
#treatment and change-baseline
print(chisq.test(dataM$trmt,dataM$ChBase))
#Desulfovibrio ----
print("######################### Desulfovibrio ###############    ##########")

otuwDataD <- subset(makeOTUwData("Desulfovibrio", dataM, "Desu"),
                    select=c("Desu","educC", "mraceth","masthma","hhincC","trmt","basevitdng"))

print(summary(lm(data=otuwDataD, Desu~masthma+educC)))
print(wilcox.test(data=otuwDataD, Desu~educC))
print(wilcox.test(data=otuwDataD[otuwDataD$hhincC!="Refused or Unknown",],Desu~hhincC,simulate.p.value = T))
print(c("anova Desu~mraceth", summary(aov(data=otuwDataD, Desu~mraceth))))
print(c("anova Desu~trmt",summary(aov(data=otuwDataD, Desu~trmt))))
print(summary(lm(data=otuwDataD, Desu~basevitdng+educC)))

otuwData$ChBasef <- as.factor(otuwData$ChBase)
chBase_g<-unique(otuwData$ChBase)
chBaseDesu <- c()

for (i in chBase_g){
  pos<-2
  for (j in chBase_g[pos:length(chBase_g)]){
	if (i!=j){
          dt <- otuwData[otuwData$ChBase==i | otuwData$ChBase==j,]
	  wc <- coin::wilcox_test(data=dt, Desu~ChBasef)
	  chBaseDesu<-rbind(chBaseDesu, c(i,j,pvalue(wc)))
	  }
	pos=pos+1
    }
}

print("exact pairwise wilcoxon Desu~ChBase")
print(chBaseDesu)
print("pairwise wilcoxon Desu~ChBase with fdr correction")
print(pairwise.wilcox.test(otuwData$Desu, otuwData$ChBase,p.adjust.method="fdr"))
