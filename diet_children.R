#diet ----
food <- read.csv(paste(dataloc,"foodq3_pc1_pc2.csv", sep=""))

otuwData <- makeOTUwData("Desulfovibrio", dataM, "Desu")
otuwDataFood <- merge(otuwData,food , by="vid")

#associations with principal components
summary(lm(PC1~Desu, data=otuwDataFood))
summary(lm(PC2~Desu, data=otuwDataFood))
#adjusted
summary(lm(PC1~Desu+educC, data=otuwDataFood))
summary(lm(PC2~Desu+educC, data=otuwDataFood))

#present/absent taxa
otuwDataFood$DesuB <- ifelse(otuwDataFood$Desu==0,0,1)
otuwDataFood$DesuB<- as.factor(otuwDataFood$DesuB)

#unadjusted
summary(lm(PC1~DesuB, data=otuwDataFood))
summary(lm(PC2~DesuB, data=otuwDataFood))
#adjusted
print("linear regression PC1 and present/absent Desulfovibrio, adjusted for education")
print(summary(lm(PC1~DesuB+educC, data=otuwDataFood)))
print("linear regression PC2 and present/absent Desulfovibrio, adjusted for education")
print(summary(lm(PC2~DesuB+educC, data=otuwDataFood)))

#children ----

childrenH <- read.csv(paste(dataloc,"childernOutcomes36y.csv", sep=""))
childrenH$X<-NULL
colnames(childrenH)<-c("vid","asthmawhz3","asthma3","eczema3","asthma6","whz6","asthmawhz6","lowerbAsthma","upperbAsthma","pseudoTime","pseudoEvent")
tax36 <-  as.data.frame(tax_table(phy36m_rg))
taxid <- rownames(tax36)[tax36$Genus=="Desulfovibrio"]
otu36trans <- as.data.frame(t(otu36rg))
Desulfo36m<-data.frame("Desulfovibrio"=otu36trans[[taxid]])
Desulfo36m$Sample <- rownames(otu36trans)

metaWDesulfo36m <- merge(data36,Desulfo36m,by="Sample")

#samples with Desulfovibrio
#counti
child_samples <- dim(metaWDesulfo36m)[1]
print(paste("total number of children's samples:" , child_samples))
child_desulfo <- sum(metaWDesulfo36m$Desulfovibrio !=0)
print(paste("children's samples with Desulfovibrio present:",child_desulfo))
prop_child_desulfo <-round(100* child_desulfo/child_samples,2)
print(paste("proportion of children's samples with Desulfovibrio present:", prop_child_desulfo,"%"))
#mothers' race
print(c("race of the mothers whose children had Desulfovibrio present:", unique(metaWDesulfo36m$mraceth[metaWDesulfo36m$Desulfovibrio !=0])))

#children with asthma
childrenHmetaDesu <- merge(childrenH,metaWDesulfo36m, by="vid")
print(c("children with asthma at 3 y.o:",sum(childrenHmetaDesu$asthma3[childrenHmetaDesu$Desulfovibrio!=0]  )))
print(c("children with asthma at 6 y.o:",sum(childrenHmetaDesu$asthma6[childrenHmetaDesu$Desulfovibrio!=0]  )))

