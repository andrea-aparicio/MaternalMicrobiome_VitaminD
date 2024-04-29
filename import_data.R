
#import phyloseq object and metadata
phyObj <- readRDS("/udd/rekle/QIIME2_Pipeline/Phyloseq")
meta <- read.csv(paste(dataloc,"metadata_m_c3_6m.csv",sep=""))

#keep useful variables
t<- c("vid"   ,         "mraceth"  , "mraceth2"  , "mraceth3"  ,      "trmt"    ,      
      "basevitdng"   ,  "wk32to38vitdng", "sitename"    ,   "educ"  ,        
     "hhinc"    ,      "masthma"   ,     "mhayfever"   ,  
     "mage" , "age3_6mcol", "cgender","cbloodvitdng" ,"mrace", "methnicity")
meta<-subset(meta, select = t)

#keep only samples with "Mother FF Third Trimester" collection 
phyObj3 <- subset_samples(phyObj, COLLECTION == "Mother FF Third Trimester")
phyObj3
#120 samples

#get IDs of samples in 3T
sampIDs<-distinct(psmelt(phyObj3) ,vid, .keep_all = TRUE)
t <- c("vid", "COLLECTION", "Sample")
sampIDs<- subset(sampIDs, select=t)
#check
unique(sampIDs$COLLECTION)

#collapse income
unique(meta$hhinc)
meta$hhincC <- "50k or more"
meta$hhincC[meta$hhinc=="Less than 30K" | meta$hhinc== "30-49K"] <- "less than 50k"
meta$hhincC[meta$hhinc=="Refused or Unknown" ] <- "Refused or Unknown"
unique(meta$hhincC)

#keep only samples with "Fecal Flora Collection month 3"  COLLECTION
phyObj3_6m <- subset_samples(phyObj, COLLECTION == "Fecal Flora Collection month 3" )
phyObj3_6m
#265 samples

#get IDs of samples in 3-6m
sampIDs3_6m<-distinct(psmelt(phyObj3_6m) ,vid, .keep_all = TRUE)
t <- c("vid", "COLLECTION", "Sample")
sampIDs3_6m<- subset(sampIDs3_6m, select=t)
#check
unique(sampIDs3_6m$COLLECTION)

#get metadata of 3T subjects
meta3<- merge(sampIDs, meta, by="vid")
colnames(meta3)
length(meta3)

#get metadata of 3-6m subjects
meta3_6m<- merge(sampIDs3_6m, meta, by="vid")
colnames(meta3_6m)
length(meta3_6m)

#rename rows of meta by sample
rownames(meta3)<-meta3$Sample
rownames(meta3_6m)<-meta3_6m$Sample

#put metadada in sample data form
metas3 <- sample_data(meta3)
metas3_6m <- sample_data(meta3_6m)

#merge phy objects to add metadata to sample_data
phy3meta <-merge_phyloseq(phyObj3,metas3)# 
phy3_6mmeta <-merge_phyloseq(phyObj3_6m,metas3_6m)# 

#check
# phy3meta
# phy3_6mmeta
# sample_variables(phy3meta)
# sample_variables(phy3_6mmeta)

#save object 
saveRDS(phy3meta, file=paste(dataloc,"Phyloseq_3meta",sep=""))
saveRDS(phy3_6mmeta, file=paste(dataloc,"Phyloseq_3-6m_meta",sep=""))

#maternal diet data ----
# Load maternal dietary data from 1st and 3rd trimester
trim3 <- read.csv("/proj/stvtds/stvtd00/FINAL_datasets/foodq3rdtrim.csv")

# Keep only dietary variables and vid
keep <- c("vid","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15","q16","q17","q18")
trim3 <- subset(trim3, select=keep)

# Replace factor values with numbers
trim3 <- as.data.frame(lapply(trim3, gsub, pattern = "Less than once per week", replacement = 0, fixed = TRUE))
trim3 <- as.data.frame(lapply(trim3, gsub, pattern = "Once per week", replacement = 1, fixed = TRUE))
trim3 <- as.data.frame(lapply(trim3, gsub, pattern = "2-4 times per week", replacement = 2, fixed = TRUE))
trim3 <- as.data.frame(lapply(trim3, gsub, pattern = "Nearly daily or daily", replacement = 3, fixed = TRUE))
trim3 <- as.data.frame(lapply(trim3, gsub, pattern = "Twice or more per day", replacement = 4, fixed = TRUE))

# Change all variables to numeric
trim3 <- as.data.frame(lapply(trim3, function(x) as.numeric(as.character(x))))

# Make vid rowname and then remove vid variable
rownames(trim3)<-trim3$vid
trim3$vid <- NULL

# Sum total reported food for each subject
trim3$sum <- rowSums(trim3)
# Remove subject who reported 0 to all questions
trim3 <- trim3[trim3$sum != 0,]

# Standardize value for each food to total for each subject
trim3_temp <- as.data.frame(lapply(trim3, function(x) (x / trim3$sum )))
rownames(trim3_temp) <- rownames(trim3)
trim3 <- trim3_temp
rm(trim3_temp)

# Remove sum variable
trim3$sum <- NULL

# Perform PCA
pca_data<-trim3
pca3<-prcomp(pca_data, scale. = TRUE)

# Create dataframe with vid and PC1 to merge into other metadata
pc1<-as.data.frame(pca3$x)
pc1$vid <- rownames(pc1, pc2)
pc1<-subset(pc1, select=c("vid","PC1","PC2"))

write.csv(pc1,paste(dataloc,"foodq3_pc1_pc2.csv",sep=""))

#children outcome data ----
asthma <- read.csv("/proj/stvtds/stvtd00/derived_outcomes/yr3outcome_casedf.csv")
x<-c("vid","asthmawhz","asthma","ezcemawrash")
asthma<-subset(asthma, select=x)
names(asthma) <- c("vid","asthmawhz3","asthma3","eczema3")
load("/proj/stvtds/stvtd0a/R_LIB/vdaartYear6Analysis/data/asthmaDF.rda")
ch <- merge(asthma,asthmaDF,by="vid")
write.csv(ch,paste(dataloc,"childernOutcomes36y.csv",sep=""))
