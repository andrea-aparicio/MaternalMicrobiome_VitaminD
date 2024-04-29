#########################
## COMPILE METADATA
#########################

# Load in data with demographics and maternal VDAART treatment assignment
meta <- read.csv("/proj/stvtds/stvtd00/FINAL_datasets/babyracen819.csv")
# colnames(meta)
# Create a child race/ethnicity variable
meta$mraceth <- 3
meta$mraceth[meta$mrace == "White"] <- 1
meta$mraceth[meta$mrace == "Black or African American"] <- 2
meta$mraceth[meta$methnicity == "Hispanic or Latino"] <- 3
meta$mraceth <- factor(meta$mraceth,
                       levels = c(1,2,3),
                       labels = c("White", "Black","Other"))

meta$mraceth3 <- 3
meta$mraceth3[meta$mrace == "White"] <- 3

meta$mraceth3[meta$mrace == "Black or African American"] <- 2
meta$mraceth3[meta$methnicity == "Hispanic or Latino"] <- 1
meta$mraceth3 <- factor(meta$mraceth3,
                       levels = c(1,2,3),
                       labels = c("Hispanic/Latino", "Black","Other"))

meta$mraceth2 <- 4
meta$mraceth2[meta$mrace == "White"] <- 1

meta$mraceth2[meta$mrace == "Black or African American"] <- 2
meta$mraceth2[meta$methnicity == "Hispanic or Latino"] <- 3
meta$mraceth2 <- factor(meta$mraceth2,
                       levels = c(1,2,3,4),
                       labels = c("White", "Black","Hispanic/Latino","Other"))

x<-c("vid","mraceth","trmt","deliverydate","cgender","mrace","methnicity","mraceth2","mraceth3")
meta<-subset(meta, select=x)
# colnames(meta)

# For trmt, A = high dose vit D, B = low dose vit D

# Add in vitamin D levels
vitd <- read.csv("/proj/stvtds/stvtd00/FINAL_datasets/vitdfinalMar2022.csv")
# colnames(vitd)
# Keep only vid and hihi/lolo variable
x2 <- c("vid","basevitdng","wk32to38vitdng","cbloodvitdng")
vitd <- subset(vitd, select=x2)

# Merge in with rest of metadata
meta <- merge(meta, vitd, by="vid", all.x=TRUE)

#### Other variables
epi2 <- read.csv("/proj/stvtds/stvtd00/FINAL_datasets/basecharn876.csv")
# colnames(epi2)
x<-c("vid","sitename","educ","hhinc","masthma","mhayfever","mage")
epi2<-subset(epi2, select=x)
#Merge demographics
meta <- merge(meta, epi2, by="vid", all.x=TRUE)

write.csv(meta, paste(dataloc,"metadata_m_c3_6m.csv",sep=""))
