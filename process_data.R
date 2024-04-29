#Function to import and filter object with metadata----
import_filter_phy <- function(PhyloObj, pruneq){
  phy3i<-readRDS(paste(dataloc,PhyloObj, sep=""))
  phy3i
  #14488 taxa and 120 samples
  #14488 taxa and 265 samples 3-6m
  
  if (pruneq == T){
    #prune samples >1000 reads
    phy3p <- prune_samples(sample_sums(phy3i) > 1000, phy3i)
    phy3p
    #14488 taxa and 118 samples 
    #14488 taxa and 255 samples  3-6m
  } else{
    phy3p<-phy3i
  }
  
  # filter OTUs present in none of the samples
  phy3_f <- filter_taxa(phy3p, function(x) sum(x > 1) > 0, TRUE)
  phy3_f
  #4170 taxa and 118 samples
  #2902 taxa and 255 samples 3-6m
  
  #add new groups to meta data
  mdata <- sample_data(phy3_f)
  colnames(mdata)
  #unique(mdata$educ)
  mdata$educC <- ifelse( mdata$educ == "Less than high school" , "high/technical school or less", "college or more") 
  mdata$mageC <- ifelse(mdata$mage<30, "<30", ">=30")
  mdata$vitDdiff <- mdata$wk32to38vitdng - mdata$basevitdng
  mdata$vitDch <- ifelse(mdata$vitDdiff<10, "L", "H")
  mdata$baseC <- ifelse(mdata$basevitdng < 20, "L", "H")
  mdata$baseC2 <- ifelse(mdata$basevitdng > mean(mdata$basevitdng), "L", "H")
  mdata$baseWtrmt <- paste(mdata$baseC,mdata$trmt, sep="")
  mdata$baseWch <- paste(mdata$baseC,mdata$vitDch, sep="")
  mdata$trmtWch <- paste(mdata$trmt,mdata$vitDch, sep="")
  mdata$doseLchH <- ifelse(mdata$trmtWch == "BH",1,0)
  mdata$trmt <- ifelse(mdata$trmt=="A", "High", "Low")
  mdata$vitDdiffpercent <- 100*mdata$vitDdiff/mdata$basevitdng
  #class(mdata)
  
  #put metadata in dataframe form
  dataMt<-data.frame(mdata)
  # class(dataMt)
  rownames(dataMt) <- rownames(mdata)
  colnames(dataMt) <- colnames(mdata)
  
  #return metadata to sample data form
  rownames(mdata)<-mdata$Sample
  mdatas <- sample_data(mdata)
  head(mdatas)
  
  #add metadata to phyl object
  phy3t <-merge_phyloseq(phy3_f,mdatas) 
  phy3t
  
  #2902 taxa and 255 samples 3-6m
  
  return(list(phy3t,dataMt))
}

#Functions to remove NAs from phylo and metadata dataframe----
#identify NAs
getNAlist <- function(datamt, vart){
  sampNA<-datamt$Sample[which(is.na(datamt[vart]))]
  return(sampNA)
}
#remove NAs
removeNAs <- function(phylist,sampNA){
  phy3t <- phylist[[1]]
  dataMt <- phylist[[2]]

  print(sampNA)
  phy3<-subset_samples(phy3t, !(Sample %in% as.vector(sampNA)))
  phy3
  dataM <- dataMt[!(row.names(dataMt) %in% as.vector(sampNA)),]
  return(list(phy3,dataM))
}

#Import data ----
#Mothers' data with fecal sample at third trimester
phylo3list <- import_filter_phy("Phyloseq_3meta", T) 
sampNA<- getNAlist(phylo3list[[2]], "vitDch")
phylo3list<- removeNAs(phylo3list, sampNA)
phy3 <- phylo3list[[1]]
dataM <- phylo3list[[2]]
#4170 taxa and 114 samples

#Children's data 3-6 months
phylo36list <- import_filter_phy("Phyloseq_3-6m_meta", T)
sampNA36<- getNAlist(phylo36list[[2]], "vitDch")
phylo36list<- removeNAs(phylo36list, sampNA36)
phylo36 <- phylo36list[[1]]
data36 <-  phylo36list[[2]]

#Add and modify fields to mothers' metadata ----
dataM$education <- ifelse(dataM$educC == "college or more", "Higher", "Lower")
dataM$income <- dataM$hhinc
dataM$income <- ifelse(dataM$hhincC == "50k or more", "Higher", 
                       ifelse(dataM$hhincC=="less than 50k", "Lower", "R"))
dataM$race <- ifelse(dataM$mraceth == "White", "White", "Non-white")
dataM$site <- ifelse(dataM$sitename == "bos", "Boston", 
                     ifelse(dataM$sitename=="sd", "SD", "St. Louis"))
dataM$asthma <- dataM$masthma
dataM["hay fever"] <- dataM$mhayfever

dataM$baseDich <- ifelse(dataM$basevitdng>mean(dataM$basevitdng), "H", "L")
dataM$ChBase <- "baseL-changeL"
dataM$ChBase[dataM$baseDich == "L" & dataM$vitDch == "H"] <- "baseL-changeH"
dataM$ChBase[dataM$baseDich == "H" & dataM$vitDch == "L"] <- "baseH-changeL"
dataM$ChBase[dataM$baseDich == "H" & dataM$vitDch == "H"] <- "baseH-changeH"

#Process data for diversity analysis ----
#extract otu tables
otu3<-as.data.frame(otu_table(phy3))
otu36m<-as.data.frame(otu_table(phylo36))

# filter taxa with <10% prevalence and <=3 counts
phy3_ff <- filter_taxa(phy3, function(x) sum(x > 3) > (0.10*length(x)), TRUE)
#phy3_ff

phy36m_ff <- filter_taxa(phylo36, function(x) sum(x > 3) > (0.10*length(x)), TRUE)
#phy36m_ff
#182 taxa and 244 samples

#transform to relative abundance
phy3_fr <- transform_sample_counts(phy3_ff, function(x) x/sum(x))
#phy3_fr
#collapse to genus level
data_frg <- tax_glom(phy3_fr, taxrank="Genus", NArm = T)
#extract otu table
otufrg <- as.data.frame(otu_table(data_frg))
#get taxonomic anotations
tax1 <- as.data.frame(tax_table(data_frg))

phy36m_fr <- transform_sample_counts(phy36m_ff, function(x) x/sum(x))
#phy36m_fr

#transform to relative abundance without final filtering
phy36m_r <- transform_sample_counts(phylo36, function(x) x/sum(x))
#phy36m_r
phy36m_rg <- tax_glom(phy36m_r, taxrank="Genus", NArm = T)
otu36rg <- otu_table(phy36m_rg)

#transform to present/absent
phy3_frb <- transform_sample_counts(phy3_fr, function(x) ifelse(x>0,1,0))





