rm(list=ls())
library(tableone)
library(phyloseq)
library(dplyr)
library(reshape)
library(vegan)
library(dplyr)
library(ANCOMBC)
#library(ALDEx2)
library(tibble)
library(ggplot2)
#library(microbiomeMarker)
library(Maaslin2)
library(rbiom)
library(stringr)
library(biomformat)
library(gtools)
library(cowplot)
library(ggpubr)

#colors for plots
treatment_colors <-c("#de0076","#59971c")
treatment_colors2 <- c("#de0076","#de9000", "#59971c")

#PREPARE DATA ----
#figs location
figsloc <- "/udd/spaap/Projects/Maternal_Microbiome_VitD/msPackage/figs/"
dataloc <- "/udd/spaap/Projects/Maternal_Microbiome_VitD/msPackage/data/"

#scale the OTUs (function)
scale_reads <- function(physeq, number) {
  physeq.scale <-
    transform_sample_counts(physeq, function(x) {
      (number * x/sum(x))
    })
  otu_table(physeq.scale) <- floor(otu_table(physeq.scale))
  physeq.scale <- prune_taxa(taxa_sums(physeq.scale) > 0, physeq.scale)
  return(physeq.scale)
}
#make OTU wit data (function)
makeOTUwData <- function(taxa,metadata,col_name){
  
  rnDes <-rownames(tax1)[tax1$Genus == taxa]
  
  otuwData <- as.data.frame( t(otufrg))
  otuwData$Sample <- colnames(otufrg)
  otuwData <- merge(metadata, otuwData, by= "Sample", all=TRUE)
  otuwData[[col_name]] <- as.numeric(otuwData[[rnDes]])
  #head(otuwData)
  return(otuwData)
}
