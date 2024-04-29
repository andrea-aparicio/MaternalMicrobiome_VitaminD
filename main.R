# Clear workspace
rm(list=ls())
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%---- section 0
# load dependencies and paths
source("dependencies.R")

#-------- Run this to import data ------
#only necessary if files Phyloseq_3meta and 
#Phyloseq_3-6m_meta are not in the /data folder
source("compile_metadata")
source("import_data")
#---------------------------------------------

#Process data using phyloseq object
source("process_data.R")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%---- section 1
#Table 1, Table 2, Table S1, Tabe S2----
source("table1.R")
# table 1 by baseline (continuous)
#variables to include
var_table1 <- c("mraceth", "hhincC", "educC",  "site", "masthma", "mhayfever", 
                "mage", "vitDdiff", "trmt")
#continuous variables
contvars_table1 <- c("vitDdiff", "mage")
#set to true to write table csv
save_table1 <- F
#create table 1
t1c<-table1_cont_fun("basevitdng", dataM, var_table1, contvars_table1, save_table1 )
t1c

#table 2 by treatment (categorical)
stratvar_table2 <- "trmt"
var_table2 <- c("mraceth", "hhincC", "educC", "basevitdng", "site", "masthma", "mhayfever", 
                "mage")
contvars_table2 <- c("basevitdng", "mage")
save_table2 <- F
table_2 <- table1_fun(stratvar_table2, dataM, var_table2, contvars_table2, save_table2)
table_2

#table S1
stratvar_tableS1 <- "ChBase"
var_tableS1 <- c("mraceth", "hhincC", "educC",  "basevitdng", "site", "masthma", "mhayfever", 
                "mage")
contvars_tableS1 <- c("basevitdng", "mage")
table_S1 <- table1_fun(stratvar_tableS1, dataM, var_tableS1, contvars_tableS1, F)
table_S1

#table S2
stratvar_tableS2 <- "vitDch"
var_tableS2 <- c("mraceth", "hhincC", "educC",  "basevitdng", "site", "masthma", "mhayfever", 
                 "mage", "trmt")
contvars_tableS2 <- c("basevitdng", "mage")
table_S2 <- table1_fun(stratvar_tableS2, dataM, var_tableS2, contvars_tableS2, F)
table_S2
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%---- section 2
#Alpha diversity ----
source("alpha_diversity.R")
var_g <- "vitDch" #stratifying variable ("basevitdng", "trmt","vitDch","ChBase")
cova <- c("mraceth", "educC") #covariates
#linear regression 
lm_alpha <- lm_richness_fun(var_g,cova)
#plots alpha diversity
source("plots_alpha_diversity.R") #fig 2.a, fig 2.c, fig S1.b
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ---- section 3
#Beta diversity ----
source("beta_diversity.R")
var_g<-"vitDch" #stratifying variable  ("basevitdng", "trmt","vitDch","ChBase")
cova <- c("mraceth","educC") #covariates
#permanova
pnv_beta <- perma_dist_fun(var_g, cova) 
#plots beta diversity
source("plots_beta_diversity.R") #figures 2.b, 2.d, S1.c, S1.d
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ---- section 4
#Diferential abundance ----
source("differential_abundance.R") 

  #Ancom-BC ----
var_g<-"vitDch" #stratifying variable - must be categorical ("baseC2", "trmt","vitDch")
cova <- c("mraceth","educC") #covariates
ancom_differential <- ancom_difabundance_fun(var_g,cova) 
#plot for vitamin D change figure 3.a
if (var_g=="vitDch"){
  ancomplot_data<- ancom_differential[[1]]
  ancomplot_labs<- c("Clostridium_sensu_stricto_1"="C. sensu stricto 1" )
  ancomplot_cols<-c( "#63b0e4","#bd93d6","#9bcd79","#e1966e")
  ancom_plot_fun(ancomplot_data,ancomplot_cols,ancomplot_labs)
}


  #Maaslin2----
var_g = "vitDch" #stratifying variable ("basevitdng", "trmt","vitDch", "ChBase")
cova <- c("mraceth", "educC")
refs_groups <- c("mraceth", "White") #reference groups for variables with more than 2 groups ("mraceth", "White", "ChBase","BLCH")
maaslin_association <- maaslin_association_fun(var_g,cova,refs_groups)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ----section 5
#food questionaire  and children's samples----
source("diet_children.R")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ---- section 6
# additional statistics ----
source("other_statistics.R")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ---- section 7
# plots (fig 1a-d, fig 3b-c, fig S1a, fig S2, fig S3a-b,) ----
source('other_plots.R')
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ----



