
#scale the OTUs
phy3_scale <-scale_reads(phy3,1000) # by 1000
phy3_scale

#define methods to try
meth= c("bray", "jaccard", "unifrac" , "wunifrac")

# # --Create metadata data frames for scaled samples ----
# sampledfm <- data.frame(sample_data(phy3_scale))
# colnames(sampledfm)
##using original metadata (if no additional filtering)
#sampledfm <- dataM

#list to store distances
dist_list <- vector("list", length(meth))
melt_dist_list <- vector("list", length(meth))
names(dist_list) = meth
names(melt_dist_list) = meth

#calculate distances for all methods
for (i in meth){
  print(i)
  #calculate distance
  dist <- phyloseq::distance(phy3_scale, method = i)
  #store distances
  dist_list[[i]] <- dist
  distdf <- melt(as.matrix(dist))
  colnames(distdf) <- c("x1", "x2", i)
  distdf$x12 <- paste(distdf$x1,distdf$x2,sep="_")
  melt_dist_list[[i]] <- distdf
}

#permanova
perma_dist_fun <- function(var_g, cova){
  #build formula 
  conf <-paste("+",cova,collapse = "")
  f <- as.formula(paste("dist~",var_g,conf, sep=""))
  #empty vectors to store pvals
  pval_permaU <- c()
  pval_permaUunad <- c()
  for (i in meth){
    set.seed(28)
    dist<- dist_list[[i]] 
    pn<-adonis2(f, data = dataM)
    pn_unadj<-adonis2(as.formula(paste("dist~",var_g, sep="")), data = dataM)
    ltemp<- data.frame("variable"=rownames(pn), "pval"=pn$`Pr(>F)`)
    ltemp_unad<- data.frame("variable"=rownames(pn_unadj), "pval"=pn_unadj$`Pr(>F)`)
    ltemp$method <- i
    ltemp_unad$method <- i
    pval_permaU<-rbind(pval_permaU,ltemp)
    pval_permaUunad<-rbind(pval_permaUunad,ltemp_unad)
  }
  pval_permaU<-na.omit(pval_permaU)
  pval_permaUunad<-na.omit(pval_permaUunad)
  print(paste("permanova: distance by ",var_g))
  print(pval_permaUunad)
  print(paste("permanova: distance by ",var_g,"adjusted for",paste(cova,collapse=", ")))
  print(pval_permaU)
  return(list( pval_permaU, pval_permaUunad))

}
