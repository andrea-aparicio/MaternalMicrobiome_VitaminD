#ALPHA DIVERSITY----

#calculate richness for every measure
rich = estimate_richness(phy3)

#make dataframe with richness and metadata to test associations
cn <- colnames(rich)
rich$Sample <- rownames(dataM)
dataRich <- merge(rich,dataM, by="Sample",all=TRUE)

#lineal regression with covariates function
lm_richness_fun <- function(var_g,conf){
  #empty vectors to store pvalues
  pvalt<-c()
  pvalt_nc<-c()
  #linear regression
  for (i in cn){
    # print(i)
    formC <- paste( c(var_g,conf), collapse="+")
    form <- paste(i, formC, sep="~")
    tlm<-lm(as.formula(form), dataRich)
    tlm_nocov<-lm(as.formula(paste(i, var_g, sep="~")), dataRich)
    t<-summary(tlm)
    tp<-t$coefficients[,4][[2]]
    tp_nocov <- summary(tlm_nocov)$coefficients[,4][[2]]
    pvalt<- append(pvalt,tp)
    pvalt_nc<- append(pvalt_nc,tp_nocov)
  }
  pval_lm_rich <- data.frame("method" = cn, "pval_adjusted"=pvalt, "pval_not_adjusted"=pvalt_nc)
  print(paste("linear regression: richness by",var_g,"adjusted for",paste(cova,collapse=", ")))
  print(pval_lm_rich)
  return(pval_lm_rich)
}
