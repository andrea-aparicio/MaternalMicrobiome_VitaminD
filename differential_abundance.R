
#AncomBC ----
ancom_difabundance_fun <- function(var_g,cova){
  set.seed(65)
  formAnc <-paste(paste(cova, collapse = "+"),var_g,sep="+")
  ABC_out = ancombc(phyloseq = data_frg, formula = formAnc,
                    p_adj_method = "fdr",
                    lib_cut = 0, 
                    group = var_g, struc_zero = TRUE, neg_lb = TRUE, tol = 1e-5, 
                    max_iter = 100, conserve = TRUE, alpha = 0.1, global = TRUE)
  ABC_out_u = ancombc(phyloseq = data_frg, formula = var_g,
                    p_adj_method = "fdr",
                    lib_cut = 0, 
                    group = var_g, struc_zero = TRUE, neg_lb = TRUE, tol = 1e-5, 
                    max_iter = 100, conserve = TRUE, alpha = 0.1, global = TRUE)
  
  #get results (taxon names, pvals, qvals, lfc)
  ABC_resg = ABC_out$res
  ABC_resg$taxonid <- rownames(ABC_resg$lfc)
  AncomPvals <- ABC_resg$p_val
  AncomQvals <- ABC_resg$q_val
  # AncomLogF <- ABC_resg$lfc
  AncomLogF <- ABC_resg$beta
  
  
  ABC_resg_u = ABC_out_u$res
  ABC_resg_u$taxonid <- rownames(ABC_resg_u$lfc)
  AncomPvals_u <- ABC_resg_u$p_val
  AncomQvals_u <- ABC_resg_u$q_val
  # AncomLogF_u <- ABC_resg_u$lfc
  AncomLogF_u <- ABC_resg_u$beta
  
  #get the column names that ancom gave to the comparison group
  cn_ancom<-colnames(AncomPvals)[grepl(var_g,colnames(AncomPvals))]
  
  resSig<-c()
  resSig_u<-c()
  for (i in cn_ancom){
    #find taxa with significant pvalues
    AncomPsig_u <- filter(AncomPvals_u, .data[[i]] < 0.05)
    pSig_u <- AncomPsig_u[[i]]
    
    AncomPsig <- filter(AncomPvals, .data[[i]] < 0.05)
    pSig <- AncomPsig[[i]]
    
    #get taxa names, qvalues and lfc
    taxSig_u <- rownames(AncomPsig_u)
    qSig_u <- AncomQvals_u[taxSig_u,i]
    lfcSig_u <- AncomLogF_u[taxSig_u,i]
    varSig_u <- rep(i,length(pSig_u))
    resSig_u <- rbind(resSig_u,data.frame("group"=varSig_u,"taxa"=taxSig_u, "pval"=pSig_u, "qval"=qSig_u, "lfc"=lfcSig_u))
    
    taxSig <- rownames(AncomPsig)
    qSig <- AncomQvals[taxSig,i]
    lfcSig <- AncomLogF[taxSig,i]
    varSig <- rep(i,length(pSig))
    resSig <- rbind(resSig,data.frame("group"=varSig,"taxa"=taxSig, "pval"=pSig, "qval"=qSig, "lfc"=lfcSig))
    
  }
  print(paste("ANCOM-BC: by",var_g,"adjusted for",paste(cova, collapse=", ")))
  print(resSig)
  print(paste("ANCOM-BC: by",var_g))
  print(resSig_u)
  return(list(resSig, resSig_u))
  }


ancom_plot_fun <- function(resSig,colsSig,labsSig){
  ggplot(resSig, aes(y=lfc, x=taxa), colour=taxa)+
    geom_bar(stat = "identity",fill=colsSig, width = .7)+
                                                  theme_bw()+
    theme(text = element_text(size = 16),axis.title.x=element_text(size=16),
          axis.text.x=element_text(size=14, angle = 45, vjust = .9, hjust=.9))+
    labs(x="genus",  y="log fold change")+
    scale_x_discrete(labels=labsSig)
    ggsave(paste(figsloc,"fig_3-a.png",sep=""),dpi=300)
    print("saved fig_3-a.png")
    
}





#Maaslin ----
maaslin_association_fun <- function(var_g,cova,refsMas){
  formMas <- append(var_g,cova)
  fit_data <- Maaslin2(
    input_data = as.data.frame(otufrg),
    input_metadata = dataM,
    output = "DAA example",
    transform = "AST",
    fixed_effects = formMas,
    reference = refsMas,  
    plot_heatmap = F,
    plot_scatter = F
  )
  
  ml_res<-filter(fit_data$results, qval <= 0.05)
  ml_res<-subset(ml_res, select = c("feature","metadata","value","coef","stderr","pval","qval"))
  ml_res$feature<-sub(".", "", ml_res$feature)
  ml_res$Genus<-tax1[ml_res$feature,"Genus"]
  print(ml_res)
  return(ml_res)
  }
