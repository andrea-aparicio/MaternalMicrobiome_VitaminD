#TABLE1----
#function for categorical variables
table1_fun <- function(var_g, dataM, variabsall, var_cat, wr){
  #define categorical variables
  catVars <- variabsall[! variabsall %in% var_cat]
  tablet<- CreateTableOne(vars = variabsall, strata = var_g, data=dataM, 
                          factorVars = catVars,addOverall = TRUE)
  if (wr==TRUE){
    write.csv(print(tablet, showAllLevels=TRUE), 
              paste(dataloc,"Table1_cat_",var_g,".csv", sep=""))}
  return(tablet)
}

#function for continuous variables
table1_cont_fun <-  function(var_g, dataM, variabsall, var_cont, wr){
  table_cont<-c()
  #define categorical variables
  catVars <- variabsall[! variabsall %in% var_cont]
  var_g_data <- dataM[[var_g]]
  #get metrics for each variable
  #anova for categorical variables
  for (i in catVars){
    for (j in unique(dataM[[i]])){
      group_mean <- mean(var_g_data[dataM[[i]]==j])
      group_sd <- sd(var_g_data[dataM[[i]]==j])
      pval<-summary(aov(data=dataM, as.formula(paste(var_g,i,sep="~"))))[[1]][["Pr(>F)"]][1]
      table_cont<-rbind( table_cont,c(i, j,group_mean, group_sd, pval))
    }
  }
  #chi square test for continuous variables
  for (i in var_cont){
    group_mean <- mean(dataM[[i]])
    group_sd <- sd(dataM[[i]])
    pval <- chisq.test(dataM[[var_g]], dataM[[i]], simulate.p.value = TRUE)$p.value
    table_cont<-rbind( table_cont,c(i, 0,group_mean, group_sd, pval))
  }
  colnames(table_cont) <- c("var", "group", "mean", "sd", "pval")
  if (wr==TRUE){
    write.csv(table_cont, paste(dataloc, "Table1_cont_",var_g,".csv", sep=""))
  }
  return(table_cont) 
}

