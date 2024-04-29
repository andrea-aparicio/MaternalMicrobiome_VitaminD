
#baseline ----
j<- "basevitdng"

#metadata subset
sd<-dataM %>%
  subset(select=c("Sample",j,cova)) %>%
  mutate_if(is.factor,as.character)

#merge the pair's metadata with distances for all 4 methods
dist_m <- melt_dist_list[[1]]
colnames(sd) <- c("x1", "T1", "mraceth1", "educC1")
dist_sd <- left_join(dist_m, sd, by="x1")
colnames(sd) <- c("x2", "T2", "mraceth2", "educC2")
dist_sd <- left_join(dist_sd, sd, by="x2")
dist_sd$x1<- NULL
dist_sd$x2<- NULL

for (i in c(2,3,4)){
  dist_m <- melt_dist_list[[i]]
  dist_m$x1<- NULL
  dist_m$x2<- NULL
  dist_sd <- merge(dist_sd, dist_m, by="x12")}

#calulate the baseline difference between every pair of samples
dist_sd$T12 <- abs(as.numeric(dist_sd$T1)-as.numeric(dist_sd$T2))

#make groups for race
dist_sd$race12<- paste(pmin(dist_sd$mraceth1, dist_sd$mraceth2),
                       pmax(dist_sd$mraceth1, dist_sd$mraceth2), sep="-")
dist_sd$mraceth1<- NULL
dist_sd$mraceth2<- NULL

#make groups for education
dist_sd$educ12<- paste(pmin(dist_sd$educC1, dist_sd$educC2),
                       pmax(dist_sd$educC1, dist_sd$educC2), sep="-")
dist_sd$educC1<-NULL
dist_sd$educC2<-NULL
# head(dist_sd)

#run linear regression with covariates for every method
pval_betat <-c()
for (i in meth){
  f<- as.formula(paste(i,"T12+race12+educ12",sep="~"))
  lt <- summary(lm(data=dist_sd, f,))$coefficients[,4][[2]]
  pval_betat <- append(pval_betat,lt)
}
#gather pvals in dataframe
pval_beta <- data.frame(pt=pval_betat)
colnames(pval_beta)<-j
rownames(pval_beta) <- meth

#make plot
datadistMelt <- melt(dist_sd, measure= meth, id = "T12")

ggplot(datadistMelt, aes(x =T12, y = value)) +
  geom_point(color= "midnightblue", size=.5,alpha=0.05)+
  stat_smooth(
    method = "lm",
    size = .5,
    color = "red",
    na.rm = TRUE,fill="green",alpha=.8, level=.95
  ) +
  facet_wrap(~ variable,scales="free",ncol=4,labeller = labeller(variable = c("bray"="Bray-Curtis", "jaccard"=
                                                                                "Jaccard","unifrac"= "Unifrac","wunifrac"= "W-Unifrac")))+   
  labs(x="baseline vitamin D difference (ng/mL)", y="beta diversity measure")+
  theme_bw()+
  theme(text = element_text(size = 16),axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14), strip.background =  element_rect( fill="white"))
ggsave(paste(figsloc,"fig_2-b.png",sep=""),dpi=300)
print("saved fig_2-b.png")

print("adjusted linear regression pvalues distance~difference in baseline vitD between subjects")
print(pval_beta)

# treatment ----

j<- "trmt"

sd<-dataM %>%
  #get selecter variable
  subset(select=c("Sample",j,cova)) %>%
  mutate_if(is.factor,as.character)

dist_m <- melt_dist_list[[1]]
colnames(sd) <- c("x1", "T1", "mraceth1", "educC1")
dist_sd <- left_join(dist_m, sd, by="x1")
colnames(sd) <- c("x2", "T2", "mraceth2", "educC2")
dist_sd <- left_join(dist_sd, sd, by="x2")
dist_sd$x1<- NULL
dist_sd$x2<- NULL

for (i in c(2,3,4)){
  dist_m <- melt_dist_list[[i]]
  dist_m$x1<- NULL
  dist_m$x2<- NULL
  dist_sd <- merge(dist_sd, dist_m, by="x12")}


dist_sd$race12<- paste(pmin(dist_sd$mraceth1, dist_sd$mraceth2),
                       pmax(dist_sd$mraceth1, dist_sd$mraceth2), sep="-")
dist_sd$mraceth1<- NULL
dist_sd$mraceth2<- NULL
dist_sd$educ12<- paste(pmin(dist_sd$educC1, dist_sd$educC2),
                       pmax(dist_sd$educC1, dist_sd$educC2), sep="-")
dist_sd$educC1<-NULL
dist_sd$educC2<-NULL

dist_sd$T12<- paste(pmin(dist_sd$T1, dist_sd$T2),
                    pmax(dist_sd$T1, dist_sd$T2), sep="-")

# head(dist_sd)

#make plot
datadistMelt <- melt(dist_sd, measure= meth, id = "T12")

ggplot(datadistMelt, aes(x =T12, y = value)) +
  geom_violin(aes(fill=T12), alpha=.8, linetype=0,show.legend = F)+
  stat_summary(fun.data=mean_sdl,  geom="pointrange", color="gray31")+
  facet_wrap(~ variable,  scales='free',ncol = 4,
             labeller = labeller(variable = 
                                   c("bray"="Bray-Curtis", "jaccard"="Jaccard",
                                     "unifrac"= "Unifrac","wunifrac"= "W-Unifrac")))+   
  scale_colour_manual(values = "gray")+
  scale_fill_manual(values = treatment_colors2)+
  labs(y="beta diversity measure", x="treatment")+
  theme_bw()+
  theme(text = element_text(size = 16),axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14), strip.background =  element_rect( fill="white"))
ggsave(paste(figsloc,"fig_2-d.png",sep=""),dpi=300)
print("saved fig_2-d.png")

# Baseline-change ----

j<- "ChBase"

sd<-dataM %>%
  #get selecter variable
  subset(select=c("Sample",j,cova)) %>%
  mutate_if(is.factor,as.character)

dist_m <- melt_dist_list[[1]]
colnames(sd) <- c("x1", "T1", "mraceth1", "educC1")
dist_sd <- left_join(dist_m, sd, by="x1")
colnames(sd) <- c("x2", "T2", "mraceth2", "educC2")
dist_sd <- left_join(dist_sd, sd, by="x2")
dist_sd$x1<- NULL
dist_sd$x2<- NULL

for (i in c(2,3,4)){
  dist_m <- melt_dist_list[[i]]
  dist_m$x1<- NULL
  dist_m$x2<- NULL
  dist_sd <- merge(dist_sd, dist_m, by="x12")}


dist_sd$race12<- paste(pmin(dist_sd$mraceth1, dist_sd$mraceth2),
                       pmax(dist_sd$mraceth1, dist_sd$mraceth2), sep="-")
dist_sd$mraceth1<- NULL
dist_sd$mraceth2<- NULL
dist_sd$educ12<- paste(pmin(dist_sd$educC1, dist_sd$educC2),
                       pmax(dist_sd$educC1, dist_sd$educC2), sep="-")
dist_sd$educC1<-NULL
dist_sd$educC2<-NULL

dist_sd$T12<- paste(pmin(dist_sd$T1, dist_sd$T2),
                    pmax(dist_sd$T1, dist_sd$T2), sep="_")
#head(dist_sd)

me<-pairwise.wilcox.test(dist_sd[[i]],dist_sd$T12, p.adjust.method = "fdr")
print(paste("paiwise wilcoxon tests",j,"by",i))
print(me)
# class(me)

#make plots 

# between groups
datadistMelt_btw <- melt(dist_sd[dist_sd$T1 != dist_sd$T2,], measure= meth, id = "T12")

print("anova distanve~ChBase between groups")
for (i in unique(datadistMelt_btw$variable)){
  print(i)
  print(summary(aov(data=datadistMelt_btw[datadistMelt_btw$variable==i,], value~T12)))
}

ggplot(datadistMelt_btw, aes(x =T12, y = value)) +
  geom_violin(aes(fill=T12), alpha=.8, linetype=0,show.legend = T)+
  stat_summary(fun.data=mean_sdl,  geom="pointrange", color="gray31")+
  facet_wrap(~ variable,  scales='free',ncol = 4,
             labeller = labeller(variable = 
                                   c("bray"="Bray-Curtis", "jaccard"="Jaccard",
                                     "unifrac"= "Unifrac","wunifrac"= "W-Unifrac")))+   
  scale_colour_manual(values = "gray")+
  scale_fill_manual(values = c("#bece7b",
                               "#d03acf",
                               "#3e8848",
                               "#7539ac",
                               "#82401d",
                               "#6f9aff",
                               "#ff898b",
                               "#0062b2",
                               "#daad2b",
                               "#756975")#, 
                    #labels = c()
  )+
  labs(y="beta diversity measure", title= "between groups comparisons")+
  theme_bw()+
  theme(text = element_text(size = 16),axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        strip.background =  element_rect( fill="white"))
ggsave(paste(figsloc,"fig_S1-c.png",sep=""),dpi=300)
print("saved fig_S1-c.png")


#melt within groups
datadistMelt_wtn <- melt(dist_sd[dist_sd$T1 == dist_sd$T2,], measure= meth, id = "T12")

print("anova distanve~ChBase within groups")
for (i in unique(datadistMelt_wtn$variable)){
  print(i)
  print(summary(aov(data=datadistMelt_wtn[datadistMelt_wtn$variable==i,], value~T12)))
}

ggplot(datadistMelt_wtn, aes(x =T12, y = value)) +
  geom_violin(aes(fill=T12), alpha=.8, linetype=0,show.legend = T)+
  stat_summary(fun.data=mean_sdl,  geom="pointrange", color="gray31")+
  facet_wrap(~ variable,  scales='free',ncol = 4,
             labeller = labeller(variable = 
                                   c("bray"="Bray-Curtis", "jaccard"="Jaccard",
                                     "unifrac"= "Unifrac","wunifrac"= "W-Unifrac")))+   
  scale_colour_manual(values = "gray")+
  scale_fill_manual(values = c("#ff898b",
                               "#0062b2",
                               "#daad2b",
                               "#756975")#, 
                    #labels = c()
  )+
  labs(y="beta diversity measure", title="within group comparisons")+
  theme_bw()+
  theme(text = element_text(size = 16),axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        strip.background =  element_rect( fill="white"))
ggsave(paste(figsloc,"fig_S1-d.png",sep=""),dpi=300)
print("saved fig_S1-d.png")



#only bray, all groups
# ggplot(datadistMelt[datadistMelt$variable=="bray",], aes(x =T12, y = value)) +
#   geom_violin(aes(fill=T12), alpha=.8, linetype=0,show.legend = T)+
#   stat_summary(fun.data=mean_sdl,  geom="pointrange", color="gray31")+
#   #facet_wrap(~ variable,  scales='free',ncol = 4,
#   # labeller = labeller(variable = 
#   #                       c("bray"="Bray-Curtis", "jaccard"="Jaccard",
#   #                         "unifrac"= "Unifrac","wunifrac"= "W-Unifrac")))+   
#   scale_colour_manual(values = "gray")+
#   scale_fill_manual(values = c("#bece7b",
#                                "#d03acf",
#                                "#daad2b",
#                                "#7539ac",
#                                "#82401d",
#                                "#6f9aff",
#                                "#ff898b",
#                                "#0062b2",
#                                "#3e8848",
#                                "#756975"))+
#   labs(y="beta diversity measure", x="treatment")+
#   theme_bw()+
#   theme(text = element_text(size = 16),axis.title.x=element_blank(),
#         axis.text.x=element_blank(), 
#         strip.background =  element_rect( fill="white"))+
#   ylim(0,1.5)



