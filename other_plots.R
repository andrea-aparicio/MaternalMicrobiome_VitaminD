#Figure 1 ----
#baseline vit D distribution fig 1-a ----
ggplot(data=dataM,aes(x=1,y=basevitdng))+
  geom_violin()+
  stat_summary(fun="mean",  geom="pointrange", color="gray31", size=1)+
  geom_jitter(aes(colour=trmt), size=1.5, alpha=0.7)+
  scale_colour_manual(values = c("#de0076",
                                 "#59971c"))+
  theme_minimal()+
  theme(text = element_text(size = 16),
        strip.background = element_rect(fill = "white"),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),panel.grid.major.x = element_blank(),
        legend.position = "none", panel.grid.minor.x = element_blank() )+
  labs(x=" ", y="baseline vitamin D level (ng/mL)", color="treatment")#+
ggsave(paste(figsloc,"fig_1-a.png",sep=""),dpi=300)
print("saved fig_1-a.png")

# scatter plot baseline vs  final fig 1-b----
ggplot(data=dataM, aes(x=basevitdng, y=wk32to38vitdng, color=trmt))+
  geom_ribbon(aes(ymin =basevitdng  - 3, ymax = basevitdng + 3), 
              colour = NA,fill = "#fb9d09", alpha=0.1) +
  geom_point(size=1.5, alpha=.7, show.legend = FALSE)+
  scale_colour_manual(values = c("#de0076",
                                 "#59971c"))+
  theme_minimal()+
  geom_abline(
    mapping = NULL,
    data = NULL,
    slope=1,
    intercept=10,
    na.rm = FALSE,
    show.legend = NA,
    colour="#004e55"
  )+
  geom_abline(
    mapping = NULL,
    data = NULL,
    slope=1,
    intercept=0,
    na.rm = FALSE,
    show.legend = NA,
    colour="#fb9d09"
  )+
  labs(x ="baseline vitamin D level  (ng/mL)" , 
       y = "final vitamin D level  (ng/mL)", color = "treatment", 
       fill="no change")+ 
  theme(text = element_text(size = 16))+
  coord_fixed(ratio=1)
ggsave(paste(figsloc,"fig_1-b.png",sep=""),dpi=300)
print("saved fig_1-b.png")
#vit D diff distribution fig 1-c ----
ggplot(data=dataM,aes(x=1,y=vitDdiff))+
  geom_violin()+
  stat_summary(fun="mean",  geom="pointrange", color="gray31" , size=1 )+
  geom_jitter(aes(colour=trmt), size=1.5, alpha=0.7)+
  scale_colour_manual(values = c("#de0076",
                                 "#59971c"))+
  theme_minimal()+
  theme(text = element_text(size = 16),
        strip.background = element_rect(fill = "white"),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank() , legend.position = "none")+
  labs(x=" ", y="vitamnin D level difference (ng/mL)", color="treatment")+
  geom_abline(
    mapping = NULL,
    data = NULL,
    slope=0,
    intercept=10,
    na.rm = FALSE,
    show.legend = NA,
    colour='gray31', linetype=1, size=1.4  )
ggsave(paste(figsloc,"fig_1-c.png",sep=""),dpi=300)
print("saved fig_1-c.png")

plot4leg <- ggplot(data=dataM,aes(x=1,y=basevitdng))+
  geom_violin()+
  stat_summary(fun.data=mean_sdl,  geom="pointrange", color="gray31"  )+
  geom_jitter(aes(colour=trmt), size=5, alpha=0.7)+
  scale_colour_manual(values = c("#de0076",
                                 "#59971c"), labels=c("treatment", "placebo"))+
  theme_minimal()+
  theme(text = element_text(size = 18),legend.text = element_text(size=16),
        strip.background = element_rect(fill = "white"),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        #legend.direction = "horizontal"
        )+
  labs(x=" ", y="baseline vit D level", color="supplementation")+
  guides(color = guide_legend(title.position="top", title.hjust = 0.5))

leg <- get_legend(plot4leg)
as_ggplot(leg)
ggsave(paste(figsloc,"fig_1-c_legend.png",sep=""),dpi=300)

#box plots education, income and race fig 1-d ----
dataMMelt <- melt(dataM, measure= c("education", "income", "mraceth"), id = "basevitdng")
dataMMelt <- filter(dataMMelt, value != "R")

ggplot(data=dataMMelt, aes(x=value, y=basevitdng))+
  #geom_violin(fill="midnightblue", alpha=.2)+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(color="midnightblue", size=1.2, alpha=0.5)+
  facet_grid(~variable, scales = "free", labeller  = labeller(variable =c("education"="education", "income"="income", "mraceth" = "race")))+
  #ggtitle(stars.pval(wilcox.test(as.formula(paste(yval,xval,sep="~")), data=datat)$p.val))+
  labs( y = "baseline vitamin D (ng/mL)", x=" ")+ 
  #stat_summary(fun.data=mean_sdl,  geom="pointrange", color="midnightblue"  )+
  theme_bw()+
  theme(text = element_text(size = 16),axis.title.x=element_text(size=16), 
        axis.text.x=element_text(size=14),strip.background = element_rect(fill = "white"))
ggsave(paste(figsloc,"fig_1-d.png",sep=""),dpi=300)
print("saved fig_1-d.png")



#Figure 3 ----
dataMt <- data.frame("income"<-dataM$hhinc)
dataMt$education <-  ifelse(dataM$educC == "college or more", "Higher", "Lower")
dataMt$asthma <- dataM$masthma
dataMt$income <- ifelse(dataM$hhincC == "50k or more", "Higher", 
                        ifelse(dataM$hhincC=="less than 50k", "Lower", "R"))
dataMt$race <- dataM$mraceth
dataMt$treatment <- dataM$trmt
dataMt$change <- ifelse(dataM$vitDch == "H", "High", "Low")
dataMt$Sample <- dataM$Sample
dataMt$diff <- dataM$vitDdiff

otuwData <- makeOTUwData("Desulfovibrio", dataMt, "Desu")

#scatter vitamin D difference vs Desulfovibrio relative abundance fig 3.b ----
ggplot(data = otuwData, aes(x=diff, y=Desu, color = treatment)) +
  geom_point(size=2, alpha=.7) +
  scale_colour_manual(values = treatment_colors, labels=c("treatment", "placebo"))+
  theme(text = element_text(size = 16),
        plot.title = element_text(size=16),axis.title.x=element_blank()
  ) +
  geom_vline(mapping = NULL,data = NULL,xintercept=10,na.rm = FALSE, 
             show.legend = NA,color = "gray62",size=1.2)+
  geom_vline(mapping = NULL,data = NULL,xintercept=15,na.rm = FALSE, 
             show.legend = NA,color = "gray62", linetype=2, size=1.4)+
  theme_bw()+
  stat_smooth(
    method = "glm",
    size = 1.5,
    color = "#63b0e4",
    na.rm = TRUE,fill="#63b0e4",alpha=.1
  ) +
  labs(x="final - baseline vitamin D level (ng/mL)", y="Desulfovibrio relative abundance", color= "treatment")+
  theme(text = element_text(size = 16),axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14))+
  ylim(0,0.046)
ggsave(paste(figsloc,"fig_3-b.png",sep=""),dpi=300)
print("saved fig_3-b.png")


#violin Desulfovibrio relative abundance vs participants' characteristics figure 3.c----
variabs <- c( "asthma","race","income","education","treatment", "change")

otuDataMelt <- melt(otuwData, measure= variabs, id = "Desu")
otuDataMelt <- filter(otuDataMelt, value != "R")

ggplot(data=otuDataMelt, aes(x=value, y=Desu))+
  geom_violin(fill="#63b0e4", linetype=0)+
  theme_bw()+
  stat_summary(fun.data=mean_sdl,  geom="pointrange", color="grey31")+
  theme(text = element_text(size = 16),axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14), strip.background =  element_rect( fill="white"))+
  facet_grid(~variable, scales = "free")+
  #geom_jitter(color="#c84a1d", size=1, alpha=0.5)+
  #ggtitle(stars.pval(wilcox.test(as.formula(paste(yval,xval,sep="~")), data=datat)$p.val))+
  labs( y = "Desulfovibrio relative abundance", x=" ")+
  ylim(0,.05)

ggsave(paste(figsloc,"fig_3-c.png",sep=""),dpi=300)
print("saved fig_3-c.png")

#Supplementary figures ----
# bar baseline-change and treatment fig S1.a ----
ggplot(data=dataM, aes(x=ChBase, fill=trmt))+
  geom_bar()+
  scale_fill_manual(values=treatment_colors,
                    labels=c("High"="treatment", "Low"="placebo"))+
  theme_bw()+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        strip.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank() )+
  scale_x_discrete(labels = c("baseH-changeH"="High - High", 
                              "baseH-changeL"="High - Low", 
                              "baseL-changeH"="Low - High", 
                              "baseL-changeL"="Low - Low"))+
  labs( y="count", fill="treatment", x="baseline - change")

ggsave(paste(figsloc,"fig_S1-a.png",sep=""),dpi=300)
print("saved fig_S1-a.png")

# violin relative abundance baseline-change fig S2----
taxid <- "87e72a4d5f3c1f1a08947e156044490a"
var_g <- "ChBase"
otutemp <- as.data.frame(t(otufrg[taxid,]))
otutemp$Sample <- rownames(otutemp)
metaDtemp <- subset(dataM, select = var_g)
metaDtemp$Sample <- dataM$Sample
otutempm <- merge(otutemp, metaDtemp, by="Sample")
otutempm$Sample<-NULL
colnames(otutempm) <- c("relative_abundance", "var")#, "var2")

ggplot(data = otutempm, aes(x=var, y=relative_abundance)) +
        theme_bw()+
        theme(text = element_text(size = 16),axis.title.x=element_text(size=16),
              axis.text.x=element_text(size=14),
              strip.background = element_rect(fill = "white"))+
        geom_violin(aes(fill=var), alpha=.8, linetype=0, show.legend = T)+
        # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
        labs(fill="baseline-change", y="Desulfovibrio relative abundance")+
        #geom_jitter(color="black", size=0.9, alpha=0.9) +
        theme(text = element_text(size = 16),
              plot.title = element_text(size=16),axis.title.x=element_blank(), 
              axis.text.x = element_blank()
        ) +
        scale_fill_discrete(labels=c("baseH-changeH"="High baseline - High change", 
                                     "baseH-changeL"="High baseline - Low change", 
                                     "baseL-changeH"="Low baseline - High change", 
                                     "baseL-changeL"="Low baseline - Low change"))+
        ylim(0,.05)
ggsave(paste(figsloc,"fig_S2.png",sep=""),dpi=300)
print("saved fig_S2.png")


#violin change vs baseline fig S3-a----
ggplot(data=dataM,aes(x=vitDch,y=basevitdng))+
  geom_violin()+
  stat_summary(fun.data=mean_sdl,  geom="pointrange", color="gray31"  )+
  geom_jitter(aes(colour=trmt), size=1.5, alpha=0.7)+
  scale_colour_manual(values = c("#de0076",
                                 "#59971c"))+
  theme_bw()+
  theme(text = element_text(size = 16),
        strip.background = element_rect(fill = "white"),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank() )+
  labs( y="baseline vitamnin D level", color="treatment", x="vitamin D level change")
ggsave(paste(figsloc,"fig_S3-a.png",sep=""),dpi=300)
print("saved fig_S3-a.png")

#bar change and treatment fig S3.b----
ggplot(data=dataM, aes(x=vitDch, fill=trmt))+
  geom_bar()+
  scale_fill_manual(values=treatment_colors,
                    labels=c("High"="treatment", "Low"="placebo"))+
  theme_bw()+
  theme(text = element_text(size = 16),
        strip.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank() )+
  scale_x_discrete(labels = c("H"="High", 
                              "L"="Low"))+
  labs( y="count", fill="supplementation", x="vitamin D level change")
ggsave(paste(figsloc,"fig_S3-b.png",sep=""),dpi=300)
print("saved fig_S3-b.png")


