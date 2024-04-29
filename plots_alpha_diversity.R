
#baseline plot ----

dataRichMelt <- melt(dataRich, measure= c("Observed", "Shannon", "Simpson"), id = "basevitdng")
dataRichMelt$variable2 <- as.character(dataRichMelt$variable)
dataRichMelt$variable2[dataRichMelt$variable2=="Observed"]  <- "Richness"

ggplot(dataRichMelt, aes(x = basevitdng, y = value)) +
  theme_bw()+
  theme(text = element_text(size = 16),axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14),strip.background = element_rect(fill = "white"))+
  geom_point(color= "midnightblue", size=1,alpha=0.5)+
  facet_wrap(~ variable2,  scales='free')+
  labs(x="baseline vitamin D level (ng/mL)", y="alpha diversity index")+
  theme(text = element_text(size = 16),axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14))+
  stat_smooth(
    method = "glm",
    size = .5,
    color = "red",
    na.rm = TRUE,fill="green",alpha=.2, level=.95
  )

ggsave(paste(figsloc,"fig_2-a.png",sep=""),dpi=300)
print("saved fig_2-a.png")
#treatment plot ----
dataRichMelt <- melt(dataRich, measure= c("Observed", "Shannon", "Simpson"), id = "trmt")
dataRichMelt$variable2 <- as.character(dataRichMelt$variable)
dataRichMelt$variable2[dataRichMelt$variable2=="Observed"]  <- "Richness"

ggplot(dataRichMelt, aes(x = trmt, y = value)) +
  theme_bw()+
  theme(text = element_text(size = 16),axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14),strip.background = element_rect(fill = "white"))+
  geom_violin(aes(fill=trmt), alpha=.8, linetype=0, show.legend = F)+
  stat_summary(fun.data=mean_sdl,  geom="pointrange", color="gray31")+
  facet_wrap(~ variable2,  scales='free')+
  labs(x="treatment", y="alpha diversity index", fill="treatment")+
  theme(text = element_text(size = 16),axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14))+
  stat_smooth(
    method = "glm",
    size = .5,
    color = "red",
    na.rm = TRUE,fill="green",alpha=.2, level=.95
  )+
  scale_fill_manual(values = treatment_colors, labels=c("High", "Low"))
ggsave(paste(figsloc,"fig_2-c.png",sep=""),dpi=300)
print("saved fig_2-c.png")

# base-change plot ----

dataRichMelt <- melt(dataRich, measure= c("Observed", "Shannon", "Simpson"), id = "ChBase")
dataRichMelt$variable2 <- as.character(dataRichMelt$variable)
dataRichMelt$variable2[dataRichMelt$variable2=="Observed"]  <- "Richness"

ggplot(dataRichMelt, aes(x = ChBase, y = value)) +
  theme_bw()+
  theme(text = element_text(size = 16),axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14),
        strip.background = element_rect(fill = "white"))+
  geom_violin(aes(fill=ChBase), alpha=.8, linetype=0, show.legend = T)+
  stat_summary(fun.data=mean_sdl,  geom="pointrange", color="gray31")+
  facet_wrap(~ variable2,  scales='free')+
  labs(x="treatment", y="alpha diversity index", fill="baseline - change")+
  theme(text = element_text(size = 16),axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  scale_fill_discrete(labels=c("baseH-changeH"="High - High",
                             "baseH-changeL"="High - Low",
                             "baseL-changeH"="Low - High",
                             "baseL-changeL"="Low - Low"))
ggsave(paste(figsloc,"fig_S1-b.png",sep=""),dpi=300)
print("saved fig_S1-b.png")
