library(ggplot2)
load("Alpha_diversity.R")

#set up the theme for all the graphs
theme_set(theme_bw()) 
theme_update(axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 12),
             axis.title.x = element_text(size = 14),
             axis.title.y = element_text(size = 14),
             legend.title = element_text(size = 10),
             legend.text = element_text(size = 8),
             legend.spacing = unit(0.25, "cm"),
             #legend.key.xdwidth = unit(0.35, "cm"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             panel.spacing = unit(0, "lines"),
             #      legend.key.size = unit(.15, "cm"),
             legend.key = element_rect(fill = "white"),
             panel.spacing.y = unit(-0.25, "lines"),
             panel.border = element_rect(colour = "black", 
                                         fill = NA, size = 1),
             strip.text.x = element_text(size = 18, colour = "black", 
                                         face = "bold"))

# graph results from richness model with no sources

#make dataframe for prediction lines and confidence intervals
newdat <- data.frame(
  Salinity_Measured = no_source_all$Salinity_Measured,
  richness = predict(Rich_no_source),
  Dispersal = no_source_all$Dispersal,
  Day = no_source_all$Day
)

(rich_g1 <- ggplot(data=no_source_all, aes(Day,richness)) + 
  geom_jitter(aes(color=as.factor(Salinity_Treat), shape=as.factor(Dispersal)),size=5))
rich_g2 <- rich_g1 + facet_wrap(~(as.factor(Salinity_Treat)),ncol=2,nrow = 2)

#richness in source tanks (supplementary Figure # X)

#evenness vs time (Supplementary Figure)

#Shannon diversity no source model

#Shannon diversity just source (Supplementary Figure)