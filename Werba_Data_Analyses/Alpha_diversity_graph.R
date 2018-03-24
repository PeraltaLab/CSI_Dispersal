library(ggplot2)
source("Alpha_diversity.R")

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

#make dataframe for prediction lines 
newdat <- data.frame(
  richness = 0,
  Dispersal = (no_source_all$Dispersal),
  Day = (no_source_all$Day),
  Salinity_Treat = (no_source_all$Salinity_Treat)
)
newdat$richness <- exp(predict(Rich_no_source))

rich_g1 <- ggplot(data=no_source_all, aes(Salinity_Treat,richness)) + 
  geom_jitter(aes(color=as.factor(Salinity_Treat), shape=as.factor(Dispersal)),size=3)

rich_g2 <- rich_g1 + geom_line(data = newdat, aes(Salinity_Treat, richness, color=as.factor(Dispersal)),size=2) 


(rich_g3 <- rich_g2 + facet_wrap(~(as.factor(Day)),ncol=2,nrow = 3))

#richness in source tanks (supplementary Figure # X)

newdat1 <- data.frame(
  richness = 0,
  Day = (source_all$Day),
  Salinity_Treat = (source_all$Salinity_Treat)
)
newdat1$richness <- exp(predict(Rich_source))

rich_source_g1 <- ggplot(data=source_all, aes(Salinity_Treat,richness)) + 
  geom_jitter(aes(color=as.factor(Salinity_Treat)),size=3)

rich_source_g2 <- rich_source_g1 + geom_line(data = newdat1, aes(Salinity_Treat, richness),size=2) 

(rich_source_g3 <- rich_source_g2 + facet_wrap(~(as.factor(Day))))


#evenness vs time (Supplementary Figure XX)
even_g1 <- ggplot(data = alpha, aes(Day,evenness)) + geom_jitter(aes(color = as.factor(Dispersal)), size =3 )

(even_g2 <- even_g1 + facet_wrap(~as.factor(Salinity_Treat)))

#Shannon diversity no source model (Figure X)
#make dataframe for prediction lines 
newdat3 <- data.frame(
  shannon_div = 0,
  Dispersal = (no_source_all$Dispersal),
  Day = (no_source_all$Day),
  Salinity_Treat = (no_source_all$Salinity_Treat)
)
newdat3$shannon_div <- predict(Shannon_no_source)

shannon_g1 <- ggplot(data=no_source_all, aes(Salinity_Treat,shannon_div)) + 
  geom_jitter(aes(color=as.factor(Salinity_Treat), shape=as.factor(Dispersal)),size=3)

shannon_g2 <- shannon_g1 + geom_line(data = newdat3, aes(Salinity_Treat, shannon_div, color=as.factor(Dispersal)),size=2) 


(shannon_g3 <- shannon_g2 + facet_wrap(~(as.factor(Day)),ncol=2,nrow = 3))


#Shannon diversity just source (Supplementary Figure)
newdat4 <- data.frame(
  shannon_div = 0,
  Day = (source_all$Day),
  Salinity_Treat = (source_all$Salinity_Treat)
)
newdat4$shannon_div <- predict(Shannon_source)

shannon_source_g1 <- ggplot(data=source_all, aes(Salinity_Treat,shannon_div)) + 
  geom_jitter(aes(color=as.factor(Salinity_Treat)),size=3)

shannon_source_g2 <- shannon_source_g1 + geom_line(data = newdat3, aes(Salinity_Treat, shannon_div),size=2) 


(shannon_g3 <- shannon_source_g2 + facet_wrap(~(as.factor(Day)),ncol=2,nrow = 3))
