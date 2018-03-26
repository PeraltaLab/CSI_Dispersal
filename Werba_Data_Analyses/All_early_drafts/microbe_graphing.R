library(ggplot2)
library(vegan)
library(tidyverse)
load(CSI_StrFxn_MS.Rmd) 

theme_set(theme_bw()) 
theme_update(axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 12),
             axis.title.x = element_text(size = 14),
             axis.title.y = element_text(size = 14),
             legend.title = element_text(size = 10),
             legend.text = element_text(size = 8),
             legend.spacing = unit(0.25, "cm"),
             legend.key.width = unit(0.35, "cm"),
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

str(csi.relanabun.full)
#just community data
micro_comm <- csi.relabun.full[,-c(1:8)]
micro_dist <- vegdist(micro_comm,method = "bray")
micro_pcoa <- cmdscale(micro_dist,k=3, eig=TRUE, add = FALSE)

#need data frame for graphing
g_dat <- data.frame(date = csi.relabun.full$Date2,
                    salinity = csi.relabun.full$Salinity,
                    dispersal =csi.relabun.full$Dispersal,
                    points1 = micro_pcoa$points[,1],
                    points2 = micro_pcoa$points[,2])
#need to aggregate centroids and sd
#need centroids and sd by group 
cent1.g <- group_by(g_dat, date, dispersal, salinity)
cent1.gs <- summarise(cent1.g, Axis1=mean(points1),sd1 = sd(points1), 
                      Axis2 = mean(points2), sd2 = sd(points2))

cent1.gs <- data.frame(cent1.gs)
#cent1.gs$Salinity <- factor(as.character(cent1.gs$Salinity),levels= c("0","5","9","13"))

#graphing
g <- ggplot(cent1.gs, aes(Axis1,Axis2)) + geom_point(aes(color=as.factor(salinity), shape=dispersal), size = 5)
g1 <- g + geom_errorbar(aes(ymax= Axis2+sd2, ymin= Axis2-sd2)) + 
  geom_errorbarh(aes(xmax=Axis1+sd1, xmin=Axis1-sd1))
(g2 <- g1 + facet_wrap(~date, ncol = 1) )
