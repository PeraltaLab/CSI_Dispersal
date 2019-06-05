source("microbial_pcoa.R")
source("Graphing_Set_Up.R")

#No source
#need a dataframe with salinity, dispersal, date and points from PCoA
graph_dat1 <- data.frame (cbind(csi.full.ns$Date2, as.character(csi.full.ns$Dispersal),
                                csi.full.ns$Salinity_real, as.factor(csi.full.ns$Salinity)))
names(graph_dat1) <- c("Date", "Dispersal","Salinity", "Salinity_Treat")

graph_dat1$Point1 <- no_source_pcoa$points[,1]
graph_dat1$Point2 <- no_source_pcoa$points[,2]


cent1.g <- group_by(graph_dat1, Date, Dispersal, Salinity_Treat)
cent1.gs <- summarise(cent1.g, Axis1=mean(Point1),sd1 = sd(Point1), 
                      Axis2 = mean(Point2), sd2 = sd(Point2))

cent1.gs <- data.frame(cent1.gs)


#now to graph

g1 <- ggplot(data= cent1.gs, aes(Axis1, Axis2)) +
  geom_point(aes(color=Salinity_Treat, shape= Dispersal), size = 5)

g2 <- g1 + geom_errorbar(aes(ymax= Axis2+sd2, ymin= Axis2-sd2)) + 
  geom_errorbarh(aes(xmax=Axis1+sd1, xmin=Axis1-sd1))

g3 <- g2 + facet_wrap(~Date, ncol = 1) 

(g4 <- g3 +  scale_shape_manual(name = "Dispersal", values = c(16,17),breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")) + 
    scale_color_brewer(name = "Salinity", type = "seq",palette = "Dark2", 
                       labels = c("0", "5", "9","13") ) +
    xlab("PCoA 1 (17.3%)") + ylab("PCoA 2 (7.3%)") + ylim(-0.6,0.6)+ xlim(-0.6,0.6))


## source only pcoa
graph_source <- data.frame(csi.relabun.full2 %>%
  dplyr::select(c(Date2,Salinity)))
names(graph_source) <- c("Date","Salinity")

graph_source$Point1 <- source_pcoa$points[,1]
graph_source$Point2 <- source_pcoa$points[,2]


cent1.ns <- group_by(graph_source, Date, Salinity)
cent1.gns <- summarise(cent1.ns, Axis1=mean(Point1),sd1 = sd(Point1), 
                      Axis2 = mean(Point2), sd2 = sd(Point2))

cent1.gns <- data.frame(cent1.gns)


#now to graph

g1 <- ggplot(data= cent1.gns, aes(Axis1, Axis2)) +
  geom_point(aes(color=as.factor(Salinity), shape = as.factor(Date)), size = 5)

g2 <- g1 + geom_errorbar(aes(ymax= Axis2+sd2, ymin= Axis2-sd2)) + 
  geom_errorbarh(aes(xmax=Axis1+sd1, xmin=Axis1-sd1))

(g3 <- g2 + scale_color_brewer(name = "Salinity", type = "seq",palette = "Dark2", 
                       labels = c("0", "13") ) + labs(shape = "Day") +
    xlab("PCoA 1 (29.3%)") + ylab("PCoA 2 (9.7%)")+ ylim(-0.5,0.5)+ xlim(-0.5,0.5))

