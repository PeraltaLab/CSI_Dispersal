source("Beta_Diversity_PCoA_and_PERMANOVA.R")
source("Graphing_Set_Up.R")
library(tidyverse)
##zooplankton
#just source and freshwater control tanks- create dataframe for graphing

graph_dat1 <- data.frame (cbind(js_3dates$Date, as.character(js_3dates$Dispersal),
                                js_3dates$Salinity_Treat))
names(graph_dat1) <- c("Date", "Dispersal","Salinity")

graph_dat1$Point1 <- js_pcoa$points[,1]
graph_dat1$Point2 <- js_pcoa$points[,2]

cent1.g <- group_by(graph_dat1, Date, Dispersal, Salinity)
cent1.gs <- summarise(cent1.g, Axis1=mean(Point1),sd1 = sd(Point1), 
                      Axis2 = mean(Point2), sd2 = sd(Point2))

cent1.gs <- data.frame(cent1.gs)
cent1.gs$Salinity <- factor(as.character(cent1.gs$Salinity),levels= c("0","13"))
cent1.gs$date2 <- rep(c(0,18,45), each =2)

#now to graph

g1 <- ggplot(data= cent1.gs, aes(Axis1, Axis2)) +
  geom_point(aes(color=Salinity, shape=as.factor(date2)), size = 5)

g2 <- g1 + geom_errorbar(aes(ymax= Axis2+sd2, ymin= Axis2-sd2)) + 
  geom_errorbarh(aes(xmax=Axis1+sd1, xmin=Axis1-sd1))+ scale_color_brewer(type = "seq",palette = "Dark2")+
    labs(shape="Day") + ylim(-0.6,0.6)+ xlim(-0.6,0.6)
(g3 <- g2 + xlab("PCoA 1 (45.1%)") + ylab("PCoA 2 (14.3%)"))


#non-source tanks --make data frame

graph_dat1 <- data.frame (cbind(ns_3dates$Date, as.character(ns_3dates$Dispersal),
                                ns_3dates$Salinity_Treat))
names(graph_dat1) <- c("Date", "Dispersal","Salinity")

graph_dat1$Point1 <- ns_pcoa$points[,1]
graph_dat1$Point2 <- ns_pcoa$points[,2]


cent1.g <- group_by(graph_dat1, Date, Dispersal, Salinity)
cent1.gs <- summarise(cent1.g, Axis1=mean(Point1),sd1 = sd(Point1), 
                      Axis2 = mean(Point2), sd2 = sd(Point2))

cent1.gs <- data.frame(cent1.gs)
cent1.gs$Salinity <- factor(as.character(cent1.gs$Salinity),levels= c("0","5","9","13"))
cent1.gs$date2 <- rep(c(0,18,45), each =4)

#now to graph

g1 <- ggplot(data= cent1.gs, aes(Axis1, Axis2)) +
  geom_point(aes(color=Salinity, shape= Dispersal), size = 5)

g2 <- g1 + geom_errorbar(aes(ymax= Axis2+sd2, ymin= Axis2-sd2)) + 
  geom_errorbarh(aes(xmax=Axis1+sd1, xmin=Axis1-sd1))

g3 <- g2 + facet_wrap(~date2, ncol = 1) 

(g4 <- g3 +  scale_shape_manual(name = "Dispersal", values = c(16,17),breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")) + 
    scale_color_brewer(type = "seq",palette = "Dark2" ) +
    xlab("PCoA 1 (34.3%)") + ylab("PCoA 2 (16.8%)") + ylim(-0.7,0.7)+xlim(-0.7,0.7))



### plot source and non-source togther for supplement
graph_dat3 <- data.frame (cbind(all_3dates$Date, as.character(all_3dates$Dispersal),
                                all_3dates$Salinity_Treat))
names(graph_dat3) <- c("Date", "Dispersal","Salinity")

graph_dat3$Point1 <- all_pcoa$points[,1]
graph_dat3$Point2 <- all_pcoa$points[,2]

cent1.g <- group_by(graph_dat3, Date, Dispersal, Salinity)
cent1.gs <- summarise(cent1.g, Axis1=mean(Point1),sd1 = sd(Point1), 
                      Axis2 = mean(Point2), sd2 = sd(Point2))

cent1.gs <- data.frame(cent1.gs)
cent1.gs$Salinity <- factor(as.character(cent1.gs$Salinity),levels= c("0","5", "9", "13"))
cent1.gs$date2 <- rep(c(0,18,45), each = 11)

#now to graph

g1 <- ggplot(data= cent1.gs, aes(Axis1, Axis2)) +
  geom_point(aes(color=Salinity, shape=as.factor(Dispersal)), size = 5)

g2 <- g1 + geom_errorbar(aes(ymax= Axis2+sd2, ymin= Axis2-sd2)) + 
  geom_errorbarh(aes(xmax=Axis1+sd1, xmin=Axis1-sd1))+ scale_color_brewer(type = "seq",palette = "Dark2")+
  labs(shape="Dispersal") + ylim(-0.6,0.6)+ xlim(-0.6,0.6)+
  scale_shape_manual(values=c(17, 15, 18,19), labels = c("Source","Control","Mixed Salt and Fresh","Salt Only"))
g3 <- g2 + facet_wrap(~date2, ncol = 1) 

(g4 <- g3 + xlab("PCoA 1 (36.1%)") + ylab("PCoA 2 (14.5%)"))



