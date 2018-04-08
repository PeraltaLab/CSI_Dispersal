source("Microbial_Diversity.R")
source("Graphing_Set_Up.R")

#first graph for only treatment tanks
#richness
#make dataframe for prediction lines 
newdat <- expand.grid(
  richness = 0,
  Dispersal = unique(div$Dispersal),
  Date2 = unique(div$Date2),
  Salinity_real = seq(0,15,0.05)
)
newdat$richness <- exp(predict(rich_lm,newdata = newdat))

rich_g1 <- ggplot(data=div, aes(Salinity_real,richness)) + 
  geom_jitter(aes(color=as.factor(Salinity), shape=as.factor(Dispersal)),size=3)

rich_g2 <- rich_g1 + geom_line(data = newdat, aes(Salinity_real, richness, linetype=as.factor(Dispersal)),size=1) 


rich_g3 <- rich_g2 + facet_wrap(~(as.factor(Date2)),ncol=2,nrow = 3) +scale_color_brewer(type = "seq",palette = "Dark2")+
    ylab("Richness") + xlab("Salinity") +
    scale_shape_manual(name = "Dispersal",values = c(16,17), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))+
    labs(color = "Salinity Treatment") +
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))

rich_g3 + theme(legend.position = c(0.75,0.2),
                legend.direction = "vertical", legend.box = "vertical", legend.background = element_blank())

#shannon
newdat <- expand.grid(
  shannon = 0,
  Dispersal = unique(div$Dispersal),
  Date2 = unique(div$Date2),
  Salinity_real = seq(0,15,0.05)
)
newdat$shannon <- predict(shan_lm,newdata = newdat)

shan_g1 <- ggplot(data=div, aes(Salinity_real,shannon)) + 
  geom_jitter(aes(color=as.factor(Salinity), shape=as.factor(Dispersal)),size=3)

shan_g2 <- shan_g1 + geom_line(data = newdat, aes(Salinity_real, shannon, linetype=as.factor(Dispersal)),size=1) 


shan_g3 <- shan_g2 + facet_wrap(~(as.factor(Date2)),ncol=2,nrow = 3) +scale_color_brewer(type = "seq",palette = "Dark2")+
    ylab("Shannon Diversity (H')") + xlab("Salinity") +
    scale_shape_manual(name = "Dispersal",values = c(16,17), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))+
    labs(color = "Salinity Treatment") +
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))

shan_g3 + theme(legend.position = c(0.75,0.2), 
                legend.direction = "vertical", legend.box = "vertical", legend.background = element_blank())

#Evenness
newdat <- expand.grid(
  J = 0,
  Dispersal = unique(div$Dispersal),
  Date2 = unique(div$Date2),
  Salinity_real = seq(0,15,0.05)
)
newdat$J <- predict(even_lm, type= "response", newdata = newdat)

even_g1 <- ggplot(data=div, aes(Salinity_real,J)) + 
  geom_jitter(aes(color=as.factor(Salinity), shape=as.factor(Dispersal)),size=3)

even_g2 <- even_g1 + geom_line(data = newdat, aes(Salinity_real, J, linetype=as.factor(Dispersal)),size=1) 


even_g3 <- even_g2 + facet_wrap(~(as.factor(Date2)),ncol=2,nrow = 3) +scale_color_brewer(type = "seq",palette = "Dark2")+
    ylab("Pielou's Evenness") + xlab("Salinity") +
    scale_shape_manual(name = "Dispersal",values = c(16,17), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))+
    labs(color = "Salinity Treatment") +
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2),
                          breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))

(even_g3 + theme(legend.position = c(0.75,0.2), legend.direction = "vertical", 
                 legend.box = "vertical", legend.background = element_blank()))


## graphs for source tanks only

#richness
#make dataframe for prediction lines 
newdat <- expand.grid(
  richness.source = 0,
  Date2 = unique(s_div$Date2),
  Salinity_Measured = seq(0,15,0.0005)
)
newdat$richness.source <- exp(predict(rich_lm_s,newdata = newdat))

rich_g1_s <- ggplot(data=s_div, aes(Salinity_Measured,richness.source)) + 
  geom_jitter(aes(color=as.factor(Salinity)),size=3)

rich_g2_s <- rich_g1_s + geom_line(data = newdat, aes(Salinity_Measured, richness.source),size=1) 


rich_g3_s <- rich_g2_s + facet_wrap(~(as.factor(Date2)),ncol=2,nrow = 3) +scale_color_brewer(type = "seq",palette = "Dark2")+
  ylab("Richness") + xlab("Salinity") +
  labs(color = "Salinity Treatment") 

rich_g3_s + theme(legend.position = c(0.75,0.2),
                legend.direction = "vertical", legend.box = "vertical", legend.background = element_blank())

#shannon
newdat <- expand.grid(
  shannon.source = 0,
  Date2 = unique(s_div$Date2),
  Salinity_Measured = seq(0,15,0.0005)
)
newdat$shannon.source <- predict(shan_lm_s,newdata = newdat)

shan_g1_s <- ggplot(data=s_div, aes(Salinity_Measured,shannon.source)) + 
  geom_jitter(aes(color=as.factor(Salinity)),size=3)

shan_g2_s <- shan_g1_s + geom_line(data = newdat, aes(Salinity_Measured, shannon.source),size=1) 


shan_g3_s <- shan_g2_s + facet_wrap(~(as.factor(Date2)),ncol=2,nrow = 3) +scale_color_brewer(type = "seq",palette = "Dark2")+
  ylab("Shannon Diveristy Index (H')") + xlab("Salinity") +
  labs(color = "Salinity Treatment") 

shan_g3_s + theme(legend.position = c(0.75,0.2),
                  legend.direction = "vertical", legend.box = "vertical", legend.background = element_blank())


#Evenness
newdat <- expand.grid(
  J.source = 0,
  Date2 = unique(s_div$Date2),
  Salinity_Measured = seq(0,15,0.0005)
)
newdat$J.source <- predict(even_lm_s,type = "response", newdata = newdat)

even_g1_s <- ggplot(data=s_div, aes(Salinity_Measured,J.source)) + 
  geom_jitter(aes(color=as.factor(Salinity)),size=3)

even_g2_s <- even_g1_s + geom_line(data = newdat, aes(Salinity_Measured, J.source),size=1) 


even_g3_s <- even_g2_s + facet_wrap(~(as.factor(Date2)),ncol=2,nrow = 3) +scale_color_brewer(type = "seq",palette = "Dark2")+
  ylab("Peilou's Evennens (J')") + xlab("Salinity") +
  labs(color = "Salinity Treatment") 

even_g3_s + theme(legend.position = c(0.75,0.2),
                  legend.direction = "vertical", legend.box = "vertical", legend.background = element_blank())
