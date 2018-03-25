source("Alpha_diversity.R")
source("Graphing_Set_Up.R")

# graph results from richness model with no sources

#make dataframe for prediction lines 
newdat <- expand.grid(
  richness = 0,
  Dispersal = unique(no_source_all$Dispersal),
  Day = unique(no_source_all$Day),
  Salinity_Measured = seq(0,15,0.05)
)
newdat$richness <- exp(predict(Rich_no_source,newdata = newdat))

rich_g1 <- ggplot(data=no_source_all, aes(Salinity_Measured,richness)) + 
  geom_jitter(aes(color=as.factor(Salinity_Treat), shape=as.factor(Dispersal)),size=3)

rich_g2 <- rich_g1 + geom_line(data = newdat, aes(Salinity_Measured, richness, linetype=as.factor(Dispersal)),size=1) 


(rich_g3 <- rich_g2 + facet_wrap(~(as.factor(Day)),ncol=2,nrow = 3) +scale_color_brewer(type = "seq",palette = "Dark2")+
    ylab("Richness") + xlab("Salinity") +
  scale_shape_manual(name = "Dispersal",values = c(16,17), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))+
    labs(color = "Salinity Treatment") +
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))
  )

#richness in source tanks (supplementary Figure # X)

newdat1 <- expand.grid(
  richness = 0,
  Day = unique(no_source_all$Day),
  Salinity_Measured = seq(0,15,0.05)
)
newdat1$richness <- exp(predict(Rich_source, newdata = newdat1))

rich_source_g1 <- ggplot(data=source_all, aes(Salinity_Measured,richness)) + 
  geom_jitter(aes(color=as.factor(Salinity_Treat)),size=3)

rich_source_g2 <- rich_source_g1 + geom_line(data = newdat1, aes(Salinity_Measured, richness),size=1) 

(rich_source_g3 <- rich_source_g2 + facet_wrap(~(as.factor(Day)))+scale_color_brewer(type = "seq",palette = "Dark2")+
    ylab("Richness") + xlab("Salinity") +labs(color = "Salinity Treatment") )


#evenness vs time (Supplementary Figure XX)
even_g1 <- ggplot(data = alpha, aes(Day,evenness)) + geom_jitter(aes(color = as.factor(Dispersal)), size =3 )

(even_g2 <- even_g1 + facet_wrap(~as.factor(Salinity_Treat)) +scale_color_brewer(type = "seq",palette = "Dark2",
      labels = c("Source", "Control Fresh", "Mixed Salt and Fresh","Salt Only"))+ labs(color="Dispersal")+
    ylab("Evenness") + xlab("Day"))


#Shannon diversity no source model (Figure X)
#make dataframe for prediction lines 
newdat3 <- expand.grid(
  shannon_div = 0,
  Dispersal = unique(no_source_all$Dispersal),
  Day = unique(no_source_all$Day),
  Salinity_Measured = seq(0,15,0.05)
)
newdat3$shannon_div <- predict(Shannon_no_source,newdata = newdat3)

shannon_g1 <- ggplot(data=no_source_all, aes(Salinity_Measured,shannon_div)) + 
  geom_jitter(aes(color=as.factor(Salinity_Treat), shape=as.factor(Dispersal)),size=3)

shannon_g2 <- shannon_g1 + geom_line(data = newdat3, aes(Salinity_Measured, shannon_div, linetype=as.factor(Dispersal)),size=1) 


(shannon_g3 <- shannon_g2 + facet_wrap(~(as.factor(Day)),ncol=2,nrow = 3)+scale_color_brewer(type = "seq",palette = "Dark2")+
    ylab("Shannon Diversity") + xlab("Salinity") +
    scale_shape_manual(name = "Dispersal",values = c(16,17), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))+
    labs(color = "Salinity Treatment") +
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))
)


#Shannon diversity just source (Supplementary Figure)
newdat4 <- expand.grid(
  shannon_div = 0,
  Day = unique(source_all$Day),
  Salinity_Measured = seq(0,15,0.05)
)
newdat4$shannon_div <- predict(Shannon_source,newdata = newdat4)

shannon_source_g1 <- ggplot(data=source_all, aes(Salinity_Measured,shannon_div)) + 
  geom_jitter(aes(color=as.factor(Salinity_Treat)),size=3)

shannon_source_g2 <- shannon_source_g1 + geom_line(data = newdat3, aes(Salinity_Measured, shannon_div),size=1) 


(shannon_g3 <- shannon_source_g2 + facet_wrap(~(as.factor(Day)),ncol=2,nrow = 3)+scale_color_brewer(type = "seq",palette = "Dark2")+
    ylab("Shannon Diversity") + xlab("Salinity") +labs(color = "Salinity Treatment"))
