source("alpha_family.R")
source("Graphing_Set_Up.R")

# graph results from richness model with no sources

#make dataframe for prediction lines 
newdat <- expand.grid(
  richness = 0,
  Dispersal = unique(no_source_all$Dispersal),
  Day = unique(no_source_all$Day),
  Salinity_Measured = seq(0,15,0.05)
 
)
newdat$richness <- predict(Rich_no_source,newdata = newdat,type=("response"),re.form = NA)

set.seed(101)
new_bb<- bootMer(Rich_no_source,
                FUN=function(x)
                  predict(x,re.form=NA,newdata=newdat,
                          type="response"),
                nsim=400)
 boot.CI <- t(apply(new_bb$t,2,quantile,c(0.025,0.975),na.rm=TRUE))

newdat$lower <- boot.CI[,1]
newdat$upper <- boot.CI[,2]

#no_source_all <- no_source_all1

#no_source_all <- no_source_all[no_source_all$Salinity_Treat == "5" , ]

#write.csv(newdat, file = "zoo_rich_predict.csv")
newdat <-  read.csv("zoo_rich_predict.csv")

nn <- no_source_all %>% group_by(Day,Salinity_Treat,Dispersal) %>% 
  summarize(mean_rich = mean(richness), sd_rich = sd(richness) )
nn <- left_join(nn, no_source_all)

newdat$mean_rich <- newdat$richness
newdat$Salinity_Treat <- newdat$Salinity_Measured
nn$day_lab <- NA
newdat$day_lab <- NA

for (i in 1:nrow(nn)){
if (nn$Day[i] == 0) {
    nn$day_lab[i] <- "a. 0"
  } else if 
  (nn$Day[i]==9) {
    nn$day_lab[i] <- "b. 9"
  } else if 
  (nn$Day[i]==18) {
    nn$day_lab[i] <- "c. 18"
  } else if 
  (nn$Day[i] == 27){
    nn$day_lab[i] <- "d. 27"
  } else if
  (nn$Day[i] == 36) {
    nn$day_lab[i] <- "e. 36"
    } else if
  (nn$Day[i] == 45) {
    nn$day_lab[i] <- "f. 45"
  }
}


for (i in 1:nrow(newdat)){
  if (newdat$Day[i] == 0) {
    newdat$day_lab[i] <- "a. 0"
  } else if 
  (newdat$Day[i]==9) {
    newdat$day_lab[i] <- "b. 9"
  } else if 
  (newdat$Day[i]==18) {
    newdat$day_lab[i] <- "c. 18"
  } else if 
  (newdat$Day[i] == 27){
    newdat$day_lab[i] <- "d. 27"
  } else if
  (newdat$Day[i] == 36) {
    newdat$day_lab[i] <- "e. 36"
  } else if
  (newdat$Day[i] == 45) {
    newdat$day_lab[i] <- "f. 45"
  }
}



labels <- c("0" = "a. 0", "9" = "b. 9", "18" = "c. 18",
            "27" = "d. 27", "36" = "e. 36", "45" = "f. 45")


rich_g1 <- ggplot(data=nn, aes(Salinity_Treat,mean_rich))  +
geom_point(aes(color=as.factor(Salinity_Treat), shape=as.factor(Dispersal)),size=3) + facet_wrap(~day_lab) + 
  geom_errorbar(aes(ymin=mean_rich-sd_rich, ymax= mean_rich+sd_rich, width = 0.2))

rich_g2 <- rich_g1 + geom_line(data = newdat, aes(Salinity_Measured, mean_rich, 
                                                  linetype=as.factor(Dispersal)),size=1) +
  geom_ribbon( data = newdat, aes(ymin=lower,ymax=upper, 
                                  fill = as.factor(Dispersal)), alpha = 0.5) 

(rich_g3 <- rich_g2 + 
    facet_wrap(~day_lab,ncol=2,nrow = 3) +
    scale_color_brewer(type = "seq",palette = "Dark2")+
    ylab("Zooplankton Order Count") + xlab("Salinity (psu)") +
  scale_shape_manual(name = "Mixing Treatment",values = c(16,17), 
                     breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))+
    labs(color = "Salinity Treatment") +
    scale_linetype_manual(name = "Mixing Treatment Prediction Lines",values = c(1,2), 
                          breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")) +
  scale_fill_manual(name = "Mixing Treatment Prediction Lines",
                    values = c("lightsteelblue4","lightsteelblue1"), 
                    breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")))



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

rich_source_g3 <- rich_source_g2 + facet_wrap(~(as.factor(Day)))+scale_color_brewer(type = "seq",palette = "Dark2")+
    ylab("Richness") + xlab("Salinity (psu)") +labs(color = "Salinity Treatment") 

rich_source_g3 + theme(legend.position = c(0.5,0.9), legend.background = element_rect(fill = "gray90"))

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
    scale_shape_manual(name = "Mixing Treatment",values = c(16,17), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))+
    labs(color = "Salinity Treatment") +
    scale_linetype_manual(name = "Mixing Treatment Prediction Lines",values = c(1,2), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))
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

shannon_source_g2 <- shannon_source_g1 + geom_line(data = newdat4, aes(Salinity_Measured, shannon_div),size=1) 


shannon_source_g3 <- shannon_source_g2 + facet_wrap(~(as.factor(Day)),ncol=2,nrow = 3)+scale_color_brewer(type = "seq",palette = "Dark2")+
    ylab("Shannon Diversity (H')") + xlab("Salinity") +labs(color = "Salinity Treatment")

shannon_source_g3 + theme(legend.position = c(0.25,0.76),legend.direction = "horizontal", 
                          legend.background = element_rect(fill = "gray90"))
