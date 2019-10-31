source("Microbial_Diversity.R")
source("Graphing_Set_Up.R")

#first graph for only treatment tanks
#richness
#make dataframe for prediction lines 
newdat <- expand.grid(
  richness = 0,
  Dispersal = unique(div$Dispersal),
  Date2 = unique(div$Date2),
  Salinity_real = seq(0,15,1)#,
#  Rep = unique(div$Rep)
)
newdat$richness <-predict(rich_lm,newdata = newdat, type= "response",re.form = NA)

set.seed(101)
new_bb<- bootMer(rich_lm,
                 FUN=function(x)
                   predict(x,re.form=NA,newdata=newdat,
                           type="response"),
                 nsim=400)
boot.CI <- t(apply(new_bb$t,2,quantile,c(0.025,0.975),na.rm=TRUE))

newdat$lower <- boot.CI[,1]
newdat$upper <- boot.CI[,2]


div1 <- div %>% group_by(Date2,Salinity,Dispersal) %>% 
  summarize(mean_rich = mean(richness), sd_rich = sd(richness) )
div1 <- left_join(div1, div)

newdat$mean_rich <- newdat$richness
newdat$Salinity <- newdat$Salinity_real
#write.csv(newdat, file = "bacterial_rich_predict.csv")
newdat <- read.csv("bacterial_rich_predict.csv")

rich_g1 <- ggplot(data=div1, aes(Salinity,mean_rich)) + 
  geom_point(aes(color=as.factor(Salinity), shape=as.factor(Dispersal)),size=3) + 
  geom_errorbar(aes(ymin=mean_rich-sd_rich, ymax = mean_rich+sd_rich))+
  facet_wrap(~(as.factor(Date2)),ncol=2,nrow = 3)

rich_g2 <- rich_g1 + geom_line(data = newdat, aes(Salinity, richness,
                                                  linetype=as.factor(Dispersal)),size=1)+ 
  geom_ribbon(data = newdat, aes(ymin=lower,ymax=upper, 
                                  fill = as.factor(Dispersal)), alpha = 0.5) 


rich_g3 <- rich_g2  + 
  scale_color_brewer(type = "seq",palette = "Dark2")+
    ylab("Observed Microbial Richness") + xlab("Salinity (psu)") +
    scale_shape_manual(name = "Mixing Treatment",values = c(16,17), 
                       breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))+
    labs(color = "Salinity Treatment") +
    scale_linetype_manual(name = "Mixing Treatment Prediction Lines",values = c(1,2), 
                          breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")) +
  scale_fill_manual(name = "Mixing Treatment Prediction Lines",
                    values = c("lightsteelblue4","lightsteelblue1"), 
                    breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))

rich_g3 + theme(legend.position = c(0.75,0.2),
                legend.direction = "vertical", legend.box = "vertical", 
                legend.background = element_blank())

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
    scale_shape_manual(name = "Mixing Treatment",values = c(16,17), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))+
    labs(color = "Salinity Treatment") +
    scale_linetype_manual(name = "Mixing Treatment Prediction Lines",values = c(1,2), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))

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
    scale_shape_manual(name = "Mixing Treatment",values = c(16,17), breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only"))+
    labs(color = "Salinity Treatment") +
    scale_linetype_manual(name = "Mixing Treatment Prediction Lines",values = c(1,2),
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
