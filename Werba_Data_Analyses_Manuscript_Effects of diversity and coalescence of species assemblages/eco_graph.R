source("Ecosystem_Function.R")  
source("Graphing_Set_Up.R")



### Carbon min graph
newdat <- expand.grid(
  Cmin = 0,
  Dispersal = unique(dat_gather_decomp3$Dispersal),
  #  Salinity_Measured = seq(0,15,0.05),
  z_rich = mean(dat_gather_decomp3$z_rich),
  m_rich = seq(100,1000,1),
  Salinity_Measured = 6
)
newdat$Cmin <- exp(predict(cmin_lm,newdata = newdat))


#se <- data.frame(predict(cmin_lm,newdata = newdat, se.fit = TRUE))
#newdat$upper <- exp((se$fit+se$se.fit*1.96))
#newdat$lower <- exp((se$fit-se$se.fit*1.96))

#newdat$Dispersal_names <- "0"
#newdat$Dispersal_names <- ifelse(newdat$Dispersal == 2, "Mixed Salt and Fresh", "Salt Only")

#########################
### ggplot starts here
#########################
#change the name of median salinity to 5 for legend purposes
newdat$Salinity_Measured <- 5


dispersal_names <- c("2" ="Mixed Salt and Fresh", "3" = "Salt Only")


g1 <- ggplot(data = dat_gather_decomp3, aes(m_rich,Cmin)) + 
  geom_point(aes(color = as.factor(Salinity), shape = as.factor(Dispersal)), size = 3)+
  scale_x_continuous(limits = c(0,725))

g2 <- g1 +  #geom_ribbon(data = newdat,aes(ymin=lower, ymax=upper, 
                                       # fill=as.factor(Dispersal)),alpha=0.1) +
  geom_line(data = newdat, 
            aes(m_rich, Cmin, linetype = as.factor(Dispersal)),size=1)

#g3 <- g2+ facet_grid(as.factor(Dispersal) ~ ., labeller = as_labeller(dispersal_names))

(g4 <- g2 + xlab("Observed Microbial Richness") + ylab("Carbon Mineralization") +
    scale_color_brewer(name = "Salinity Treatment", type = "seq", palette = "Dark2")+
    scale_fill_brewer(name = "Mixing Treatment", 
                      type = "seq",
                      palette = 'Dark2')+ 
    scale_linetype_manual(name = "Mixing Treatment",values = c(1,2),
    breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")) +
    scale_shape_manual(name = "Mixing Treatment", values = c(16,17), 
                       breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")   ))
  

## make second graph for exploratory analysis
newdat1 <- expand.grid(
  Cmin = 0,
  Dispersal = unique(dat_gather_decomp3$Dispersal),
  Salinity_Measured = seq(0,15,0.05),
  z_rich = mean(dat_gather_decomp3$z_rich),
  m_rich = mean(dat_gather_decomp3$m_rich)
 
)


newdat1$Cmin<- exp(predict(cmin_2,newdata = newdat1))

gg <- ggplot(data = dat_gather_decomp3, aes(Salinity_Measured ,Cmin)) + 
geom_point(aes(color = as.factor(Salinity), shape = as.factor(Dispersal)), size = 3) +
  geom_line(data = newdat1, aes(Salinity_Measured, Cmin, linetype= as.factor(Dispersal)))

(gh <- gg+ ylim(0,250)+ xlab("Salinity (psu)") + ylab("Carbon Mineralization")+
    scale_color_brewer(name = "Salinity Treatment", type = "seq", palette = "Dark2")+
    scale_fill_brewer(name = "Mixing Treatment", 
                      type = "seq",
                      palette = 'Dark2')+ 
    scale_linetype_manual(name = "Mixing Treatment",values = c(1,2),
                          breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")) +
    scale_shape_manual(name = "Mixing Treatment", values = c(16,17), 
                       breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")   ))

