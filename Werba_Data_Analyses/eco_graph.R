source("Ecosystem_Function.R")
source("Graphing_Set_Up.R")







### Carbon min graph
newdat <- expand.grid(
  Cmin = 0,
  Dispersal = unique(dat_gather_decomp3$Dispersal),
  #  Salinity_Measured = seq(0,15,0.05),
  z_rich = mean(dat_gather_decomp3$z_rich),
  m_rich = seq(100,1000,1),
  Salinity_Measured = unique(dat_gather_decomp3$Salinity)
)
newdat$Cmin <- exp(predict(cmin_lm,newdata = newdat))-1
se <- data.frame(se = predict(cmin_lm,newdata = newdat, se.fit = TRUE))
newdat$se <- exp(se$se.se.fit)-1
newdat$upper <- with(newdat, Cmin + se)
newdat$lower <- with(newdat, Cmin - se)





g1 <- ggplot(data = dat_gather_decomp3, aes(m_rich,Cmin)) + 
  geom_point(aes(color = as.factor(Salinity)), size = 3)

g2 <- g1 + geom_ribbon(data = newdat,aes(ymin=lower, ymax=upper, 
                                         fill=as.factor(Dispersal)),alpha=0.1) +
  geom_line(data = newdat, 
            aes(m_rich, Cmin,color = as.factor(Salinity_Measured)),size=1)

g3 <- g2 + facet_grid(Dispersal~. )

(g4 <- g3 + xlab("Microbial Richness") + ylab("Carbon Mineralization") +
    scale_color_brewer(name = "Salinity Treatment", type = "seq", palette = "Dark2")+
    scale_fill_manual(name = "Salinity Treatment",
                      #type = "seq", 
                      palette = "Dark2"))
    

