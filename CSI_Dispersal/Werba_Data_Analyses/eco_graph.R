source("Ecosystem_Function.R")
source("Graphing_Set_Up.R")

newdat <- expand.grid(
  weight_change = 0,
  Dispersal = unique(dat_gather_decomp1$Dispersal),
#  Salinity_Measured = seq(0,15,0.05),
  z_rich = mean(dat_gather_decomp1$z_rich),
  m_rich = seq(100,1000,1),
  Leaf_Type = unique(dat_gather_decomp1$Leaf_Type),
  Salinity_Measured = unique(dat_gather_decomp1$Salinity)
)
newdat$weight_change <- exp(predict(rich_decomp,newdata = newdat))


dat_gather_decomp1$Leaf_Type <- as.factor(dat_gather_decomp1$Leaf_Type)
levels(dat_gather_decomp1$Leaf_Type)

levels(dat_gather_decomp1$Leaf_Type) <- c("Maple", "Phragmites", "Spartina")
levels(newdat$Leaf_Type) <- c("Phragmites", "Maple", "Spartina")
g1 <- ggplot(data = dat_gather_decomp1, aes(m_rich,weight_change)) + 
  geom_point(aes(color = as.factor(Salinity), shape = Leaf_Type), size = 3)

g2 <- g1 + geom_line(data = newdat, aes(m_rich, weight_change,
          linetype=as.factor(Dispersal), color = as.factor(Salinity_Measured)),size=1)

g3 <- g2 + facet_grid(Leaf_Type~. )

(g4 <- g3 + xlab("Microbial Richness") + ylab("Proportion Leaf Remaining") +
    scale_color_brewer(name = "Salinity Treatment", type = "seq", palette = "Dark2")+
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2,3),
                          breaks = c(0,2,3),
                          labels = c("Source","Mixed Salt and Fresh","Salt Only")))





### Carbon min graph
newdat <- expand.grid(
  Cmin = 0,
  Dispersal = unique(dat_gather_decomp$Dispersal),
  #  Salinity_Measured = seq(0,15,0.05),
  z_rich = mean(dat_gather_decomp$z_rich),
  m_rich = seq(100,1000,1),
  Salinity_Measured = unique(dat_gather_decomp$Salinity)
)
newdat$Cmin <- exp(predict(cmin_lm,newdata = newdat))-1

dat_gather_decomp$Dispersal <- as.factor(dat_gather_decomp$Dispersal)
levels(dat_gather_decomp$Dispersal) <- c("No Dispersal", "Mixed Salt and Fresh", "Salt Only")

newdat$Dispersal <- as.factor(newdat$Dispersal)
levels(newdat$Dispersal) <- c("No Dispersal", "Mixed Salt and Fresh", "Salt Only")


g1 <- ggplot(data = dat_gather_decomp, aes(m_rich,Cmin)) + 
  geom_point(aes(color = as.factor(Salinity)), size = 3)

g2 <- g1 + geom_line(data = newdat, aes(m_rich, Cmin,color = as.factor(Salinity_Measured)),size=1)
g3 <- g2 + facet_grid(Dispersal~. )

(g4 <- g3 + xlab("Microbial Richness") + ylab("Carbon Mineralization") +
    scale_color_brewer(name = "Salinity Treatment", type = "seq", palette = "Dark2")+
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2,3),
                          breaks = c(0,2,3),
                          labels = c("Source","Mixed Salt and Fresh","Salt Only")))

