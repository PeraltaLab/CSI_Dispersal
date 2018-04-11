source("Ecosystem_Function.R")
source("Graphing_Set_Up.R")

newdat <- expand.grid(
  weight_change = 0,
  Dispersal = unique(dat_gather_decomp_ns$Dispersal),
#  Salinity_Measured = seq(0,15,0.05),
  z_rich = mean(dat_gather_decomp_ns$z_rich),
  m_rich = seq(300,990,5),
  Leaf_Type = unique(dat_gather_decomp_ns$Leaf_Type),
  Salinity_Measured = unique(dat_gather_decomp_ns$Salinity)
)
newdat$weight_change <- exp(predict(rich_decomp,newdata = newdat))


dat_gather_decomp_ns$Leaf_Type <- as.factor(dat_gather_decomp_ns$Leaf_Type)
levels(dat_gather_decomp_ns$Leaf_Type)

levels(dat_gather_decomp_ns$Leaf_Type) <- c("Maple", "Phragmites", "Spartina")
levels(newdat$Leaf_Type) <- c("Phragmites", "Maple", "Spartina")
g1 <- ggplot(data = dat_gather_decomp_ns, aes(m_rich,weight_change)) + 
  geom_point(aes(color = as.factor(Salinity), shape = Leaf_Type), size = 3)

g2 <- g1 + geom_line(data = newdat, aes(m_rich, weight_change,
          linetype=as.factor(Dispersal), color = as.factor(Salinity_Measured)),size=1)

g3 <- g2 + facet_grid(Leaf_Type~. )

(g4 <- g3 + xlab("Microbial Richness") + ylab("Dry Weight Change (g)") +
    scale_color_brewer(name = "Salinity Treatment", type = "seq", palette = "Dark2")+
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2),
                          breaks = c(2,3),
                          labels = c("Mixed Salt and Fresh","Salt Only")))
