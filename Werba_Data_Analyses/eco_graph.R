source("Ecosystem_Function.R")
source("Graphing_Set_Up.R")

newdat <- expand.grid(
  weight_change = 0,
  Dispersal = unique(dat_gather_decomp_ns$Dispersal),
  Salinity_Measured = seq(0,15,0.05),
  z_rich = mean(dat_gather_decomp_ns$z_rich),
  m_rich = seq(300,990,5),
  Leaf_Type = unique(dat_gather_decomp_ns$Leaf_Type),
  Salinity = unique(dat_gather_decomp_ns$Salinity)
)
newdat$weight_change <- exp(predict(rich_decomp,newdata = newdat))

g1 <- ggplot(data = dat_gather_decomp_ns, aes(m_rich,weight_change)) + geom_point(aes(color = as.factor(Salinity), 
                                                                                      shape = Leaf_Type))

g2 <- g1 + geom_line(data = newdat, aes(m_rich, weight_change, linetype=as.factor(Dispersal), color = as.factor(Salinity)),size=1)

g3 <- g2 + facet_wrap(~Leaf_Type)
