source("Ecosystem_Function.R")
source("Graphing_Set_Up.R")

newdat <- expand.grid(
  weight_change = 0,
  Dispersal = unique(dat_gather_decomp_ns$Dispersal),
  Day = unique(dat_gather_decomp_ns$Day),
  salinity_measured = seq(0,15,0.05),
  z_richness = mean(dat_gather_decomp_ns$z_richness),
  m_richness = seq(300,990,5),
  Leaf_Type = unique(dat_gather_decomp_ns$Leaf_Type)
)
newdat$weight_change <- exp(predict(rich_decomp,newdata = newdat))
g1 <- ggplot()
