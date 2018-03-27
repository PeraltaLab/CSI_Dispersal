source("Ecosystem_Function.R")
source("Graphing_Set_up.R")

#need a dataframe for prediction lines, yes it weird to do richness with fractions but i want a smooth line

newdat <- expand.grid(
  m_richness = seq(306,987,10),
  z_richness = seq(1,6),
  Leaf_Type = unique(dat_gather_decomp_ns$Leaf_Type),
  Dispersal = unique(dat_gather_decomp_ns$Dispersal),
  Salinity = unique(dat_gather_decomp_ns$Salinity),
  weight_change = 0
)
newdat$weight_change <- exp(predict(rich_decomp,newdata = newdat))

leaf_type_names <- c("diff_maple" ="Maple", "diff_phrag" = "Phragmites","diff_spar" = "Spartina")


dec_g <- ggplot(dat_gather_decomp_ns, aes(m_richness, weight_change)) + 
  geom_point(aes(color=as.factor(Salinity)),size = 2)

dec_g1 <- dec_g + geom_line(data = newdat
                            , aes(m_richness,weight_change,  color = as.factor(Salinity)
                                  , linetype = as.factor(Dispersal)), size = 1)

(dec_g3 <- dec_g1 + facet_grid(Leaf_Type ~ ., labeller = as_labeller(leaf_type_names)) + 
    scale_color_brewer(type = "seq",palette = "Dark2") +
    ylab("Proportional Change in Dry Weight") + xlab("Microbial Richness") + labs(color = "Salinity")+
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2),
                          breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")))

#alternate graph across zooplankton richness-- for supplement
newdat1 <- expand.grid(
  m_richness = seq(306,987,20),
  z_richness = seq(1,6,0.5),
  Leaf_Type = unique(dat_gather_decomp_ns$Leaf_Type),
  Dispersal = unique(dat_gather_decomp_ns$Dispersal),
  Salinity = unique(dat_gather_decomp_ns$Salinity),
  weight_change = 0
)
newdat1$weight_change <- exp(predict(rich_decomp,newdata = newdat1))


dec_g <- ggplot(dat_gather_decomp_ns, aes(z_richness, weight_change)) + 
  geom_point(aes(color=as.factor(Salinity)),size = 2)

dec_g1 <- dec_g + geom_line(data = newdat1
                            , aes(z_richness,weight_change,  color = as.factor(Salinity)
                                  , linetype = as.factor(Dispersal)), size = 1)

(dec_g3 <- dec_g1 + facet_grid(Leaf_Type ~ ., labeller = as_labeller(leaf_type_names)) + 
    scale_color_brewer(type = "seq",palette = "Dark2") +
    ylab("Proportional Change in Dry Weight") + xlab("Zooplankton Richness") + labs(color = "Salinity")+
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2),
                          breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")))
h