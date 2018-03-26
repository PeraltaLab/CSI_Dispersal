source("Ecosystem_Function.R")
source("Graphing_Set_up.R")

#need a dataframe for prediction lines, yes it weird to do richness with fractions but i want a smooth line

newdat <- expand.grid(
  richness = seq(1,6,0.01),
  Leaf_Type = unique(dat_gather_decomp$Leaf_Type),
  dispersal = unique(dat_gather_decomp$dispersal),
  salinity = unique(dat_gather_decomp$salinity),
  weight_change = 0
)
newdat$weight_change <- exp(predict(rich_decomp,newdata = newdat))

leaf_type_names <- c("diff_maple" ="Maple", "diff_phrag" = "Phragmites","diff_spar" = "Spartina")

# want to only graph main experiment dispersal - no source no freshwater control
dat_gather_decomp1 <- dat_gather_decomp %>%
  filter(dispersal == 2 | dispersal == 3)
newdat <- newdat %>%
  filter(dispersal == 2 | dispersal == 3)

dec_g <- ggplot(dat_gather_decomp1, aes(richness, weight_change)) + 
  geom_point(aes(color=as.factor(salinity)),size = 2)

dec_g1 <- dec_g + geom_line(data = newdat
                            , aes(richness,weight_change,  color = as.factor(salinity)
                                  , linetype = as.factor(dispersal)), size = 1)

(dec_g3 <- dec_g1 + facet_grid(Leaf_Type ~ ., labeller = as_labeller(leaf_type_names)) + 
    scale_color_brewer(type = "seq",palette = "Dark2") +
    ylab("Proportional Change in Dry Weight") + xlab("Zooplankton Richness") + labs(color = "Salinity")+
    scale_linetype_manual(name = "Dispersal Prediction Lines",values = c(1,2),
                          breaks = c(2,3),labels = c("Mixed Salt and Fresh","Salt Only")))

