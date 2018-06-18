# ecosystem function
source("Beta_Diversity_PCoA_and_PERMANOVA.R")
source("alpha_family.R")

decomp_full <- ("decomp_full.csv")

#make new columns with proportional difference in weights
decomp_full$diff_phrag <- with(decomp_full, DryWt_Phragmites/Phragmites)
decomp_full$diff_maple <- with(decomp_full, DryWt_Maple/Maple)
decomp_full$diff_spar <- with(decomp_full, DryWt_Spartina/Spartina)


## linear model - how is alpha div related to ecosystem function
#bring in micorbial data
div <-read.csv("Microbial_Treatment_Diversity.csv")
div1 <- div %>%
  dplyr::select (Replicate, Treatment, richness, Date2 ) %>%
  filter (Date2 == 45)

# need a dataframe to merge with decomp data that is the correct order
z_rich <- alpha %>%
  dplyr::select (Replicate, Treatment, Salinity_Measured, richness,Day ) %>%
  filter (Day == 45)

# add new dataframe with information to decomp data frame- joins by aligning columns of the same name so order isn't messed up
z_dat <- left_join(z_rich,decomp_full)
colnames(z_dat)[which(names(z_dat) == "richness")] <- "z_rich"

# add in microbial richness
dat_gather_decomp <- left_join(z_dat,div1)
colnames(dat_gather_decomp)[which(names(dat_gather_decomp) == "richness")] <- 
  "m_rich"

##need pcoa axes 1-3 for both zoo and microbes
#pcoa for zoo
js_pcoa$points[,1]
