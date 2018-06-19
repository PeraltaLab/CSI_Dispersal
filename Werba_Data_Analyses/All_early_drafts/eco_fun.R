# ecosystem function
source("Beta_Diversity_PCoA_and_PERMANOVA.R")
source("alpha_family.R")
source("microbial_pcoa.R")
library("ade4")
library("tidyverse")
#make relative abundance matrix
zoo_rel <- community
for (i in 1:dim(community)[1]) {
  zoo_rel[i, ] <- community[i, ]/sum(community[i,])
}

#add date column
zoo_rel$Day <- full_data$Day

#subset final day and remove day column and any columns that have rowsum of 0
zoo_rel_final <- zoo_rel %>%
  filter(Day == 45) %>%
  dplyr::select(-Day) 


zoo_rel_final <- filter(zoo_rel_final, rowSums(zoo_rel_final) != 0)
#distance matrix zooplankton
dist.zoop <- vegdist(zoo_rel_final, method = "bray")

#distance matrix decomposition

decomp_full <- read.csv("decomp_full.csv")

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

##need pcoa axes 1-3 for both zoo and microbes, all tanks
#pcoa for zoo
full_data <-read.csv("family_data.csv")
#remove tank B7 which didn't exist due to leaking
dat <- full_data[-which(full_data$Replicate =="B" & full_data$Treatment==7),]
#only final day
final_dates <- dat %>%
  filter(Day==45)
#remove all rows that sums = zero
final_dates1 <- final_dates %>%
  dplyr::select(-c(X,Date,Day, Replicate,Treatment, Salinity_Treat, Dispersal,
                   Salinity_Measured))

#make community matrix
zoo_comm <- final_dates1 [rowSums(final_dates1!= 0),]
#make distance matrix
zoo_dist <- vegdist(zoo_comm, method = "bray")
#run PCoA
zoo_pcoa <- cmdscale(zoo_dist, k=3, eig = TRUE, add = FALSE)
(expvar1_s <- round(zoo_pcoa$eig[1] / sum(zoo_pcoa$eig), 3) * 100)  #71.4
(expvar2_s <- round(zoo_pcoa$eig[2] / sum(zoo_pcoa$eig), 3) * 100)  #23.1

#RDA
zoo_final <- alpha %>%
  filter(Day == 45) %>%
  dplyr::select(-c(Date,X,Day,Replicate,Treatment,Salinity_Treat,
                   Dispersal, Salinity_Measured,richness,shannon_div,
                   evenness))


crda <- rda(zoo_final~carbon$Cmin)
plot(crda,choices=c(1,2))

#PCoA for microbes


#linear models

#decompostion
#first need a new dataframe with all the decomp differences into one column and add a type column
##start here just changed decompp final to full maybe will work w/o lines afterward?
dat_gather_decomp1 <- dat_gather_decomp %>%
  gather(diff_phrag,diff_maple,diff_spar, key = "Leaf_Type", value = "weight_change" )