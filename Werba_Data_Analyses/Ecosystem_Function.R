## ecosystem functions
## mantel test with vegdist-- how is beta div related to ecosystem function
## linear model with richness, salinity and dispersal?? how is alpha div related to ecosystem function

library(ade4)
source("alpha_family.R")

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

#first read in full data
decomp_full <- read.csv("decomp_full.csv")


#make new columns with proportional difference in weights
decomp_full$diff_phrag <- with(decomp_full, DryWt_Phragmites/Phragmites)
decomp_full$diff_maple <- with(decomp_full, DryWt_Maple/Maple)
decomp_full$diff_spar <- with(decomp_full, DryWt_Spartina/Spartina)


#take only the difference columns 
decomp_final <- decomp_full %>%
  dplyr::select(c(diff_phrag,diff_maple,diff_spar)) 


#make distance matrix
dist.decomp <- vegdist(decomp_final, method = "euclidean" )

#run mantel test
mantel.rtest(dist.zoop, dist.decomp, nrepet = 999)
#obs(rsquared):0.03797966  #p=.0.249 std obs: 0.598

#now do for Carbon and zooplankton
carbon <- decomp_full %>% 
  dplyr::select(Cmin)

#distance matrix
dist.carb <- vegdist(carbon,method = "euclidean")

#run mantel test
mantel.rtest(dist.zoop, dist.carb, nrepet = 999)
#p= 0.001, r = 0.269


## linear model with richness- how is alpha div related to ecosystem function
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
colnames(dat_gather_decomp)[which(names(dat_gather_decomp) == "richness")] <- "m_rich"


#first need a new dataframe with all the decomp differences into one column and add a type column
##start here just changed decompp final to full maybe will work w/o lines afterward?
dat_gather_decomp1 <- dat_gather_decomp %>%
  gather(diff_phrag,diff_maple,diff_spar, key = "Leaf_Type", value = "weight_change" )



#remove source tanks
dat_gather_decomp_ns <- dat_gather_decomp1 %>% 
  filter(Dispersal == 3 | Dispersal == 2)


#run linear model
#used log(data) since proportions can't be negative

rich_decomp <- lm(log(weight_change)~(log(z_rich)+log(m_rich)+Salinity_Measured+
                                        as.factor(Dispersal))*Leaf_Type, 
                  data = dat_gather_decomp_ns)




#check model # some weird tails
plot(resid(rich_decomp))
qqnorm(resid(rich_decomp))
qqline(resid(rich_decomp))

#exp(-0.295114) = 0.744 therefore when e fold increase in  microbes regardless of everything else, .74 fold less of leaf remaining than you would have had at half the microbes
# == 1.34x the loss


#microbial mantel test with cim

#linear model for carbon mineralization

cmin_lm <-lm(Cmin~((z_rich)+(m_rich)+Salinity_Measured+
                                   as.factor(Dispersal)), 
             data = dat_gather_decomp_ns) 
#check model
plot(resid(cmin_lm))
qqnorm(resid(cmin_lm))
qqline(resid(cmin_lm))
