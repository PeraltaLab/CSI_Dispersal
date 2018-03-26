## ecosystem functions
## mantel test with vegdist-- how is beta div related to ecosystem function
## linear model with richness, salinity and dispersal?? how is alpha div related to ecosystem function

library(ade4)
source("Alpha_diversity.R")

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
  select(-Day) 


zoo_rel_final <- filter(zoo_rel_final, rowSums(zoo_rel_final) != 0)

#distance matrix zooplankton
dist.zoop <- vegdist(zoo_rel_final, method = "bray")

#distance matrix decomposition

#first read in full data
decomp_full <- read.csv("decomp_full.csv")

#make new columns with proportional difference in weights
decomp_full$diff_phrag <- with(decomp_full, Phragmites/DryWt_Phragmites)
decomp_full$diff_maple <- with(decomp_full, Maple/DryWt_Maple)
decomp_full$diff_spar <- with(decomp_full, Spartina/DryWt_Spartina)

#remove rows that were removed from zooplankton (e.g. zero zooplankton)
decomp_full <- decomp_full[-1, ]

#take only the difference columns 
decomp_final <- decomp_full %>%
  select(c(diff_phrag,diff_maple,diff_spar)) 


#make distance matrix
dist.decomp <- vegdist(decomp_final, method = "euclidean" )

#run mantel test
mantel.rtest(dist.zoop, dist.decomp, nrepet = 999)
#p=.174 rsqaured= 0.051

#now do for Carbon and zooplankton
carbon <- decomp_full %>% 
  select(Cmin)

#distance matrix
dist.carb <- vegdist(carbon,method = "euclidean")

#run mantel test
mantel.rtest(dist.zoop, dist.carb, nrepet = 999)
#p= 0.044, rsquared = 0.09


## linear model with richness?? how is alpha div related to ecosystem function

#first need a new dataframe with all the decomp differences into one column and add a type column

dat_gather_decomp <- decomp_final %>%
  gather(diff_phrag,diff_maple,diff_spar, key = "Leaf_Type", value = "weight_change" )

# add in salinity and dispersal information
dat_gather_decomp$salinity <- rep(decomp_full$Salinity,3)
dat_gather_decomp$dispersal <- rep(decomp_full$Dispersal,3)

#add in richness information
richness <- alpha$richness[alpha$Day == 45]
#remove column with no zooplankton
richness <- richness[-1]
#add to dataframe
dat_gather_decomp$richness <- rep(richness, 3)

#run linear model
#used log(data) since proportions can't be negative
rich_decomp <- lm(log(weight_change)~(richness+salinity+dispersal)*Leaf_Type, data = dat_gather_decomp)

#check model # some weird tails
plot(resid(rich_decomp))
qqnorm(resid(rich_decomp))
qqline(resid(rich_decomp))
