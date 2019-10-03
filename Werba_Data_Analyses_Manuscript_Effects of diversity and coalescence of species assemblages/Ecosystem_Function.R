## ecosystem functions
## linear model with richness, salinity and dispersal how is alpha div related to ecosystem function
library(plyr)
source("alpha_family.R")  #alpha dataframe includes all dates, tanks, zooplankton, richness
#first read in ecosystem function data
decomp_full <- read.csv("decomp_full.csv")

#make new columns with proportional difference in weights
decomp_full$diff_phrag <- with(decomp_full, DryWt_Phragmites/Phragmites)
decomp_full$diff_maple <- with(decomp_full, DryWt_Maple/Maple)
decomp_full$diff_spar <- with(decomp_full, DryWt_Spartina/Spartina)

#bring in micorbial data
div <-read.csv("Microbial_Treatment_Diversity.csv")
div1 <-read.csv("Microbial_Source_Diversity.csv")

div <- div %>% 
  filter(Date2==45) %>%
  dplyr::select(Date2,Replicate,Treatment, Dispersal,Salinity_real,richness)
names(div) <- c("Day", "Replicate","Treatment",
                "Dispersal","Salinity_Measured","richness")
div1 <- div1 %>%
  filter(Date2==45) %>%
  dplyr::select(Date2,Replicate,Number,Salinity_Measured,richness.source)

names(div1) <- c("Day","Replicate","Treatment","Salinity_Measured","richness")
div1$Dispersal <- as.factor(0)
#combine the source and treatment tanks into one dataframe

micro_div <- cbind(names=c(rownames(div), rownames(div1)),
                   plyr::rbind.fill(list(div, div1)))

micro_dat <- micro_div %>%
  dplyr::select(Day, Replicate,Treatment,richness)


## linear model with richness- how is alpha div related to ecosystem function


# need a dataframe to merge with decomp data that is the correct order
z_rich <- alpha %>%
  dplyr::select (Replicate, Treatment, Salinity_Measured, richness,Day ) %>%
  filter (Day == 45)

# add new dataframe with information to decomp data frame- joins by aligning columns of the same name so order isn't messed up
z_dat <- left_join(z_rich,decomp_full)
colnames(z_dat)[which(names(z_dat) == "richness")] <- "z_rich"

#remove control tanks
z_dat <- z_dat %>%
  filter(Treatment !=3 )

# add in microbial richness
dat_gather_decomp <- left_join(z_dat,micro_dat)
colnames(dat_gather_decomp)[which(names(dat_gather_decomp) == "richness")] <- 
  "m_rich"


#first need a new dataframe with all the decomp differences into one column and add a type column
##start here just changed decompp final to full maybe will work w/o lines afterward?
dat_gather_decomp1 <- dat_gather_decomp %>%
  gather(diff_phrag,diff_maple,diff_spar, key = "Leaf_Type", value = "weight_change" )

dat_gather_decomp2 <- dat_gather_decomp1 %>% filter(Dispersal != 0 )

## try shifting the intercept
dat_gather_decomp2 <- dat_gather_decomp2 %>% 
  mutate(m_rich = m_rich - min(m_rich))

#run linear model
#used log(data) since proportions can't be negative
library(betareg)

rich_decomp <- betareg(weight_change~(z_rich)+
                         m_rich+
                         as.factor(Dispersal)+
                         Salinity_Measured*Leaf_Type
                         # Salinity_Measured
                    , 
                  data = dat_gather_decomp2)

#rich_decomp <- lm(log(weight_change)~(z_rich)+(m_rich)+as.factor(Dispersal)+
 #                                       Leaf_Type*Salinity_Measured, 
  #                data = dat_gather_decomp2)




#check model # some weird tails
plot(resid(rich_decomp))
qqnorm(resid(rich_decomp))
qqline(resid(rich_decomp))



#linear model for carbon mineralization
#remove tank A8 that has a 0 in carbon mineralization
dat_gather_decomp3 <- dat_gather_decomp2 %>%
  filter(Cmin != 0)
cmin_lm <-lm(log(Cmin)~(z_rich)+(m_rich)+Salinity_Measured +
               as.factor(Dispersal), 
             data = dat_gather_decomp3)
#check model
plot(resid(cmin_lm))
qqnorm(resid(cmin_lm))
qqline(resid(cmin_lm))


## alternative post-hoc model
cmin_2 <-lm(log(Cmin)~(z_rich)+(m_rich)+poly(Salinity_Measured,2) +
               as.factor(Dispersal), 
             data = dat_gather_decomp3)


extractAIC(cmin_lm)

extractAIC(cmin_2)


plot(resid(cmin_2))
qqnorm(resid(cmin_2))
qqline(resid(cmin_2))


