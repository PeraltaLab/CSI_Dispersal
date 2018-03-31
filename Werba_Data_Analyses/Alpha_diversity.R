# Alpha diversity
# Richness and evenness

library(lme4)
library(vegan)
library(tidyverse)


full_data <-read.csv("full_data.csv")

#remove tank B7 which didn't exist due to leaking
full_data <- full_data[-which(full_data$Replicate =="B" & full_data$Treatment==7),]

#make just a community dataframe
community <- full_data %>% 
  select(-c(Date,Day,Replicate,Treatment,Salinity_Treat,Dispersal,Salinity_Measured))

# calculate richness 
richness <- specnumber(community, MARGIN = 1)

#calculate shannon's diversity 
shannon_div <- diversity(community, index = "shannon", MARGIN = 1, base = exp(2))

#calculate evenness -- Pielous evennnes shannon diversity/ log(richness)
evenness <- shannon_div/log(richness)

#make a dataframe with all alpha diversity indices and all data from full data
alpha <- as.data.frame(cbind(full_data,richness, shannon_div,evenness))

# remove source tanks
no_source_all <- alpha[alpha$Dispersal!= 0 & alpha$Dispersal!=1, ]

# ony source tanks
source_all <- alpha[alpha$Dispersal== 0 | alpha$Dispersal==1, ]

# richness over time given treatment for non-source tanks
# I used poisson because it is count data but was slightly underdispersed so switched to quasipoisson- same for both
Rich_no_source<-glm(richness~as.factor(Dispersal)+Salinity_Measured*Day,data = no_source_all,family =quasipoisson)

Rich_source<-glm(richness~Salinity_Measured*Day,data = source_all,family =quasipoisson)

#check for model assumptions
plot(resid(Rich_no_source))
plot(resid(Rich_source))

#evenness over time given treatment for non-source tanks
# evennness is very weird in this data because some tanks only have one species

# Shannon's Diversity- accounts for both richness and evenness in some way
Shannon_no_source<-lm(shannon_div~as.factor(Dispersal)+Salinity_Measured*Day,data = no_source_all)


Shannon_source<-lm(shannon_div~Salinity_Measured*Day,data = source_all)

#check for model assumptions ## hmm this really isn't normal... not sure what my assumption would be for shannon's index
plot(resid(Shannon_no_source))
qqnorm(resid(Shannon_no_source))
qqline(resid(Shannon_no_source))

plot(resid(Shannon_source))
qqnorm(resid(Shannon_source))
qqline(resid(Shannon_source))

