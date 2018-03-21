# Alpha diversity
# Richness and evenness

library(lme4)
library(vegan)
library(betareg)

full_data <-read.csv("full_data.csv")

# calculate richness 
richness <- specnumber(full_data[,-c(1:6)], MARGIN = 1)

#calculate shannon's diversity 
shannon_div <- diversity(full_data[,-c(1:6)], index = "shannon", MARGIN = 1, base = exp(2))

#calculate evenness -- Pielous evennnes shannon diversity/ log(richness)
evenness <- shannon_div/log(richness)

#make a dataframe with all alpha diversity indices and all data from full data
alpha <- as.data.frame(cbind(full_data,richness, shannon_div,evenness))

#remove tank B7 which didn't exist due to leaking
alpha <- alpha[-which(alpha$Replicate =="B" & alpha$Treatment==7),]

# remove source tanks
no_source_all <- alpha[alpha$Dispersal!= 0 & alpha$Dispersal!=1, ]

# ony source tanks
source_all <- alpha[alpha$Dispersal== 0 & alpha$Dispersal==1, ]

# richness over time given treatment for non-source tanks
# I used poisson because it is count data but was slightly underdispersed so switched to quasipoisson
Rich_no_source<-glm(richness~Dispersal+sal*Date,data = no_source_all,family =quasipoisson)

#check for model assumptions
plot(resid(Rich_no_source))

#evenness over time given treatment for non-source tanks
# here I use a beta distribution because evennenss is constrained between 0 and 1 but is not success/failure
#even_no_source <- betareg(evenness~Dispersal+sal*Date,data = no_source_all)-- evennness is very weird in this data because some tanks only have one species



