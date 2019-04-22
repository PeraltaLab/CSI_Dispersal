# Alpha diversity with only family and above level data
# Richness and evenness

library(lme4)
library(vegan)
library(tidyverse)


full_data <-read.csv("family_data.csv")

#remove tank B7 which didn't exist due to leaking
full_data <- full_data[-which(full_data$Replicate =="B" & full_data$Treatment==7),]

#make just a community dataframe
community <- full_data %>% 
  dplyr::select(-c(Date,Day,Replicate,Treatment,Salinity_Treat,Dispersal,Salinity_Measured))

# calculate richness 

richness <- specnumber(community, MARGIN = 1)

#calculate shannon's diversity 
shannon_div <- diversity(community, index = "shannon", MARGIN = 1, base = exp(2))

#calculate evenness -- Pielous evennnes shannon diversity/ log(richness)
evenness <- shannon_div/log(richness)

#make a dataframe with all alpha diversity indices and all data from full data
alpha <- as.data.frame(cbind(full_data,richness, shannon_div,evenness))
alpha$Rep <- paste(alpha$Replicate,alpha$Treatment)
# remove source tanks
no_source_all <- alpha[alpha$Dispersal!= 0 & alpha$Dispersal!=1, ]

# ony source tanks
source_all <- alpha[alpha$Dispersal== 0, ]

# richness over time given treatment for non-source tanks
# I used poisson because it is count data but was slightly underdispersed so switched to quasipoisson- same for both
Rich_no_source<-glmer.nb(richness~ 
                           Salinity_Measured*as.factor(Dispersal)+
                           Salinity_Measured*Day+
                           (1+Day|Rep),
                         data = no_source_all,family = quasipoisson(link = "log"))


Rich_no_source2<-glmer(richness~as.factor(Dispersal)*Day+
                         Salinity_Measured*Day+ as.factor(Dispersal)+
                         (1+Day|Rep),data = no_source_all,family = "poisson" )


overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(Rich_no_source2)  ## chisq 67, ratio = .381, rdf = 177, p= 1

Rich_no_source1<-glm(richness~as.factor(Dispersal)*Day+Salinity_Measured*Day,data = no_source_all,family = quasipoisson(link = "log"))

Rich_source<-glm(richness~Salinity_Measured*Day,data = source_all,family =quasipoisson)

#check for model assumptions
plot(resid(Rich_no_source))
plot(resid(Rich_source))

#evenness over time given treatment for non-source tanks
# evennness is very weird in this data because 5 tanks only have one species so evenness is Na...

# Shannon's Diversity- accounts for both richness and evenness in some way
Shannon_no_source<-lm(shannon_div~as.factor(Dispersal)+Salinity_Measured*Day,data = no_source_all)


Shannon_source<-lm(shannon_div~Salinity_Measured*Day,data = source_all)

#check for model assumptions 
plot(resid(Shannon_no_source))
qqnorm(resid(Shannon_no_source))
qqline(resid(Shannon_no_source))

plot(resid(Shannon_source))
qqnorm(resid(Shannon_source))
qqline(resid(Shannon_source))

