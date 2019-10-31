# Alpha diversity with only family and above level data
# Richness and evenness

library(lme4)
library(vegan)
library(tidyverse)
library(nlme)
library(MASS)

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
## make sure singularity doesn't mess up fixed effects
Rich<-glm(richness~ 
                         Salinity_Measured*as.factor(Dispersal)+
                           Salinity_Measured*Day,
                         data = no_source_all,family = quasipoisson)

#Summary(Rich)
#Summary(Rich_no_source)

## check for autocorrelation
no_source_all$resid <- resid(Rich_no_source)
no_source_all$rep2 <- rep(seq(1,31,1), 6)
newdat <- data.frame(
  day = rep(seq(1,6,1),length(unique(no_source_all$Rep))),
  rep2 = rep(seq(1,31,1),6),
  auto = 0
)

#for (i in 1:length(unique(no_source_all$rep2))){
  #temp <- acf(no_source_all[no_source_all$rep2 == i, ]$resid, plot = F)
  #newdat[newdat$rep2 == i,]$auto <- unlist(temp)
#}
## bah give up on for loop

acf(no_source_all[no_source_all$Rep == "A 4",]$resid)
acf(no_source_all[no_source_all$Rep == "A 5",]$resid)
acf(no_source_all[no_source_all$Rep == "A 6",]$resid)
acf(no_source_all[no_source_all$Rep == "A 7",]$resid)
acf(no_source_all[no_source_all$Rep == "A 8",]$resid)
acf(no_source_all[no_source_all$Rep == "A 9",]$resid)
acf(no_source_all[no_source_all$Rep == "A 10",]$resid)
acf(no_source_all[no_source_all$Rep == "A 11",]$resid)
acf(no_source_all[no_source_all$Rep == "B 4",]$resid)
acf(no_source_all[no_source_all$Rep == "B 5",]$resid)
acf(no_source_all[no_source_all$Rep == "B 6",]$resid)
acf(no_source_all[no_source_all$Rep == "B 8",]$resid)
acf(no_source_all[no_source_all$Rep == "B 9",]$resid)
acf(no_source_all[no_source_all$Rep == "B 10",]$resid)
acf(no_source_all[no_source_all$Rep == "B 11",]$resid)
acf(no_source_all[no_source_all$Rep == "C 4",]$resid)
acf(no_source_all[no_source_all$Rep == "C 5",]$resid)
acf(no_source_all[no_source_all$Rep == "C 6",]$resid)
acf(no_source_all[no_source_all$Rep == "C 7",]$resid)
acf(no_source_all[no_source_all$Rep == "C 8",]$resid)
acf(no_source_all[no_source_all$Rep == "C 9",]$resid)
acf(no_source_all[no_source_all$Rep == "C 10",]$resid)
acf(no_source_all[no_source_all$Rep == "C 11",]$resid)
acf(no_source_all[no_source_all$Rep == "D 4",]$resid)
acf(no_source_all[no_source_all$Rep == "D 5",]$resid)
acf(no_source_all[no_source_all$Rep == "D 6",]$resid)
acf(no_source_all[no_source_all$Rep == "D 7",]$resid)
acf(no_source_all[no_source_all$Rep == "D 8",]$resid)
acf(no_source_all[no_source_all$Rep == "D 9",]$resid)
acf(no_source_all[no_source_all$Rep == "D 10",]$resid)
acf(no_source_all[no_source_all$Rep == "D 11",]$resid)



###
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

