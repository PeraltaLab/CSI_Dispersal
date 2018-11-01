#matching models- so slight changes to microbial div models and graphing
library(lme4)
library(MASS)
library(betareg)
div <- read.csv("Microbial_Treatment_Diversity.csv")

#Shannon Diversity

shan_lm <- lm(shannon ~ as.factor(Dispersal)+Salinity_real*Date2, data = div)

plot(resid(shan_lm))
qqnorm(resid(shan_lm))
qqline(resid(shan_lm))

#richness- this is a count so I used poisson- but was super overdispersed (38) so changed to negative binomial 
#now dispersion is close to expected 1 (1.05)

div$Rep <- paste(div$Replicate,div$Treatment)

rich_lm1 <- glm.nb(richness ~ as.factor(Dispersal)*Date2+Salinity_real*Date2, data = div)
rich_lm <- glmer.nb(richness ~ as.factor(Dispersal)*Date2+Salinity_real*Date2+ (1+Date2|Rep), data = div)

plot(resid(rich_lm))


#Evenness - continuous bounded between 0 and 1 so used beta distribution- #logit link
even_lm <- betareg(J ~ as.factor(Dispersal)+Salinity_real*Date2, data = div)

plot(resid(even_lm))


## for source tanks
s_div <- read.csv("Microbial_Source_Diversity.csv")

#Shannon Diversity

shan_lm_s <- lm(shannon.source ~ Salinity_Measured*Date2, data = s_div)

plot(resid(shan_lm_s))
qqnorm(resid(shan_lm_s))
qqline(resid(shan_lm_s))

#richness- this is a count so I used poisson- but was super overdispersed so changed to negative binomial 
#now dispersion is close to expected 1 (1.2)

rich_lm_s <- glm.nb(richness.source ~ Salinity_Measured*Date2, data = s_div)

plot(resid(rich_lm_s))


#Evenness - continuous bounded between 0 and 1 so used beta distribution- #logit link
even_lm_s <- betareg(J.source ~ Salinity_Measured*Date2, data = s_div)

plot(resid(even_lm_s))


