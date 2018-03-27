#matching models- so slight changes to microbial div models and graphing
library(lme4)
library(MASS)
library(betareg)
div <- read.csv("Microbial_Diversity.csv")

#Shannon's Diversity

shan_lm <- lm(shannon ~ Dispersal+Salinity_real*Date2, data = div)

plot(resid(shan_lm))
qqnorm(resid(shan_lm))
qqline(resid(shan_lm))

#richness- this is a count so I used poisson- but was super overdispersed (38) so changed to negative binomial 
#now dispersion is close to expected 1 (1.05)

rich_lm <- glm.nb(richness ~ Dispersal+Salinity_real*Date2, data = div)

plot(resid(rich_lm))


#Evenness - continuous bounded between 0 and 1 so used beta distribution- #logit link
even_lm <- betareg(J ~ Dispersal+Salinity_real*Date2, data = div)

plot(resid(even_lm))
