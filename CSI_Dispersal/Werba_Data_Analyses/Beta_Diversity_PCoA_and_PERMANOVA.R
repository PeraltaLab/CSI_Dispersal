## beta diversity
## PCoA and permanova
library(vegan)


full_data <-read.csv("family_data.csv")

#remove tank B7 which didn't exist due to leaking
dat <- full_data[-which(full_data$Replicate =="B" & full_data$Treatment==7),]

# take only 3 dates to match microbial data and to represent the beginning middle and end
all_3dates <- dat [dat$Date==42166 | dat$Date==42184 |dat$Date==42211, ]

#remove all rows that sums = zero
all_3dates1 <- all_3dates[rowSums(all_3dates[,-c(1:6)]) != 0,]

#just source for PCoA
js_3dates <- all_3dates1[all_3dates1$Dispersal==0, ]

#make community matrix
js_comm <- js_3dates[,-c(1:6)]

#make distance matrix
js_dist <- vegdist(js_comm, method = "bray")

#run PCoA
js_pcoa <- cmdscale(js_dist, k=3, eig = TRUE, add = FALSE)
(expvar1_s <- round(js_pcoa$eig[1] / sum(js_pcoa$eig), 3) * 100)  #45.1
(expvar2_s <- round(js_pcoa$eig[2] / sum(js_pcoa$eig), 3) * 100)  #14.3

#no source for PCoA
ns_3dates <- all_3dates1[all_3dates1$Dispersal!= 0 & all_3dates1$Dispersal!=1, ]

#make community matrix
ns_comm <- ns_3dates[,-c(1:5)]

#make distance matrix
ns_dist <- vegdist(ns_comm, method = "bray")

#run PCoA
ns_pcoa <- cmdscale(ns_dist, k=3, eig = TRUE, add = FALSE)
expvar1 <- round(ns_pcoa$eig[1] / sum(ns_pcoa$eig), 3) * 100  #34.3
expvar2 <- round(ns_pcoa$eig[2] / sum(ns_pcoa$eig), 3) * 100  #16.8


#PERMANOVA ---- this combines source and no source
#remove rows with all zeros
no_zero <- dat[rowSums(dat[,-c(1:6)]) != 0, ]

#run PERMANOVA
adonis = adonis(no_zero[,-c(1:6)] ~ Day*Dispersal*Salinity_Measured, method = "bray",data=no_zero, perm=1000)




