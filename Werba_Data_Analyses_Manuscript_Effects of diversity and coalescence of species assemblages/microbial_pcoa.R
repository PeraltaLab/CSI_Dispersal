# PCoA for microbial data
library(vegan)
library(tidyverse)

# no source pcoa
csi.full.ns <- read.csv("csi.full.ns.csv")

#make community matrix
csi_comm_no_source <- csi.full.ns %>%
  dplyr::select(-c(X,Field_ID,Date, Date2, Replicate, Treatment, Dispersal,
            Salinity, Salinity_real, NH4um, NO3um, PO4um, Maple_dmass, 
            Spartina_dmass, Phrag_dmass, Cmin, Sample..))

#make distance matrix
no_source_dist <- vegdist(csi_comm_no_source, method = "bray")
#run PCoA
no_source_pcoa <- cmdscale(no_source_dist, k=3, eig = TRUE, add = FALSE)
(expvar1 <- round(no_source_pcoa$eig[1] / sum(no_source_pcoa$eig), 3) * 100)  #17.3
(expvar2<- round(no_source_pcoa$eig[2] / sum(no_source_pcoa$eig), 3) * 100)  #7.3 

# source only pcoa
csi.relabun.full2 <- read.csv("csi.relabun.full2.csv") #make distance matrix

#remove everything that isn't community data
comm_source <-csi.relabun.full2 %>%
  dplyr::select(-c(X,CSI_ID,Date,Date2,Replicate,Number, Tank_Name, Salinity, Dispersal))

# run distance matrix
source_dist <- vegdist(comm_source, method = "bray")

#run PCoA
source_pcoa <- cmdscale(source_dist, k=3, eig = TRUE, add = FALSE)
(expvar1_s <- round(source_pcoa$eig[1] / sum(source_pcoa$eig), 3) * 100)  # 29.3
(expvar2_s<- round(source_pcoa$eig[2] / sum(source_pcoa$eig), 3) * 100)  #9.7

## make dataframe with both source and non-source and run pcoa
names(csi.relabun.full2)[names(csi.relabun.full2) == 'Number'] <- 'Treatment'
csi.relabun.full2$Dispersal <- as.numeric(csi.relabun.full2$Dispersal)  #source 0 is 1 source13 is 2

for(i in 1:nrow(csi.relabun.full2)){
  if (csi.relabun.full2$Dispersal[i] == 1) {
    csi.relabun.full2$Dispersal[i] <- 0
  } else
    if (csi.relabun.full2$Dispersal[i] == 2) {
      csi.relabun.full2$Dispersal[i] <- 1
    }
}  
  
dat <- full_join(csi.full.ns,csi.relabun.full2)


comm_all <- dat %>% dplyr::select(contains("Otu"))

all_dist <- vegdist(comm_all, method = "bray")
all_pcoa <- cmdscale(all_dist, k=3, eig = TRUE, add = FALSE)
(expvar1_s <- round(all_pcoa$eig[1] / sum(all_pcoa$eig), 3) * 100)  # 19.3
(expvar2_s<- round(all_pcoa$eig[2] / sum(all_pcoa$eig), 3) * 100)  #6.9


## CCA

con <- cca(formula = csi_comm_no_source ~Salinity_real+Maple_dmass + Spartina_dmass +Phrag_dmass + Cmin, data =csi.full.ns )

plot(con)



