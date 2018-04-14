# PCoA for microbial data
library(vegan)
library(tidyverse)

# no source pcoa
csi.full.ns <- read.csv("csi.full.ns.csv")

#make community matrix
csi_comm_no_source <- csi.full.ns %>%
  select(-c(X,Field_ID,Date, Date2, Replicate, Treatment, Dispersal,
            Salinity, Salinity_real, NH4um, NO3um, PO4um, Maple_dmass, 
            Spartina_dmass, Phrag_dmass, Cmin, Sample..))

#make distance matrix
no_source_dist <- vegdist(csi_comm_no_source, method = "bray")
#run PCoA
no_source_pcoa <- cmdscale(no_source_dist, k=3, eig = TRUE, add = FALSE)
(expvar1 <- round(no_source_pcoa$eig[1] / sum(no_source_pcoa$eig), 3) * 100)  #17.2
(expvar2<- round(no_source_pcoa$eig[2] / sum(no_source_pcoa$eig), 3) * 100)  #7.4 

# source only pcoa
csi.relabun.full2 <- read.csv("csi_relabun.full2.csv") #make distance matrix

#remove everything that isn't community data
comm_source <-csi.relabun.full2 %>%
  select(-c(X,CSI_ID,Date,Date2,Replicate,Number, Tank_Name, Salinity, Dispersal))

# run distance matrix
source_dist <- vegdist(comm_source, method = "bray")

#run PCoA
source_pcoa <- cmdscale(source_dist, k=3, eig = TRUE, add = FALSE)
(expvar1_s <- round(source_pcoa$eig[1] / sum(source_pcoa$eig), 3) * 100)  # 29.1
(expvar2_s<- round(source_pcoa$eig[2] / sum(source_pcoa$eig), 3) * 100)  #9.8
