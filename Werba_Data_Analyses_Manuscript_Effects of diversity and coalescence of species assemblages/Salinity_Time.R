library(tidyverse)
source("Graphing_Set_Up.R")
full_data <-read.csv("family_data.csv")

sal <- full_data %>%
  select(Salinity_Treat,Salinity_Measured,Day,Replicate, Treatment)

#D2 day 9 needs to be checked. 

(g1 <- ggplot(sal, aes(Day, Salinity_Measured)) + 
  geom_point(aes(color=as.factor(Salinity_Treat))) + 
  scale_color_brewer(name = "Salinity Treatment", type = "seq", palette = "Dark2")+
  ylab("Salinity Measured"))

  