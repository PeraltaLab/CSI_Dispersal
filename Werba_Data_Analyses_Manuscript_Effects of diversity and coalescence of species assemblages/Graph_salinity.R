full_data <- read.csv("full_data.csv")

source("Graphing_Set_Up.R")

ggplot(data = full_data, aes(Day, Salinity_Measured, group = Salinity_Treat)) +
  geom_point(aes(color= as.factor(Salinity_Treat))) + 
  scale_color_brewer(name = "Salinity Treatment",type = "seq", palette = "Dark2")+
  ylab("Salinity")
