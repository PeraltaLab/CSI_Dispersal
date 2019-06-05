library(tidyverse)

full_data <-read.csv("full_data.csv")


daphniidae <- full_data %>%
  select(Scapholeberis,Daphnia, Simocephalus, Daphniidae,Ceriodaphnia)

daphniidae1 <- rowSums(daphniidae)

family_data <- full_data %>%
  select(-Scapholeberis,-Daphnia, -Simocephalus, -Daphniidae,-Ceriodaphnia)

family_data$Daphniidae <- daphniidae1

write.csv(family_data, file = "family_data.csv" )
