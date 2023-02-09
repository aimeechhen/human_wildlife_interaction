
### animal interaction full code with details ####

library("readr")
library("ggplot2")

setwd("C:/Users/achhen/OneDrive - UBC/BIOL 520C Statistical Modelling/Github/animal-interaction/R working directory")

data <- read_csv("C:/Users/achhen/OneDrive - UBC/BIOL 520C Statistical Modelling/Github/animal-interaction/pca-human-wildlife-coexistence-animals-involved-detailed-records-2010-2021.csv")
# DATA <- read_csv("https://open.canada.ca/data/en/datastore/dump/b2a9f7e4-7c49-471d-8337-0192c15dd52a?bom=True")
  # REFERENCE: https://open.canada.ca/data/en/dataset/cc5ea139-c628-46dc-ac55-a5b3351b7fdf/resource/b2a9f7e4-7c49-471d-8337-0192c15dd52a?inner_span=True

## PRE-REGISTRATION 
# exploring and visualization of the data

ggplot() +
  geom_point(aes(x = data$`Incident Date`, y = data$`Animal Behaviour`), alpha = 0.1) + theme_bw()

ggplot() +
  geom_point(aes(x = data$`Incident Date`, y = data$`Species Common Name`), alpha = 0.1) + theme_bw()

#Some subsetting and carpentry
data2 <- data[which(data$`Incident Type` == "Human Wildlife Interaction"),]
names(data2)[2] <- "date"
names(data2)[6] <- "species"
names(data2)[4] <- "park"
names(data2)[5] <- "interaction"

test <- aggregate(interaction ~ date + park, data = data2, FUN = "length")

ggplot(test) +
  geom_point(aes(x = date, y = interaction, col = park), alpha = 0.2) + theme_bw()

test2 <- aggregate(interaction ~ date + species, data = data2, FUN = "length")
ggplot(test2) +
  geom_point(aes(x = date, y = interaction, col = species), alpha = 0.2) + theme_bw()


