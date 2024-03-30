# bc national parks data

# Load packages ----
library(readr)
library(lubridate)       #date formats
library(zoo)             #date format 'year-month'
library(dplyr)           #data wrangling

# Parks Canada Agency (PCA) data

# Download Parks Canada Agency dataset
#source: https://open.canada.ca/data/en/dataset/cc5ea139-c628-46dc-ac55-a5b3351b7fdf/resource/b2a9f7e4-7c49-471d-8337-0192c15dd52a?inner_span=True

# Import Parks Canada dataset
#data obtained on Feb. 3, 2023
PCA <- read_csv("Data/HWI/pca-human-wildlife-coexistence-animals-involved-detailed-records-2010-2021.csv")

#There is a total of 35 National Parks listed within the dataset. Our project is interested in the National Parks located within British Columbia (BC).
#There is a total of 9 incident types assigned in the dataset. Our project is interested in the 'Human Wildlife Interaction' (HWI).

#data carpentry
#subset data to National Parks located in BC only
BC <- PCA[which(PCA$`Protected Heritage Area` %in% c("Glacier National Park of Canada",
                                                     "Kootenay National Park of Canada",
                                                     "Mount Revelstoke National Park of Canada",
                                                     "Pacific Rim National Park Reserve of Canada",
                                                     "Yoho National Park of Canada")),]
#subset incident type to human wildlife interactions only
BC <- BC[which(BC$`Incident Type` == "Human Wildlife Interaction"),]
#rename column names to make it easier for coding
names(BC)[2] <- "incident_date"
names(BC)[4] <- "park"
names(BC)[5] <- "HWI"
names(BC)[6] <- "species"
#shorten park names
BC$park[BC$park == "Glacier National Park of Canada"] <- "Glacier"
BC$park[BC$park == "Kootenay National Park of Canada"] <- "Kootenay"
BC$park[BC$park == "Mount Revelstoke National Park of Canada"] <- "Revelstoke"
BC$park[BC$park == "Pacific Rim National Park Reserve of Canada"] <- "Pacific_Rim"
BC$park[BC$park == "Yoho National Park of Canada"] <- "Yoho"

#exclude unknown species
BC <- BC[BC$species != "None",]
BC <- BC[BC$species != "Unknown",]
BC <- BC[BC$species != "Unknown bat",]
BC <- BC[BC$species != "Unknown bear",]
BC <- BC[BC$species != "Unknown bird",]
BC <- BC[BC$species != "Unknown deer",]
BC <- BC[BC$species != "Unknown sea lion",]

# Data preparation for climate data
#Note: climate data is in monthly intervals
#extract year from date then create a column for year, required for climate data
BC$year <- lubridate::year(BC$incident_date)
#extract month from date then create a column for month, required for climate data
BC$month <- lubridate::month(BC$incident_date)
#combine year and month column into a single column
BC$year_month <- as.yearmon(paste(BC$year, BC$month), "%Y %m") 

# Create a new dataframe for analysis via HWI grouped monthly
data <- aggregate(HWI ~ year_month + park, data = BC, FUN = "length")
data$year_month <- as.Date(data$year_month, format = "%Y-%m")
data$year <- lubridate::year(data$year_month)
data$month <- lubridate::month(data$year_month)
data <- relocate(data, HWI, .before = year_month)
data <- relocate(data, park, .after = month)

saveRDS(object = data, file = "Data/HWI/data.rds")