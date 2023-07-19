






#............................................................
# Projecting HWI under climate change ----
#............................................................

#import clean projected climate data
#refer to "projected climate data" section within the 'elevation and climate.R' script
projection_climate_data <- readRDS("data/ClimateNA_v731/climate data/projection_climate_data_monthly.rds")
projection_climate_data <- merge(projection_climate_data, nationalparks_bc_coordinates,
                                 by = c("latitude","longitude"), all.x=TRUE)
names(projection_climate_data)[7] <- "avgtemp"
names(projection_climate_data)[6] <- "dec_year_month"

#Project the number of interactions under 4 SSP climate scenarios
projection_climate_data$predicted_interactions <- predict(model, newdata = projection_climate_data, type = "response")

#Calculate sum of interactions in each year then scale relative to 2020
agg_proj <- aggregate(predicted_interactions ~ year + park + scenario , data = projection_climate_data, FUN = "sum")

data2 <- agg_proj[which(agg_proj$year == 2022),]
data2 <- aggregate(predicted_interactions ~ park, data = data2, FUN = "median")

#Kootenay
agg_proj[which(agg_proj$park == "Kootenay National Park of Canada"),"predicted_interactions"] <- 
  agg_proj[which(agg_proj$park == "Kootenay National Park of Canada"),"predicted_interactions"]/data2[which(data2$park == "Kootenay National Park of Canada"),"predicted_interactions"]

#Pacific Rim
agg_proj[which(agg_proj$park == "Pacific Rim National Park Reserve of Canada"),"predicted_interactions"] <- 
  agg_proj[which(agg_proj$park == "Pacific Rim National Park Reserve of Canada"),"predicted_interactions"]/data2[which(data2$park == "Pacific Rim National Park Reserve of Canada"),"predicted_interactions"]

#Glacier
agg_proj[which(agg_proj$park == "Glacier National Park of Canada"),"predicted_interactions"] <- 
  agg_proj[which(agg_proj$park == "Glacier National Park of Canada"),"predicted_interactions"]/data2[which(data2$park == "Glacier National Park of Canada"),"predicted_interactions"]

#Yoho
agg_proj[which(agg_proj$park == "Yoho National Park of Canada"),"predicted_interactions"] <- 
  agg_proj[which(agg_proj$park == "Yoho National Park of Canada"),"predicted_interactions"]/data2[which(data2$park == "Yoho National Park of Canada"),"predicted_interactions"]

#Revelstoke
agg_proj[which(agg_proj$park == "Mount Revelstoke National Park of Canada"),"predicted_interactions"] <- 
  agg_proj[which(agg_proj$park == "Mount Revelstoke National Park of Canada"),"predicted_interactions"]/data2[which(data2$park == "Mount Revelstoke National Park of Canada"),"predicted_interactions"]
