
#............................................................
# Shapefile of Canada ----
#............................................................

library(readr)
library(dplyr)           #data wrangling
library(canadianmaps)    #to download a shapefile of BC
library(sf)              #for spatial data
library(sp)              #for spatial data, SpatialPoints()

#import a shapefile of British Columbia
bc_shape <- 
  st_as_sf(PROV) %>%  # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

#import coordinates obtained from Google maps of the national parks
nationalparks_bc_coordinates <- read_csv("data/ClimateNA_v731/nationalparks_bc_coordinates.csv")

#convert all telemetry dataset to spatial data points
nationalparks_location <- SpatialPoints(select(nationalparks_bc_coordinates, longitude, latitude))

#do not load the the ctmm package for this or you will run into errors
ctmm::projection(nationalparks_location) <- '+proj=longlat' 

#plot shape file
plot(bc_shape)

#basic visualization, check locations
sp::plot(nationalparks_location, add = TRUE, col = 'red', pch = 19, cex = 0.5)
#need sp:: in front of plot because function will try to use plot() from another package