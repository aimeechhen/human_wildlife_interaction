
#............................................................
## National parks map  ----
#............................................................

library(dplyr)           #data wrangling
library(canadianmaps)    #to download a shapefile of BC
library(sf)              #for spatial data
library(sp)              #for spatial data, SpatialPoints()
library(ggplot2)

#import a shapefile of British Columbia
bc_shape <- 
  st_as_sf(PROV) %>%  # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

# Import coordinates obtained from Google maps of the national parks
nationalparks_bc_coordinates <- read.csv("Data/HWI/ClimateNA_v731/nationalparks_bc_coordinates.csv")

# Convert all telemetry dataset to spatial data points
nationalparks_location <- SpatialPoints(select(nationalparks_bc_coordinates, longitude, latitude))

#do not load the the ctmm package for this or you will run into errors
ctmm::projection(nationalparks_location) <- '+proj=longlat' 

#plot shape file
plot(bc_shape)

#basic visualization, check locations
sp::plot(nationalparks_location, add = TRUE, col = 'red', pch = 19, cex = 0.5)
#need sp:: in front of plot because function will try to use plot() from another package

#assign colours to each park, label in this order when plotting for the colours to correspond with the parks correctly
#Glacier: light blue; Kootenay: orange; Revelstoke: green; Pacific Rim: dark blue; Yoho: purple
colour_park <- c("#66CCEE", "#EE7733", "#228833", "#004488", "#AA4499")

map <-
  ggplot() +
  geom_sf(data = bc_shape) +
  geom_point(data = nationalparks_bc_coordinates, 
             aes(longitude, latitude, col = park),
             size = 3, shape = 17, alpha = 0.6) +
  guides(col = guide_legend(override.aes = list(alpha=1))) +
  scale_color_manual(name = "National Parks", values = colour_park,
                     labels = c("Glacier",
                                "Kootenay",
                                "Revelstoke",
                                "Pacific Rim",
                                "Yoho")) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.15, 0.2), #horizontal, vertical
        legend.background=element_blank(),
        plot.margin = unit(c(-1,0,-1,0), "cm"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),) +
  coord_sf() # ensures points don't get jittered around when figure dimensions change
map
ggsave(map, 
       filename = "figures/map.png", 
       device = NULL, path = NULL, scale = 1, width = 6, height = 6, units = "in", dpi = 600)



#map <-
  ggplot() +
  geom_sf(data = bc_shape) +
  geom_point(data = nationalparks_bc_coordinates, aes(longitude, latitude), 
             col = 'black', size = 3, shape = 16) +
  #guides(col = guide_legend(override.aes = list(alpha=1))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.15, 0.2), #horizontal, vertical
        legend.background=element_blank(),
        plot.margin = unit(c(-1,0,-1,0), "cm"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),) +
  coord_sf() # ensures points don't get jittered around when figure dimensions change
map