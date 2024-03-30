
library(rworldmap)
library(terra)
library(sf)
library(geodata)

canada <- gadm(country="Canada", level=0, path = tempdir())
provinces <- gadm(country="Canada", level=1, path = tempdir())


#define the projection system
crs_wintri <- "ESRI:53018"  

#Get world boundaries for clipping (not critical, but makes maps nicer looking)
world_sf <- st_as_sf(rworldmap::getMap(resolution = "low"))

#Drop antarctica
world_sf <- subset(world_sf, continent != "Antarctica")

#Reproject
world_wintri <- lwgeom::st_transform_proj(world_sf, crs = crs_wintri)
canada_wintri <- project(canada, crs_wintri)
provinces_wintri <- project(provinces, crs_wintri)

# Convert to spatvector class
world_wintri <- vect(world_wintri) 


png(file="Figures/world_bc_map.png",
    width=6.86, height=3, units="in", res=600)
#World baselayer
plot(world_wintri, border = "darkgrey", col = "lightgrey", axes = FALSE)
#add canada layer
plot(canada_wintri, col="white", lwd = 0.5, add = TRUE)
#add BC layer
plot(provinces_wintri[provinces_wintri$NAME_1 %in% "British Columbia", ], border="black", lwd = 0.5,
     col="#de2d26", add=TRUE)
dev.off()


png(file="Figures/world_bc_map_provinces.png",
    width=6.86, height=3, units="in", res=600)
#World baselayer
plot(world_wintri, border = "darkgrey", col = "lightgrey", axes = FALSE)
#add Canada layer with provinces
plot(provinces_wintri, col="white", lwd = 0.5, add = TRUE)
#add BC layer
plot(provinces_wintri[provinces_wintri$NAME_1 %in% "British Columbia", ], border="black", lwd = 0.5,
     col="#de2d26", add=TRUE)
dev.off()