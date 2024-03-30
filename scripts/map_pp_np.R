library('dplyr')        # for data wrangling
library('sf')           # for working with spatial data
library('sp')           # for working with spatial data
library('canadianmaps') # to download a shapefile of BC
library('ggplot2')      # for fancy plots
library('khroma')       # for colour palettes
theme_set(theme_map())


# import a shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only




#...........................................................................

# Import BC Parks data
bcparks = readRDS("Data/Attendance/Park Data/bcparks.rds")
parks <- read.csv('Data/Attendance/Park Data/park_coordinates.csv')
# Import coordinates obtained from Google maps of the National Parks in BC
nationalparks_bc_coordinates <- read.csv("Data/HWI/ClimateNA_v731/nationalparks_bc_coordinates.csv")

# Remove parks with no data (were not used)
parks <- merge(bcparks, parks, by=c("park", "latitude", "longitude", "region"))
parks <- na.omit(parks)

# Filter data set such that each park only has one observation 
parks <- parks %>% distinct(park, latitude, longitude, .keep_all = TRUE)

# Subset to "park", "latitude", "longitude", "region"
parks_coordinates <- parks[,c("park", "latitude", "longitude", "region")]
parks_coordinates$management <- "provincial"

# Add column for region and add National Park coordinates to BC Park dataframe
nationalparks_bc_coordinates$region <- "np"
nationalparks_bc_coordinates$management <- "federal"
all_parks <- rbind(parks_coordinates, nationalparks_bc_coordinates)



#...........................................................................


# Define manual color scale due to adding national parks
manual_colors <- c("north" = "#CC6677", "ok" = "#332288", "south" = "#DDCC77", 
                   "tc" = "#117733", "west" = "#88CCEE", "np" = "black")

# Define the desired order of levels for the legend
legend_order <- c("north", "ok", "south", "tc", "west", "np")

# Reorder levels of region variable
all_parks$region <- factor(all_parks$region, levels = legend_order)


map <-
  ggplot() +
  ggtitle("A")+
  geom_sf(data = bc_shp) +
  geom_point(aes(longitude, latitude, col = region, shape = management, alpha = management), all_parks, 
             size = 3 ) +
  guides(col = guide_legend(override.aes = list(alpha=1,
                                                shape = c(17, 17 ,17 , 17 , 17, 18))), 
         shape = "none", alpha = "none") +
  scale_colour_manual(name="Region",
                     values = manual_colors,
                     labels=c('Northern',
                              'Kootenay-Okanagan',
                              'South Coast',
                              'Thompson-Cariboo',
                              'West Coast',
                              'National Parks')) +
    scale_shape_manual(values = c(18, 17)) + 
    scale_alpha_manual(values = c(0.8,0.6)) + 
  theme(legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.box.background = element_rect(color = "black"),
        plot.margin = unit(c(-1,0,-1,0), "cm"),
        plot.title = element_text(vjust = -8.5, hjust = 0.03,
                                  size = 30, family = "sans", face = "bold")) +
  coord_sf() # ensures points don't get jittered around when figure dimensions change
  