#Elevation and climate data

# ClimatenaR package requires elevation data to obtain climate data. 
# Coordinates of the national parks were obtained from Google maps and then saved in a csv file.
library(readr)
library(dplyr)        # for data wrangling
library(canadianmaps) # to download a shapefile of BC
library(elevatr)      # to download digital elevation models
library(purrr)        # for functional programming (map_***(), etc.)
library(sp)           # for spatial data, SpatialPoints()
library(sf)           # for spatial data
library(terra)        # for raster data
library(progress)     # for elevation, get_elev_raster()
library(ggplot2)

setwd("C:/Users/achhen/OneDrive - UBC/Github/human wildlife interaction")

#import a shapefile of British Columbia
bc_shape <- st_as_sf(PROV) %>%  # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

#import coordinates obtained from Google maps of the national parks
nationalparks_bc_coordinates <- read_csv("data/ClimateNA_v731/nationalparks_bc_coordinates.csv")

#convert all telemetry dataset to spatial data points
nationalparks_location <- SpatialPoints(select(nationalparks_bc_coordinates, longitude, latitude))

#do not load the the ctmm package for this or you will run into errors
ctmm::projection(nationalparks_location) <- '+proj=longlat' 

#check locations
plot(bc_shape)
sp::plot(nationalparks_location, add = TRUE, col = 'red', pch = 19, cex = 0.5) 
#need sp:: in front of plot because function will try to use plot() from another package

# Elevation data ----
#obtain elevation data required for climate data
#import a Digital Elevation Model (DEM) for the region(s) of interest
dem <- get_elev_raster(locations = nationalparks_location,
                       z = 3,
                       clip = 'bbox',
                       expand = 0.1)

plot(dem)

# write the csv
#(circumventing climatenaR's functions because we only need a specific number of locations)
nationalparks_bc_coordinates <- mutate(nationalparks_bc_coordinates,
                                       el = terra::extract(dem, nationalparks_location),
                                       ID1 = 1:n(),
                                       ID2 = ID1) %>%
  relocate(ID1:ID2)
#save elevation file into 'ClimateNA_v731' folder for downloading climate data (next step)
write.csv(nationalparks_bc_coordinates, file = 'data/ClimateNA_v731/nationalparks_bc_dem.csv', row.names = FALSE) 

# check the csv
nationalparks_bc_dem <- read.csv('data/ClimateNA_v731/nationalparks_bc_dem.csv') %>%
  head()

##plot coordinates for visual ----
ggplot(nationalparks_bc_dem, aes(latitude, longitude, el)) +
  geom_tile()

# Historical climate data ----
# climatenaR package requires the ClimateNA application (https://climatena.ca/) for the package to work.
# Registration is required to download the ClimateNA application (https://register.climatena.ca/).
# Download the ClimateNA application (https://register.climatena.ca/Home/Download) and extract the zip folder.
# For help, refer to: https://rdrr.io/github/Silva21-irss/silvR21/f/vignettes/silvR21_vignette.Rmd
# For historical climate data help, refer to: https://rdrr.io/github/Silva21-irss/silvR21/src/R/histAnnualClimateNA.R
# For more information, refer to: https://rdrr.io/github/Silva21-irss/silvR21/f/
#   Please note: ClimateNA does not provide daily climate information.
# If necessary, install the `climatenaR` package with `remotes::install_github('burnett-m/climatenaR', build_vignettes = TRUE)`.
 
remotes::install_github('burnett-m/climatenaR', build_vignettes = TRUE)
library(climatenaR)
# download historical climate data
for(y in 2010:2021) {
  cat('Downloading ', y, '...\n', sep = '') # to track progress
  histClimateNA(
    file = '/data/ClimateNA_v731/nationalparks_bc_dem.csv', #elevation file, error, remove "park" column in the csv to fix
    dateR = as.character(y),
    tFrame = 'M', # monthly averages
    exe = '/data/ClimateNA_v731/ClimateNA_v7.31.exe',
    outdir = '/data/ClimateNA_v731/climate data/historical climate data') #create a output file where the data will be saved to within the 'ClimateNA_v731' folder
}

## Clean historical climate data ----
library(tidyverse)
library(stringi)
library(purrr)
historical_climate_data <-
  # list all files, and import each of the CSVs
  map_dfr(
    list.files('data/ClimateNA_v731/climate data/historical climate data', full.names = TRUE), #folder where the downloaded CSV of historical data is located
    \(.fname) {
      readr::read_csv(.fname, col_types = '?') %>%
        # add a column of the file name
        mutate(file = .fname)
    }) %>%
  mutate(year = substr(file,
                       start = stri_locate_first(file, regex = 'dem_')[1] + 4,
                       stop = nchar(file) - nchar('.csv'))) %>%
  # only keep relevant columns 
  select(year, Latitude, Longitude, Elevation, Tave01, Tave02, Tave03,
         Tave04, Tave05, Tave06, Tave07, Tave08, Tave09, Tave10, Tave11, Tave12,
         PPT01, PPT02, PPT03, PPT04, PPT05, PPT06, PPT07, PPT08, PPT09, PPT10,
         PPT11, PPT12) %>%
  # pivot from wide to long format (only one column of precip and temp)
  pivot_longer(-c(year, Latitude, Longitude, Elevation),
               names_to = 'parameter', values_to = 'value') %>%
  # extract month and year out of parameters
  mutate(month = map_chr(parameter,
                         \(.chr) substr(.chr, nchar(.chr) - 1, nchar(.chr))),
         dec_date = decimal_date(date(paste(year, month, '15', sep = '-'))),
         month = as.numeric(month),
         year = as.numeric(year),
         parameter = map_chr(parameter,
                             \(.chr) substr(.chr, 1, nchar(.chr) - 2))) %>%
  # pivot wider to make separate columns of temperature and precipitation
  pivot_wider(names_from = parameter, values_from = value) %>%
  # convert monthly total precipitation to average daily precipitation
  mutate(first_day = as.Date(paste(year, month, '01', sep = '-')),
         next_month = if_else(month != '12', as.numeric(month + 1), 1),
         next_year = if_else(month != '12', year, year + 1),
         last_day = as.Date(paste(next_year, next_month, '01', sep = '-')),
         samples = as.numeric((last_day - first_day)),
         avgprecip = PPT / samples) %>% # convert to millimeters per day
  # drop temporary columns
  select(-c(first_day, next_month, next_year, last_day, samples, PPT)) %>%
  # change to names used in the models
  rename(avgtemp = Tave,
         latitude = Latitude,
         longitude = Longitude,
         elevation = Elevation) %>%
  relocate(c(month, dec_date), .after = year)
saveRDS(historical_climate_data, file = "data/ClimateNA_v731/climate data/historical_climate_data.rds")

# Projected climate data ----
# download projected climate data
for(y in 2022:2100) {
  cat('Downloading ', y, '...\n', sep = '') # to track progress
  projClimateNA(
    file = '/data/ClimateNA_v731/nationalparks_bc_dem.csv', #elevation file, error, remove "park" column in the csv to fix
    tFrame = 'M', # monthly averages
    exe = '/data/ClimateNA_v731/ClimateNA_v7.31.exe',
    scen = '8GCM', # 8GCMs_ensemble General Circulation Model
    ssp = c('S1', 'S2', 'S3', 'S5'), # Shared Socioeconomic Pathway scenarios (ie. climate change scenarios)
    years = as.character(y)) # can only extract data for two decades as a time
} #this will save into another folder called 'nationalparks_bc_dem' (whatever your input.csv file's name is)
#renamed folder to "projected climate data"

## Clean climate data projections ----
projection_climate_data <-
  # list all files, and import each of the CSVs
  map_dfr(
    list.files(
      'data/ClimateNA_v731/climate data/projected climate data',  #folder where the downloaded climate projection data is located
      full.names = TRUE,
      pattern = '@'),
    \(.fname) {
      readr::read_csv(.fname, col_types = '?') %>%
        # add a column of the file name
        mutate(file = .fname)
    }) %>%
  # extract scenario and year from file name column
  mutate(scenario =
           substr(file,
                  start = stri_locate_last(file, regex = '/')[1] + 1,
                  stop = stri_locate_first(file, regex = '@')[1] - 1),
         year = substr(file,
                       start = stri_locate_first(file, regex = '@')[1] + 1,
                       stop = nchar(file) - nchar('.csv'))) %>%
  # only keep relevant columns
  select(scenario, year, Latitude, Longitude, Elevation, Tave01, Tave02,
         Tave03, Tave04, Tave05, Tave06, Tave07, Tave08, Tave09, Tave10,
         Tave11, Tave12, PPT01, PPT02, PPT03, PPT04, PPT05, PPT06, PPT07,
         PPT08, PPT09, PPT10, PPT11, PPT12) %>%
  # pivot from wide to long format (only one column of precip and temp)
  pivot_longer(-c(scenario, year, Latitude, Longitude, Elevation),
               names_to = 'parameter', values_to = 'value') %>%
  # extract month and year out of parameters
  mutate(month = map_chr(parameter,
                         \(.chr) substr(.chr, nchar(.chr) - 1, nchar(.chr))),
         dec_date = decimal_date(date(paste(year, month, '15', sep = '-'))),
         month = as.numeric(month),
         year = as.numeric(year),
         parameter = map_chr(parameter,
                             \(.chr) substr(.chr, 1, nchar(.chr) - 2))) %>%
  # pivot wider to make separate columns of temperature and precipitation
  pivot_wider(names_from = parameter, values_from = value) %>%
  # convert monthly total precipitation to average daily precipitation
  mutate(first_day = as.Date(paste(year, month, '01', sep = '-')),
         next_month = if_else(month != '12', as.numeric(month + 1), 1),
         next_year = if_else(month != '12', year, year + 1),
         last_day = as.Date(paste(next_year, next_month, '01', sep = '-')),
         samples = as.numeric((last_day - first_day)),
         avgprecip = PPT / samples) %>% # convert to millimeters per day
  # change to names used in the models
  rename(temperature = Tave,
         tot_precip = PPT,
         latitude = Latitude,
         longitude = Longitude,
         elevation = Elevation,
         ssp = scenario) %>%
  # remove unnecessary columns from dataset
  select(-c(elevation, first_day, last_day, next_month, next_year, samples, tot_precip)) %>%
  # move month column to after the year one
  relocate(month, .after = year)
names(projection_climate_data)[1] <- "scenario"
projection_climate_data$scenario <- recode_factor(projection_climate_data$scenario,
                                                  "8GCMs_ensemble_ssp126" = "ssp126_low", 
                                                  "8GCMs_ensemble_ssp245" = "ssp245_intermediate",
                                                  "8GCMs_ensemble_ssp370" = "ssp370_high",
                                                  "8GCMs_ensemble_ssp585" = "ssp585_veryhigh")
saveRDS(projection_climate_data, file = "data/ClimateNA_v731/climate data/projection_climate_data_monthly.rds")


