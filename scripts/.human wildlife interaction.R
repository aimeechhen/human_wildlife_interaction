# Human-wildlife interactions

#............................................................
# Load packages ----
#............................................................

#data, visualization
library(readr)
library(lubridate)       #date formats
library(zoo)             #date format 'year-month'
library(dplyr)           #data wrangling
library(tidyr)           #data wrangling
library(ggplot2)
library(khroma)          #colour blind friendly palette
library(viridis)
library(marginaleffects) #predictions()
library(gridExtra)       #arrange multi-plot
library(ggh4x)           #to fill in facet wrap title boxes
#model structure
library(mgcv)            #gam
library(MuMIn)           #AICc()
#remotes::install_github('burnett-m/climatenaR', build_vignettes = FALSE)
library(climatenaR)

#............................................................
# National Parks Data ----
#............................................................

# Set working directory
setwd("C:/Users/achhen/OneDrive - UBC/Github/human wildlife interaction")

# Download Parks Canada Agency dataset
#source: https://open.canada.ca/data/en/dataset/cc5ea139-c628-46dc-ac55-a5b3351b7fdf/resource/b2a9f7e4-7c49-471d-8337-0192c15dd52a?inner_span=True

# Import Parks Canada dataset
#data obtained on Feb. 3, 2023
PCA <- read_csv("data/pca-human-wildlife-coexistence-animals-involved-detailed-records-2010-2021.csv")

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

#............................................................
## Historical climate data ----
#............................................................

#import coordinates obtained from Google maps of the national parks
#refer to 'elevation and climate.R' for script
nationalparks_bc_coordinates <- read_csv("data/ClimateNA_v731/nationalparks_bc_coordinates.csv")
#adding park latitude and longitude coordinates to dataframe
data <- left_join(data, nationalparks_bc_coordinates, by = 'park')
#extract year from date then create a column for year, required for climate data

# Import cleaned historical climate data
#refer to "Historical climate data" section in 'elevation and climate.R' for script
historical_climate_data <- readRDS("data/ClimateNA_v731/climate data/historical_climate_data.rds")
data <- left_join(data, historical_climate_data, by = c("latitude","longitude", "year", "month"))

#number of total interactions
sum(data$HWI)
#number of months with recorded interactions
totalmonthlyHWI <- aggregate(HWI ~ year_month, data = BC, FUN = "length")

#............................................................
# Model ----
#............................................................

data$park <- as.factor(data$park)
data$year_month <- as.Date(data$year_month, format = "%Y-%m-%d")

model <- readRDS("RDS/model.RDS")

#............................................................
## Model distribution specification ----
#............................................................

#Poisson distribution
model <- gam(HWI ~
               s(park, bs = 'fs') +
               s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 5) +        #random intercept & slope effect of park level trend
               s(avgtemp, park, bs = 'fs', k = 5) +                           #global effect of temperature, specify k explicitly (even if it is the default)
               s(log(avgprecip + 1e-10), park, bs = 'fs', k = 5) +            #global effect of precipitation, add a tiny number to avgprecip so we don't take log of 0
               ti(avgtemp, log(avgprecip + 1e-10), park, bs = c('tp', 'tp', 'fs'), k = 5) + #response to snow
               ti(month, avgtemp, k = 5, bs = c('cc', 'tp')) +                 #what is hot in january is cold in july
               ti(month, log(avgprecip + 1e-10), bs = c('cc', 'tp'), k = 5),
             family = poisson(link = "log"),                                   #indicate distribution family, poisson because count data
             data = data,
             method = "REML",
             control = gam.control(nthreads = 8, trace = TRUE),
             knots = list(month = c(0.5, 12.5)))

#saveRDS(model, file = "RDS/model.RDS")

summary(model) #deviance 65.8%
par(mfrow = c(3,2))
plot(model)
par(mfrow = c(1,1))

# Negative binomial distribution
model2 <- gam(HWI ~
                s(park, bs = 're') +
                s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 5) +        #random intercept & slope effect of park level trend
                s(avgtemp, park, bs = 'fs', k = 10) +                           #global effect of temperature, specify k explicitly (even if it is the default)
                s(log(avgprecip + 1e-10), park, bs = 'fs', k = 10) +            #global effect of precipitation, add a tiny number to avgprecip so we don't take log of 0
                ti(avgtemp, log(avgprecip + 1e-10), park, bs = c('tp', 'tp', 're'), k = 5) + #response to snow
                ti(month, avgtemp, k = 5, bs = c('cc', 'tp')) +                 #what is hot in january is cold in july
                ti(month, log(avgprecip + 1e-10), bs = c('cc', 'tp'), k = 5),
              family = nb(link = "log"),                                    #indicate distribution family, negative binomial because count data
              data = data,
              method = "REML",
              control = gam.control(nthreads = 8, trace = TRUE))

#saveRDS(model2, file = "RDS/model2.RDS")

summary(model2) #deviance 62.2%
par(mfrow = c(3,2))
plot(model2)
par(mfrow = c(1,1))

#............................................................
## Model diagnostics ----
#............................................................

#comparing distributions
AICc(model, model2)
#Based on AICc model selection, a poisson distribution was a better fit

#diagnostics for a fitted gam model
par(mfrow = c(2,2))
gam.check(model)
gam.check(model2)
par(mfrow = c(1,1))

#dispersion check
#install.packages("countreg", repos="http://R-Forge.R-project.org")
library(countreg)
#https://stackoverflow.com/questions/59342595/how-to-check-for-overdispersion-in-a-gam-with-negative-binomial-distribution

#check for for over/under dispersion
#assessing the goodness of fit of model
root_pois <- rootogram(model, style = "hanging", plot = FALSE)
root_nb   <- rootogram(model2, style = "hanging", plot = FALSE)
#plot rootogram
autoplot(root_pois)
autoplot(root_nb)
#calculate the Pearson estimate for the dispersion parameter using the Pearson residuals of each model
#values should be 1
sum(residuals(model, type = "pearson")^2) / df.residual(model) #above 1 = overdispersion
sum(residuals(model2, type = "pearson")^2) / df.residual(model2) #above 1 = overdispersion

#tests for temporal autocorrelation in the residuals
model_acf <- acf(residuals(model, type = "deviance"))
model2_acf <- acf(residuals(model2, type = "deviance"))

#............................................................
# Model Fit ----
#............................................................

#model validation via historical prediction from the model to check to see if the model is 
#behaving appropriately based on the observed data by visually comparing predicted values 
#and the historical values for each park

#Create new dataframe for each park
group <- data %>% 
  group_by(park) %>%
  groups %>%
  unlist %>% 
  as.character

#Glacier National Park of Canada
park_glacier <- data[which(data$park == "Glacier National Park of Canada"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  slice_sample(n = 2) %>% 
  mutate(unique_id=1:NROW(.))
#create new dataframe
newdata_glacier <- data[which(data$park == "Glacier National Park of Canada"),] %>% 
  group_by(park)  %>% 
  right_join(park_glacier, by=group) %>%
  group_by_(group)

#Kootenay National Park of Canada
park_kootenay <- data[which(data$park == "Kootenay National Park of Canada"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  slice_sample(n = 2) %>% 
  mutate(unique_id=1:NROW(.))
newdata_kootenay <- data[which(data$park == "Kootenay National Park of Canada"),] %>% 
  group_by(park)  %>% 
  right_join(park_kootenay, by=group) %>%
  group_by_(group)

#Mount Revelstoke National Park of Canada
park_revelstoke <- data[which(data$park == "Mount Revelstoke National Park of Canada"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  slice_sample(n = 2) %>% 
  mutate(unique_id=1:NROW(.))
newdata_revelstoke <- data[which(data$park == "Mount Revelstoke National Park of Canada"),] %>% 
  group_by(park)  %>% 
  right_join(park_revelstoke, by=group) %>%
  group_by_(group)

#Pacific Rim National Park Reserve of Canada
park_pacific_rim <- data[which(data$park == "Pacific Rim National Park Reserve of Canada"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  slice_sample(n = 2) %>% 
  mutate(unique_id=1:NROW(.))
newdata_pacific_rim <- data[which(data$park == "Pacific Rim National Park Reserve of Canada"),] %>% 
  group_by(park)  %>% 
  right_join(park_pacific_rim, by=group) %>%
  group_by_(group)

#Yoho National Park of Canada
park_yoho <- data[which(data$park == "Yoho National Park of Canada"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  slice_sample(n = 2) %>% 
  mutate(unique_id=1:NROW(.))
newdata_yoho <- data[which(data$park == "Yoho National Park of Canada"),] %>% 
  group_by(park)  %>% 
  right_join(park_yoho, by=group) %>%
  group_by_(group)

#Generate predictions for each park
newdata_glacier$predict <- predict(model, newdata = newdata_glacier, type = "response")
newdata_kootenay$predict <- predict(model, newdata = newdata_kootenay, type = "response")
newdata_revelstoke$predict <- predict(model, newdata = newdata_revelstoke, type = "response")
newdata_pacific_rim$predict <- predict(model, newdata = newdata_pacific_rim, type = "response")
newdata_yoho$predict <- predict(model, newdata = newdata_yoho, type = "response")

#............................................................
## Visual comparison ----
#............................................................

#Plot to visually compare historical recordings vs. model-predicted interactions to make sure model is behaving

#Glacier
ggplot() +
  geom_boxplot(data = newdata_glacier, aes(x = month, y = HWI, group = cut_width(month, 1))) +
  geom_point(data = newdata_glacier, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "#66CCEE") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Number of Interaction (Monthly)") +
  ggtitle("Glacier National Park of Canada: Historical vs. model-predicted interaction recordings") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggsave(plot = last_plot(), filename = "figures/fitcheck_glacier.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#Kootenay
ggplot() +
  geom_boxplot(data = newdata_kootenay, aes(x = month, y = HWI, group = cut_width(month, 1))) +
  geom_point(data = newdata_kootenay, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "#EE7733") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Number of Interaction (Monthly)") +
  ggtitle("Kootenay National Park of Canada: Historical vs. model-predicted interaction recordings") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggsave(plot = last_plot(), filename = "figures/fitcheck_kootenay.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#Revelstoke
ggplot() +
  geom_boxplot(data = newdata_revelstoke, aes(x = month, y = HWI, group = cut_width(month, 1))) +
  geom_point(data = newdata_revelstoke, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "#228833") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Number of Interaction (Monthly)") +
  ggtitle("Mount Revelstoke National Park of Canada: Historical vs. model-predicted interaction recordings") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggsave(plot = last_plot(), filename = "figures/fitcheck_revelstoke.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#Pacific Rim
ggplot() +
  geom_boxplot(data = newdata_pacific_rim, aes(x = month, y = HWI, group = cut_width(month, 1))) +
  geom_point(data = newdata_pacific_rim, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "#004488") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Number of Interaction (Monthly)") +
  ggtitle("Pacific Rim National Park of Canada: Historical vs. model-predicted interaction recordings") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggsave(plot = last_plot(), filename = "figures/fitcheck_pacific_rim.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#Yoho
ggplot() +
  geom_boxplot(data = newdata_yoho, aes(x = month, y = HWI, group = cut_width(month, 1))) +
  geom_point(data = newdata_yoho, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "#AA4499") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Number of Interaction (Monthly)") +
  ggtitle("Yoho National Park of Canada: Historical vs. model-predicted interaction recordings") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggsave(plot = last_plot(), filename = "figures/fitcheck_yoho.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#............................................................
# HWI Projections ----
#............................................................

#Projecting HWI under climate change

#............................................................
## Projected climate data ----
#............................................................

#import clean projected climate data
#refer to "projected climate data" section within the 'elevation and climate.R' script
projection_climate_data <- readRDS("data/ClimateNA_v731/climate data/projection_climate_data_monthly.rds")
projection_climate_data <- merge(projection_climate_data, nationalparks_bc_coordinates,
                                 by = c("latitude","longitude"), all.x=TRUE)
names(projection_climate_data)[7] <- "avgtemp"
names(projection_climate_data)[6] <- "dec_year_month"

#............................................................
## HWI projections under 4 SSP climate scenarios ----
#............................................................

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

#............................................................
# Figures ----
#............................................................

colour_park <- c("#66CCEE", "#EE7733", "#228833", "#004488", "#AA4499")
#Glacier = light blue -> #66CCEE
#Kootenay = orange -> #EE7733
#Revelstoke = green -> #228833
#Pacific Rim = blue -> #004488
#Yoho = purple -> #AA4499

#............................................................
## National parks map  ----
#............................................................

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

#check locations
plot(bc_shape)
sp::plot(nationalparks_location, add = TRUE, col = 'red', pch = 19, cex = 0.5) 
#need sp:: in front of plot because function will try to use plot() from another package

map <-
ggplot() +
  geom_sf(data = bc_shape) +
  geom_point(data = data, aes(longitude, latitude, col = park),
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
ggsave(map, filename = "figures/map.png", device = NULL, path = NULL, scale = 1, width = 6, height = 6, units = "in", dpi = 600)

#............................................................
## Historical HWI recordings over time ----
#............................................................

ggplot() +
  geom_point(data = data, aes(x = year_month, y = HWI, col = park)) +
  xlab("Time") +
  ylab("Human-wildlife interactions") +
  scale_color_manual(name = "National Parks", values = colour_park,
                     labels = c("Glacier",
                                "Kootenay",
                                "Revelstoke",
                                "Pacific Rim",
                                "Yoho")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = 0.04, vjust = -7, size = 16, 
                                  family = "sans", face = "bold"),
        legend.position = c(0.15,0.8), #horizontal, vertical
        #legend.box.background = element_rect(color = "black"),
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggsave(last_plot(), filename = "figures/historical_HWI.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#............................................................
## Historical seasonal trends of human-wildlife interactions ----
#............................................................

ggplot() +
  geom_jitter(data = data, 
              aes(y = HWI, x = month, col = park),
              alpha = 0.5, size = 1, width = 0.25, shape = 16) + #point for every observation
  geom_smooth(data = data, 
              aes(y = HWI, x = month, col = park),
              linewidth = 0.6, se = F) + #line for each park
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(limits = c(0,130), expand = c(0,0.1)) +
  scale_color_manual(name = "National Parks", values = colour_park,
                     labels = c("Glacier",
                                "Kootenay",
                                "Revelstoke",
                                "Pacific Rim",
                                "Yoho")) +
  xlab("Month") +
  ylab("Human-wildlife interaction") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=7, family = "sans"),
        axis.text.x  = element_text(size=7, family = "sans"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 25, family = "sans", face = "bold",
                                  vjust = -5.5, hjust = 0.02),
        legend.position=c(0.1,0.8),
        legend.key = element_rect(color = NA),
        #legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0.2,0,0.2), "cm"))
ggsave(filename = "figures/historical_HWI_seasonal.png", plot = last_plot(), device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#.......................................................
# Final Results Figure 5 ----
#.......................................................

#Number of parks to loop over
PARKS <- levels(data$park)
N <- length(PARKS)

#............................................................
## Temperature: Figure 5a ----
#............................................................

# Projected monthly HWI in response to temperature

#Loop over the number of parks
for(i in 1:N){
  
  preds <- try(predictions(model,
                           datagrid(park = levels(data$park)[i],
                                    month = 1:12,
                                    avgtemp = seq(min(data$avgtemp),
                                                  max(data$avgtemp),
                                                  by = 0.1))))
  
  #Bind the results
  if(i == 1){temp_preds <- preds}
  if(i>1 && class(preds)[1] == "predictions"){temp_preds <- rbind(temp_preds, preds)}
  
  #Clear the intermediate variable
  rm(preds)
}

# Average temp effect for each park
temp_park_level <- aggregate(estimate ~ avgtemp + park, data = temp_preds, FUN = "mean")

# Average temp effect across all parks
temp_pop_level <- aggregate(estimate ~ avgtemp, data = temp_preds, FUN = "mean")

projected_HWI_temp <-
  ggplot() +
  geom_point(data = data, 
             aes(y = HWI, x = avgtemp, col = park),
             alpha = 0.4, shape = 16) + # points for each observation
  
  geom_line(data = temp_park_level,
            aes(x = avgtemp, estimate, col = park),
            alpha = 0.9, linewidth = 0.7) + # line for each region
  geom_line(data = temp_pop_level,
            aes(x = avgtemp, estimate),
            alpha = 1, col = "black", linewidth = 1.2) + # line for overall trend
  scale_x_continuous(breaks = seq(-15, 15, by = 5)) +
  scale_y_continuous(limits = c(0,130), expand = c(0,0.1)) +
  scale_color_manual(name = "National Parks", values = colour_park,
                     labels = c("Glacier",
                                "Kootenay",
                                "Revelstoke",
                                "Pacific Rim",
                                "Yoho")) + 
  xlab("Average Monthly Temperature (ºC)") +
  ylab("Total Monthly HWIs") +
  ggtitle("A") +
  guides(col = guide_legend(override.aes = list(alpha=1))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = 0.02, vjust = -10, size = 20, 
                                  family = "sans", face = "bold"),
        #plot.subtitle = element_text(size = 14, family = "sans", face = "bold"),
        legend.position = c(0.15,0.7),
        #legend.box.background = element_rect(color = "black"),
        legend.title = element_text(face = "bold"),
        legend.background=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(t = 0.2, r = 0.0, b = 0.2, l = 0.2), "cm"))
projected_HWI_temp
ggsave(projected_HWI_temp, filename = "figures/projected_HWI_temp.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#.......................................................
## Precipitation: Figure 5b ----
#.......................................................

# Projected monthly HWI in response to precipitation

#Loop over the number of parks
for(i in 1:N){
  
  
  preds <- try(predictions(model,
                           datagrid(park = levels(data$park)[i],
                                    month = 1:12,
                                    avgprecip = seq(min(data$avgprecip),
                                                    max(data$avgprecip),
                                                    by = 0.1))))
  
  #Bind the results
  if(i == 1){precip_preds <- preds}
  if(i>1 && class(preds)[1] == "predictions"){precip_preds <- rbind(precip_preds, preds)}
  
  #Clear the intermediate variable
  rm(preds)
}

# Average precip effect for each park
precip_park_level <- aggregate(estimate ~ avgprecip + park, data = precip_preds, FUN = "mean")

# Average precip effect across all parks
precip_pop_level <- aggregate(estimate ~ avgprecip, data = precip_preds, FUN = "mean")

projected_HWI_precip <-
  ggplot() +
  geom_point(data = data, 
             aes(y = HWI, x = avgprecip, col = park),
             alpha = 0.4, shape = 16) + # points for each observation
  geom_line(data = precip_park_level,
            aes(x = avgprecip, estimate, col = park),
            alpha = 0.9, linewidth = 0.7) + # line for each region
  geom_line(data = precip_pop_level,
            aes(x = avgprecip, estimate),
            alpha = 1, col = "black", linewidth = 1.2) + # line for overall trend
  scale_y_continuous(limits = c(0,130), expand = c(0,0.1)) +
  scale_color_manual(name = "National Parks", values = colour_park,
                     labels = c("Glacier",
                                "Kootenay",
                                "Revelstoke",
                                "Pacific Rim",
                                "Yoho")) + 
  xlab("Average Monthly Precipitation (mm)") +
  ylab("Total Monthly HWIs") +
  ggtitle("B") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = 0.02, vjust = -10, size = 20, 
                                  family = "sans", face = "bold"),
        legend.position="none",
        #legend.key = element_rect(color = NA),
        #legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(t = 0.2, r = 0.1, b = 0.2, l = 0.7), "cm"))
projected_HWI_precip

ggsave(projected_HWI_precip, filename = "figures/projected_HWI_precip.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#.......................................................
## Predicted HWI: Figure 5c-f  ----
#.......................................................

# Predicted HWIs under climate change scenarios

#Make colour strips in x-direction for panel title boxes (they will correspond to SSPs)
strip <- strip_themed(background_x = 
                        elem_list_rect(fill = c("#4EBAF9", "#C0DEED", "#FFC1B5", "#FF7B7B")))

#Assign a letter for each SSP so we can label each panel
data_text <- data.frame(label = c("C", "D", "E", "F"),
                        scenario = names(table(projection_climate_data$scenario)),
                        x = c(2023, 2023, 2023, 2023), 
                        y = c(3.5, 3.5, 3.5, 3.5))

BOT <- 
  ggplot(agg_proj, aes(x = year , y = predicted_interactions)) + 
  geom_hline(yintercept = 1, linewidth = 0.5, color = "grey70") + # line at 1 for reference
  geom_line(linewidth=0.5, aes(group = park, col = park), alpha = 0.8) + # line for each park
  facet_wrap2(~ scenario, strip = strip, labeller = as_labeller(c(
    `ssp126_low` = "SSP 1-2.6",
    `ssp245_intermediate` = "SSP 2-4.5",
    `ssp370_high` = "SSP 3-7.0",
    `ssp585_veryhigh` = "SSP 5-8.5")),
    nrow = 1) + # create a panel for each climate change scenario
  geom_text(data = data_text,
            mapping = aes(x = x,
                          y = y,
                          label = label),
            check_overlap = TRUE,
            size = 8, fontface = "bold") +
  xlab("Year") +
  ylab("Relative change in annual HWIs") +
  scale_y_continuous(limits = c(0.75,3.6)) +
  scale_x_continuous(limits = c(2020,2104), expand = c(0,1)) +
  guides(col = guide_legend(override.aes = list(alpha=1))) +
  scale_color_manual(name = "National Parks", values = colour_park,
                     labels = c("Glacier",
                                "Kootenay",
                                "Revelstoke",
                                "Pacific Rim",
                                "Yoho")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(vjust = -5, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position="none",
        # legend.key.size = unit(0.4, 'cm'),
        # legend.title = element_text(size = 8, face = "bold"),
        # legend.text=element_text(size=7),
        # legend.box.background = element_rect(color = "black"),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
BOT 
ggsave(BOT, filename = "figures/BOT.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 3, units = "in", dpi = 600)

#..................................................
## Multi-panel ----
#..................................................

TOP <- grid.arrange(projected_HWI_temp, projected_HWI_precip, 
                    ncol=2)

FIG5 <- grid.arrange(TOP, BOT, 
                     ncol=1, heights = c(1,0.7))

ggsave(FIG5, filename = "figures/figure5.png", device = NULL, path = NULL, scale = 1, width = 14.91, height = 10.47, units = "in", dpi = 600)


