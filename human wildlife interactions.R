# Human-wildlife interactions

# Load packages ----
#data, visualization
library(readr)
library(lubridate)       #date formats
library(zoo)             #date format 'year-month'
library(dplyr)           #data wrangling
library(tidyr)           #data wrangling
library(ggplot2)
library(khroma)          #colour blind friendly palette
#model structure
library(mgcv)            #gam
library(MuMIn)           #AICc()
library(climatenaR)





# Data Preparation ----

# Set working directory
setwd("C:/Users/achhen/OneDrive - UBC/Github/human wildlife interaction")

## Parks Canada Agency (PCA) data ----

# Download Parks Canada Agency data set
#source: https://open.canada.ca/data/en/dataset/cc5ea139-c628-46dc-ac55-a5b3351b7fdf/resource/b2a9f7e4-7c49-471d-8337-0192c15dd52a?inner_span=True

# Import Parks Canada dataset
#data obtained on Feb. 3, 2023
PCA <- read_csv("data/pca-human-wildlife-coexistence-animals-involved-detailed-records-2010-2021.csv")

#There is a total of 35 National Parks listed within the dataset. Our project is interested in the National Parks located within British Columbia (BC).
#There is a total of 9 incident types assigned in the dataset. Our project is interested in the 'Human Wildlife Interaction'.

#data carpentry
BC <- PCA[which(PCA$`Protected Heritage Area` %in% c("Glacier National Park of Canada",
                                                     "Kootenay National Park of Canada",
                                                     "Mount Revelstoke National Park of Canada",
                                                     "Pacific Rim National Park Reserve of Canada",
                                                     "Yoho National Park of Canada")),]
BC <- BC[which(BC$`Incident Type` == "Human Wildlife Interaction"),]

#rename column names to make it easier for coding
names(BC)[2] <- "incident_date"
names(BC)[4] <- "park"
names(BC)[5] <- "HWI"
names(BC)[6] <- "species"

#clean BC National Parks data, removing unknown species
BC <- BC[BC$species != "None",]
BC <- BC[BC$species != "Unknown",]
BC <- BC[BC$species != "Unknown bat",]
BC <- BC[BC$species != "Unknown bear",]
BC <- BC[BC$species != "Unknown bird",]
BC <- BC[BC$species != "Unknown deer",]
BC <- BC[BC$species != "Unknown sea lion",]

# Data preparation for climate data
#Note: climate data is in monthly intervals
BC$year <- lubridate::year(BC$incident_date)
#extract month from date then create a column for month, required for climate data
BC$month <- lubridate::month(BC$incident_date)
#combine year and month column into a single column
BC$year_month <- as.yearmon(paste(BC$year, BC$month), "%Y %m") 

# Create a new dataframe for analysis via subset the dataset grouped monthly
data <- aggregate(HWI ~ year_month + park, data = BC, FUN = "length")
data$year_month <- as.Date(data$year_month, format = "%Y-%m")
data <- relocate(data, HWI, .before = year_month)

## Historical climate data ----

#import coordinates obtained from Google maps of the national parks
#refer to 'elevation and climate.R' for script
nationalparks_bc_coordinates <- read_csv("data/ClimateNA_v731/nationalparks_bc_coordinates.csv")
#adding park latitude and longitude coordinates to dataframe
data <- left_join(data, nationalparks_bc_coordinates, by = 'park')
data$year <- lubridate::year(data$year_month)
#extract month from date then create a column for month, required for climate data
data$month <- lubridate::month(data$year_month)

# Import cleaned historical climate data
#refer to "Historical climate data" section in 'elevation and climate.R' for script
historical_climate_data <- readRDS("data/ClimateNA_v731/climate data/historical_climate_data.rds")
data <- left_join(data, historical_climate_data, by = c("latitude","longitude", "year", "month"))
#data <- relocate(data, dec_date, .after = month)

# Visualization ----

## Plot historical number of interaction recordings over time ----
plot(data$HWI ~ data$year_month, xlab = "time (monthly)", ylab = "number of interactions")

## Plot historical seasonal trends of human-wildlife interactions ----
#Glacier = light blue -> #66CCEE
#Kootenay = orange -> #EE7733
#Revelstoke = green -> #228833
#Pacific Rim = blue -> #004488
#Yoho = purple -> #AA4499
colour_park <- c("#66CCEE", "#EE7733", "#228833", "#004488", "#AA4499")

ggplot() +
  geom_jitter(data = data, 
              aes(y = HWI, x = month, col = park),
              alpha = 0.5, size = 1, width = 0.25, shape = 17) + #point for every observation
  geom_smooth(data = data, 
              aes(y = HWI, x = month, col = park),
              linewidth = 0.6, se = F) + #line for each park
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(limits = c(0, 10)) +
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
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0.2,0,0.2), "cm"))
ggsave(filename = "figures/seasonal_HWI.png", plot = last_plot(), device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)


# Model Structure ----

data$park <- as.factor(data$park)
data$species <- as.factor(data$species)
data$year_month <- as.Date(data$year_month, format = "%Y-%m-%d")
data$dec_year_month <- decimal_date(data$year_month)

## Distribution Specification ----
### Poisson distribution ----
model <- gam(HWI ~
               #s(dec_year_month, k = 3) +                #global effect of time
               s(avgtemp) +                               #global effect of temperature
               s(log(avgprecip + 1e-10)) +                #global effect of precipitation, add a tiny number to avgprecip so we don't take log of 0
               avgtemp:log(avgprecip + 1e-10) +
               month:avgtemp +
               month:log(avgprecip + 1e-10) +
               s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 5),     #random intercept & slope effect of park level trend
             family = poisson(link = "log"),              #indicate distribution family, poisson because count data
             data = data,
             method = "REML",
             control = gam.control(nthreads = 8, trace = TRUE),
             knots = list(month = c(0.5, 12.5)))
saveRDS(model, file = "RDS/HWI_model.RDS")
model <- readRDS("RDS/HWI_model.RDS")
summary(model)

### Negative binomial distribution ----
nb_full <- gam(HWI ~
                 s(dec_year_month) +                     #global effect of time
                 s(avgtemp) +                               #global effect of temperature
                 s(log(avgprecip + 1e-10)) +                #global effect of precipitation, add a tiny number to avgprecip so we don't take log of 0
                 avgtemp:log(avgprecip + 1e-10) +
                 month:avgtemp +
                 month:log(avgprecip + 1e-10) +
                 s(dec_year_month, park, bs = "fs") +    #random intercept & slope effect of park level trend
                 s(dec_year_month, species, bs = "fs"),  #random intercept & slope effect of species level trend
               family = nb(link = "log"),                   #indicate distribution family, negative binomial because count data
               data = data,
               method = "REML",
               control = gam.control(nthreads = 8, trace = TRUE))
saveRDS(nb_full, file = "RDS/nb_full.RDS")
nb_full <- readRDS("RDS/nb_full.RDS")
summary(nb_full)

### Comparing distributions ----
AICc(poisson_full,nb_full)
#Based on AICc model selection, a poisson distribution was a better fit

#### Model diagnostics ----

#diagnostics for a fitted gam model
par(mfrow = c(2,2))
gam.check(poisson_full)
gam.check(nb_full)
par(mfrow = c(1,1))

#### Dispersion check ----
#check for for over/under dispersion
#https://stackoverflow.com/questions/59342595/how-to-check-for-overdispersion-in-a-gam-with-negative-binomial-distribution
#install.packages("countreg", repos="http://R-Forge.R-project.org")
library(countreg)
#assessing the goodness of fit of model
root_pois <- rootogram(poisson_full, style = "hanging", plot = FALSE)
root_nb   <- rootogram(nb_full, style = "hanging", plot = FALSE)
#plot rootogram
autoplot(root_pois)
autoplot(root_nb)
#calculate the Pearson estimate for the dispersion parameter using the Pearson residuals of each model
#values should be 1
#above = overdispersion
#under = underdispersion
sum(residuals(poisson_full, type = "pearson")^2) / df.residual(poisson_full)
sum(residuals(nb_full, type = "pearson")^2) / df.residual(nb_full)
#both are ~0.407

## Autocorrelation ----
#tests for temporal autocorrelation in the residuals
poisson_acf <- acf(residuals(poisson_full, type = "deviance"))
nb_acf <- acf(residuals(nb_full, type = "deviance"))

# Model Validation ----

# Validation via historical prediction from the model
#check to see if the model is behaving appropriately based on the observed data
#by visually comparing predicted values and the historical values for each park

##Create new dataframe for each park ----
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

## Generate predictions for each park ----
newdata_glacier$predict <- predict(poisson_nospecies, newdata = newdata_glacier, type = "response")
newdata_kootenay$predict <- predict(poisson_nospecies, newdata = newdata_kootenay, type = "response")
newdata_revelstoke$predict <- predict(poisson_nospecies, newdata = newdata_revelstoke, type = "response")
newdata_pacific_rim$predict <- predict(poisson_nospecies, newdata = newdata_pacific_rim, type = "response")
newdata_yoho$predict <- predict(poisson_nospecies, newdata = newdata_yoho, type = "response")



##Plot to visually compare historical recordings vs. model-predicted interactions to make sure model is behaving ----
library(viridis)
par(mfrow = c(1,1))

#Glacier
ggplot() +
  geom_boxplot(data = newdata_glacier, aes(x = month, y = HWI, group = cut_width(month, 1))) +
  geom_point(data = newdata_glacier, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "#66CCEE") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Number of Interaction (Monthly)") +
  ggtitle("Glacier National Park of Canada: Historical vs. model-predicted interaction recordings") +
  # facet_wrap(~ unique_id, labeller = as_labeller(c(
  #   `1` = "Random park 1",
  #   `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Kootenay
ggplot() +
  geom_boxplot(data = newdata_kootenay, aes(x = month, y = HWI, group = cut_width(month, 1))) +
  geom_point(data = newdata_kootenay, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "#EE7733") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Number of Interaction (Monthly)") +
  ggtitle("Kootenay National Park of Canada: Historical vs. model-predicted interaction recordings") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Revelstoke
ggplot() +
  geom_boxplot(data = newdata_revelstoke, aes(x = month, y = HWI, group = cut_width(month, 1))) +
  geom_point(data = newdata_revelstoke, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "#228833") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Number of Interaction (Monthly)") +
  ggtitle("Mount Revelstoke National Park of Canada: Historical vs. model-predicted interaction recordings") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Pacific Rim
ggplot() +
  geom_boxplot(data = newdata_pacific_rim, aes(x = month, y = HWI, group = cut_width(month, 1))) +
  geom_point(data = newdata_pacific_rim, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "#004488") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Number of Interaction (Monthly)") +
  ggtitle("Mount Revelstoke National Park of Canada: Historical vs. model-predicted interaction recordings") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Yoho
ggplot() +
  geom_boxplot(data = newdata_yoho, aes(x = month, y = HWI, group = cut_width(month, 1))) +
  geom_point(data = newdata_yoho, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "#AA4499") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Number of Interaction (Monthly)") +
  ggtitle("Yoho National Park of Canada: Historical vs. model-predicted interaction recordings") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Plot temperature vs interactions ----

library(gridExtra)
temp <- 
  ggplot(data = data, 
         aes(y = HWI, x = avgtemp)) +
    geom_point(aes(col = park), alpha = 0.1, pch = 16) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs",
                              k = 4),
              method.args = list(family = poisson),
              se = FALSE,
              col = "black",
              size = 1.2) + # line for overall trend
  geom_smooth(aes(col = park),
              method = "gam",
              formula = y ~ s(x, bs = "cs",
                              k = 4),
              method.args = list(family = poisson),
              se = FALSE,
              linewidth = 0.7) + 
  scale_x_continuous(breaks = seq(-15,15,5)) + 
  scale_y_continuous(limits = c(0,30), expand = c(0,0.2)) +
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
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = 0.04, vjust = -7, size = 16, 
                                  family = "sans", face = "bold"),
        legend.position=c(0.1,0.5),
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


# Plot monthly interactions in response to precipitation
precip <- 
  ggplot(data = data, 
         aes(y = HWI, x = avgprecip)) +
    geom_point(aes(col = park), alpha = 0.1, pch = 16) +
    geom_smooth(method = "gam",
                formula = y ~ s(x, bs = "cs",
                                k = 4),
                method.args = list(family = poisson),
                se = FALSE,
                col = "black",
                size = 1.2) + # line for overall trend
    geom_smooth(aes(col = park),
                method = "gam",
                formula = y ~ s(x, bs = "cs",
                                k = 4),
                method.args = list(family = poisson),
                se = FALSE,
                linewidth = 0.7) + 
    scale_y_continuous(limits = c(0,30), expand = c(0,0.2)) +
    scale_color_manual(name = "National Parks", values = colour_park,
                       labels = c("Glacier",
                                  "Kootenay",
                                  "Revelstoke",
                                  "Pacific Rim",
                                  "Yoho")) + 
    xlab("Average Monthly Precipitation (mm)") +
    ylab("Total Monthly HWIs") +
    ggtitle("B") +
    guides(col = guide_legend(override.aes = list(alpha=1))) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=12, family = "sans", face = "bold"),
          axis.title.x = element_text(size=12, family = "sans", face = "bold"),
          axis.text.y = element_text(size=8, family = "sans"),
          axis.text.x  = element_text(size=8, family = "sans"),
          plot.title = element_text(hjust = 0.04, vjust = -7, size = 16, 
                                    family = "sans", face = "bold"),
          legend.position = "none",
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

TOP <- grid.arrange(temp, precip, 
                     ncol=2)

# Projecting ----

## Projected climate data ----
# Import clean projected climate data
#refer to "projected climate data" section within the 'elevation and climate.R' script
#
projection_climate_data <- readRDS("data/ClimateNA_v731/climate data/projection_climate_data_monthly.rds")
projection_climate_data <- merge(projection_climate_data, nationalparks_bc_coordinates,
                                 by = c("latitude","longitude"), all.x=TRUE)
names(projection_climate_data)[7] <- "avgtemp"
names(projection_climate_data)[6] <- "dec_year_month"

##Project the number of interactions under 4 SSP climate scenarios ----
#error when using the full model, species not found, unsure how to add species to the dataframe
projection_climate_data$predicted_interactions <- predict(poisson_nospecies, newdata = projection_climate_data, type = "response")
any(is.na(projection_climate_data))

#Calculate sum of interactions in each year then scale relative to 2020
agg_proj <- aggregate(predicted_interactions ~ year + park + scenario , data = projection_climate_data, FUN = "sum")

data2 <- agg_proj[which(agg_proj$year == 2022),]
data2 <- aggregate(predicted_interactions ~ park, data = data2, FUN = "median")

#Kootenay
agg_proj[which(agg_proj$park == "Kootenay National Park of Canada"),"predicted_interactions"] <- agg_proj[which(agg_proj$park == "Kootenay National Park of Canada"),"predicted_interactions"]/data2[which(data2$park == "Kootenay National Park of Canada"),"predicted_interactions"]

#Pacific Rim
agg_proj[which(agg_proj$park == "Pacific Rim National Park Reserve of Canada"),"predicted_interactions"] <- agg_proj[which(agg_proj$park == "Pacific Rim National Park Reserve of Canada"),"predicted_interactions"]/data2[which(data2$park == "Pacific Rim National Park Reserve of Canada"),"predicted_interactions"]

#Glacier
agg_proj[which(agg_proj$park == "Glacier National Park of Canada"),"predicted_interactions"] <- agg_proj[which(agg_proj$park == "Glacier National Park of Canada"),"predicted_interactions"]/data2[which(data2$park == "Glacier National Park of Canada"),"predicted_interactions"]

#Yoho
agg_proj[which(agg_proj$park == "Yoho National Park of Canada"),"predicted_interactions"] <- agg_proj[which(agg_proj$park == "Yoho National Park of Canada"),"predicted_interactions"]/data2[which(data2$park == "Yoho National Park of Canada"),"predicted_interactions"]

#Revelstoke
agg_proj[which(agg_proj$park == "Mount Revelstoke National Park of Canada"),"predicted_interactions"] <- agg_proj[which(agg_proj$park == "Mount Revelstoke National Park of Canada"),"predicted_interactions"]/data2[which(data2$park == "Mount Revelstoke National Park of Canada"),"predicted_interactions"]

library(ggh4x)        # to fill in facet wrap title boxes
#Make colour strips in x-direction for panel title boxes (they will correspond to SSPs)
strip <- strip_themed(background_x = 
                        elem_list_rect(fill = c("#0571b0", "#92c5de", "#f4a582", "#ca0020")))

#Assign a letter for each SSP so we can label each panel
data_text <- data.frame(label = c("A", "B", "C", "D"),
                        scenario = names(table(projection_climate_data$scenario)),
                        x = c(2023, 2023, 2023, 2023), 
                        y = c(1.9, 1.9, 1.9, 1.9))

BOT <- 
ggplot(agg_proj, aes(x = year , y = predicted_interactions)) + 
  geom_hline(yintercept = 1, size = 0.5, color = "grey70") + # line at 1 for reference
  geom_line(linewidth=0.5, aes(group = park, col = park), alpha = 0.8) + # line for each park
  facet_wrap2(~ scenario, strip = strip, labeller = as_labeller(c(
    `low_ssp126` = "SSP 1-2.6",
    `intermediate_ssp245` = "SSP 2-4.5",
    `high_ssp370` = "SSP 3-7.0",
    `veryhigh_ssp585` = "SSP 5-8.5")),
    nrow = 1) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative change in annual HWIs") +
  #scale_y_continuous(limits = c(0.75,2)) +
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
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(size = 12, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none",
        legend.key.size = unit(0.4, 'cm'),
        legend.title = element_text(size = 8, face = "bold"),
        legend.text=element_text(size=7),
        legend.box.background = element_rect(color = "black"),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

FIG <- grid.arrange(TOP, BOT, 
                    ncol=1, heights = c(1,0.7))

ggsave(FIG, filename = "figures/final_figure.png", device = NULL,
       path = NULL, scale = 1, width = 14.91, height = 6.47, units = "in", dpi = 600)
