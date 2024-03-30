# model fit

#............................................................
# Load packages ----
#............................................................

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
# Data ----
#............................................................

#import national parks data
data <- readRDS("Data/HWI/data.RDS")

#load model
model <- readRDS("Data/HWI/model.RDS")

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
# Visual comparison ----
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
ggsave(plot = last_plot(), filename = "Figures/Supplementary Figures/HWI/model behaviour/fitcheck_kootenay.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

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
ggsave(plot = last_plot(), filename = "Figures/Supplementary Figures/HWI/model behaviour/fitcheck_revelstoke.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

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
ggsave(plot = last_plot(), filename = "Figures/Supplementary Figures/HWI/model behaviour/fitcheck_pacific_rim.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

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
ggsave(plot = last_plot(), filename = "Figures/Supplementary Figures/HWI/model behaviour/fitcheck_yoho.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

