
# Human-wildlife interactions
# Figure 5

#.......................................................
# Final Results Figure 5 ----
#.......................................................

#Number of parks to loop over
PARKS <- levels(data$park)
N <- length(PARKS)

#............................................................
## Temperature: Figure 5a ----
#............................................................

# Monthly HWI in response to temperature

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

temp <-
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
temp
ggsave(temp, filename = "figures/temp.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

#.......................................................
## Precipitation: Figure 5b ----
#.......................................................

# Monthly HWI in response to precipitation

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

precip <-
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
precip

ggsave(precip, filename = "figures/precip.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

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
        legend.key.size = unit(0.4, 'cm'),
        legend.title = element_text(size = 8, face = "bold"),
        legend.text=element_text(size=7),
        legend.box.background = element_rect(color = "black"),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
BOT 
ggsave(BOT, filename = "figures/BOT.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 3, units = "in", dpi = 600)

#..................................................
## Multi-panel ----
#..................................................

TOP <- grid.arrange(temp, precip, 
                    ncol=2)

FIG5 <- grid.arrange(TOP, BOT, 
                     ncol=1, heights = c(1,0.7))

ggsave(FIG5, filename = "figures/figure5.png", device = NULL, path = NULL, scale = 1, width = 14.91, height = 10.47, units = "in", dpi = 600)

