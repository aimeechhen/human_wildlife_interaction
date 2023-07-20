
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
