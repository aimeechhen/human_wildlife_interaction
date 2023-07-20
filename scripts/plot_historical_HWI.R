

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
