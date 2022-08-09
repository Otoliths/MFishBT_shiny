################################################################################################################################
#                                                                                                                              #
# Purpose:       Fig.3 Plotting                                                                                                #
#                                                                                                                              #
# Author:        Liuyong Ding et al.                                                                                           #
# Contact:       ly_ding@126.com                                                                                               #
# Client:        Liuyong Ding et al.                                                                                           #
#                                                                                                                              #
# Code created:  2022-08-08                                                                                                    #
# Last updated:  2022-08-08                                                                                                    #
# Source:        paper                                                                                                         #
#                                                                                                                              #
# Comment:       a biotracking/biomarkers database of global migratory fish                                                    #
#                                                                                                                              #
################################################################################################################################

############################### Packages loading ###############################
rm(list = ls())
library(ggplot2)
library(sf)
library(tmap)
library(RColorBrewer)
library(patchwork)

################################# Data loading #################################
data(rivers)
world <- map_data("world")
scape <- readRDS("data/scape.rds")

chemoscape <- subset(scape, scape$Element_composition == "Sr/Ca" & scape$Element_tissue == "Otolith")
isoscape <- subset(scape, scape$Element_composition == "87Sr/86Sr" & scape$Element_tissue == "Otolith")

############################### Fig.3a Plotting ################################
p1 <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#dedede") +
  geom_sf(data = rivers, colour = "white", size = 0.05) +
  geom_point(data = chemoscape, aes(x = Longitude, y = Latitude, colour = Value, size = N), fill = "white", alpha = 0.5) +
  scale_color_gradientn(expression("Sr/Ca"[" Otolith_edge"]),
    colours = rev(brewer.pal(11, "Spectral")),
    limits = c(-0.001, 0.031), breaks = c(0, 0.01, 0.02, 0.03)) +
  scale_size("Sample size", range = c(0.1, 3)) +
  theme_bw() +
  theme(
    legend.background = element_blank(),
    legend.position = c(0.17, 0.24),
    legend.text = element_text(color = "black", size = 5),
    legend.key.width = unit(5, "mm"),
    legend.key.height = unit(2, "mm"),
    legend.direction = "horizontal",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid = element_blank(),
    legend.box.spacing = unit(0, "mm"),
    legend.key = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  guides(
    colour = guide_colourbar(
      order = 1,
      title.position = "top",
      title.theme = element_text(size = 7, color = "black", face = "bold"),
      ticks.colour = "grey60"
    ),
    size = guide_legend(
      order = 2,
      nrow = 1,
      title.position = "top",
      title.theme = element_text(size = 7, color = "black", face = "bold"),
      size = 1
    )
  ) +
  xlab("") +
  ylab("")


############################### Fig.3b Plotting ################################
p2 <- ggplot() +
  geom_polygon(
    data = world, aes(x = long, y = lat, group = group),
    fill = "#dedede") +
  geom_sf(data = rivers, colour = "white", size = 0.05) +
  geom_point(data = isoscape, aes(x = Longitude, y = Latitude, colour = Value, size = N), fill = "white", alpha = 0.5) +
  scale_color_gradientn(expression(""^87 * "Sr/"^86 * "Sr" * "(‰)"[" Otolith_edge"]),
    colours = rev(brewer.pal(11, "Spectral")),
    limits = c(0.69, 0.79), breaks = c(0.70, 0.72, 0.74, 0.76, 0.78)) +
  scale_size("Sample size", range = c(0.1, 3), breaks = c(50, 100, 150)) +
  theme_bw() +
  theme(
    legend.background = element_blank(),
    legend.position = c(0.17, 0.24),
    legend.text = element_text(color = "black", size = 5),
    legend.key.width = unit(5, "mm"),
    legend.key.height = unit(2, "mm"),
    legend.direction = "horizontal",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    panel.grid = element_blank(),
    legend.box.spacing = unit(0, "mm"),
    legend.key = element_rect(fill = NA),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  guides(
    colour = guide_colourbar(
      order = 1,
      title.position = "top",
      title.theme = element_text(size = 7, color = "black", face = "bold"),
      ticks.colour = "grey60"
    ),
    size = guide_legend(
      nrow = 1, order = 2,
      title.position = "top",
      title.theme = element_text(size = 7, color = "black", face = "bold"),
      size = 1
    )
  ) +
  xlab("") +
  ylab("")

################################# Fig.3 saving #################################
p1 / p2 + plot_annotation(tag_levels = "a")
ggsave(filename = "output/figure3.pdf", width = 16, height = 16, dpi = 600, units = "cm")
