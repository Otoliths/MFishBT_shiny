################################################################################################################################
#                                                                                                                              #
# Purpose:       Fig.2 Plotting                                                                                                #
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
library(tidyverse)
library(ggtext)

################################# Data loading #################################
phylopic <- read.csv("data/phylopic.csv")
tags <- readRDS("data/tags.rds")

labels <- setNames(
  paste0(
    "<img src='phylopic_img/", phylopic$uid, ".png'  width='8.5'/>  ",
    sapply(
      strwrap(phylopic$family, width = 8.5, simplify = FALSE),
      function(x) paste0(x, collapse = "")
    )
  ),
  phylopic$family
)

dat$Tags_type <- factor(dat$Tags_type, levels = c("Microchemistry", "Isotope"))

############################### Fig.2 Plotting #################################
ggplot(data = dat, aes(x = Family_fishbase, y = n, fill = GROMs_fishbase)) +
  geom_bar(
    stat = "identity",
    position = "fill") +
  scale_fill_manual("GROMS category", values = c("#b30059", "#220050", "#0091a8", "#359023", "#ffa500")) +
  coord_flip() +
  facet_grid(. ~ Tags_type) +
  ylab("Proportion of species") +
  xlab("") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    legend.margin = (margin(0, 0, 0, 0)),
    legend.text = element_text(size = 6, colour = "black"),
    legend.title = element_text(size = 6, colour = "black"),
    axis.title = element_text(size = 8, colour = "black"),
    axis.text = element_text(size = 5, colour = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.text.y = element_markdown(size = 4, colour = "black")
  ) +
  scale_y_continuous(expand = c(0.02, 0)) +
  scale_x_discrete(expand = c(0.01, 0), labels = labels)

################################# Fig.2 saving #################################
ggsave(filename = "output/figure2.pdf", width = 16, height = 21, dpi = 600, units = "cm")
