library(tidyverse)
library(gridExtra)
library(pBrackets)
library(Cairo)

my.theme <- theme_classic() +
  theme(text=element_text(size=12, family="Arial"),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        plot.title=element_text(size=12, hjust = 0.5))

root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper"

#Figure 1. Study design attributes####

#1A. Method attributes----

method <- expand.grid(Recording = c(0, 0.5),
                    Classification = c(0, 0.5)) %>% 
  mutate(Score = Recording + Classification)

plot.method <- ggplot(method) +
  geom_raster(aes(x=Recording, y=Classification, fill = Score)) +
  scale_fill_viridis_c(name="Difficulty of acoustic      \nindividual identification     ", breaks=c(0, 1), labels=c("Easy", "Hard")) +
  coord_cartesian(xlim = c(-0.25, 0.75), ylim=c(-0.25, 0.75), clip = 'off') +
  scale_x_continuous(breaks = c(0, 0.5), labels = c("Targeted", "Passive")) +
  scale_y_continuous(breaks = c(0, 0.5), labels = c("Closed-set", "Open-set")) +
  my.theme +
  theme(legend.text = element_text(size=10),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, margin = margin(0,-12,0,0)),
        axis.text.x = element_text(vjust = 2, size=10, margin = margin(-2,0,0,0)),
        axis.title.y = element_text(margin=margin(0, 50, 0, 0)),
        axis.title.x = element_text(margin=margin(50, 0, 0, 0)),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=14, face="bold", margin = margin(0,0,-5,0)),
        legend.position = "") +
  xlab("Recording method") +
  ylab("Classification method") + 
  ggtitle("Study design attributes")
plot.method

#1B. Extent attributes----

base <- expand.grid(Time = seq(0, 1, 0.01),
                    Space = seq(0, 1, 0.01)) %>% 
  mutate(Score = Time+Space)

plot.base <- ggplot() +
  geom_raster(data=base, aes(x=Time, y=Space, fill = Score)) +
  scale_fill_viridis_c(name="Difficulty of\nindividual\nidentification", breaks=c(0, 1, 2), labels=c("Low", "Medium", "High")) +
  scale_x_continuous(breaks = c(0.25, 0.5, 0.75), labels = c("Single recording", "Multiple recordings\nin season", "Multiple recordings\nbetween years")) +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75), labels = c("Single location", "Multiple locations\nin population", "Multiple locations\nin meta-population")) +
  #  geom_text(aes(x=-0.35, y=1, label = c("B)")), size=10) +
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1), clip = 'off') +
  xlab("Temporal extent") +
  ylab("Spatial extent") +
  my.theme +
  theme(axis.text.x = element_text(angle = 35, size=10, hjust = 1, vjust = 0.5, margin = margin(-35,0,20,0)),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(angle=35, size=10, vjust = 0, hjust=1, margin = margin(0,-10,0,0)),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(margin=margin(0, 10, 0, 0)),
        axis.title.x = element_text(margin=margin(10, 0, 0, 0)),
        legend.text = element_text(size=10),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(size=14, face="bold", margin = margin(0,0,-5,0)),
        legend.position = "") +
  ggtitle("Extent attributes")
plot.base


#Figure 1A & 1B together----
plot.legend <- ggplot() +
  geom_raster(data=base, aes(x=Time, y=Space, fill = Score)) +
  scale_fill_viridis_c(name="Difficulty of\nindividual identification", breaks=c(0, 1, 2), labels=c("Low", "Medium", "High")) +
  my.theme +
  theme(legend.text = element_text(size=10, margin=margin(0,10,5,0)),
        legend.position = "bottom")

leg <- cowplot::get_legend(plot.legend)

ggsave(grid.arrange(plot.base, plot.method, leg,
                    widths = c(5,5.5),
                    heights = c(5.2,0.5),
                    layout_matrix = rbind(c(2,1),
                                          c(3,3))),
       filename=file.path(root, "Figures", "FrameworkPlot.jpeg"), width = 10.5, height = 5.7, units="in", dpi = 300, device = "jpeg")

