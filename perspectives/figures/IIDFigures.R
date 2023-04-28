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

root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper/Figures"

#Figure 1. Study design attributes####

#1A. Method attributes----

method <- expand.grid(Recording = c(0, 0.5),
                    Classification = c(0, 0.5)) %>% 
  mutate(Score = Recording + Classification)

plot.method <- ggplot(method) +
  geom_raster(aes(x=Recording, y=Classification, fill = Score)) +
  scale_fill_viridis_c(name="Difficulty of\nindividual\nidentification", breaks=c(0, 1, 2), labels=c("Low", "Medium", "High")) +
  coord_cartesian(xlim = c(-0.25, 0.75), ylim=c(-0.25, 0.75), clip = 'off') +
  scale_x_continuous(breaks = c(0, 0.5), labels = c("Targeted", "Passive")) +
  scale_y_continuous(breaks = c(0, 0.5), labels = c("Closed set", "Open set")) +
  my.theme +
  theme(legend.text = element_text(size=10),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, margin = margin(0,-12,0,0)),
        axis.text.x = element_text(vjust = 2, size=10, margin = margin(-2,0,0,0)),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=14, face="bold"),
        legend.position = "none") +
  xlab("Recording method") +
  ylab("Individual classification method") +
  ggtitle("A. Method attributes")
plot.method

#ggsave(plot.method, filename=file.path(root, "Figure1AMethodPlot.jpeg"), width=7, height=5.85, units="in", dpi = 300, device="jpeg")

#1B. Extent attributes----

base <- expand.grid(Time = seq(0, 1, 0.01),
                    Space = seq(0, 1, 0.01)) %>% 
  mutate(Score = Time+Space)

plot.base <- ggplot() +
  geom_raster(data=base, aes(x=Time, y=Space, fill = Score)) +
  scale_fill_viridis_c(name="Difficulty of\nindividual\nidentification", breaks=c(0, 1, 2), labels=c("Low", "Medium", "High")) +
  scale_x_continuous(breaks = c(0.15, 0.5, 0.85), labels = c("Single recording", "Multiple recordings\nin season", "Multiple recordings\nbetween years")) +
  scale_y_continuous(breaks = c(0.15, 0.5, 0.85), labels = c("Single location", "Multiple locations\nin population", "Multiple locations\nin meta-population")) +
  #  geom_text(aes(x=-0.35, y=1, label = c("B)")), size=10) +
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1), clip = 'off') +
  xlab("Temporal extent") +
  ylab("Spatial extent") +
  my.theme +
  theme(axis.text.x = element_text(angle = 35, size=10, hjust = 1, vjust = 0.5, margin = margin(-40,0,20,0)),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(angle=35, size=10, vjust = 0, hjust=1, margin = margin(0,-12,0,0)),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(size=14, face="bold"),
        legend.position = "") +
  ggtitle("B. Extent attributes")
plot.base

ggsave(plot.base, width = 7, height = 5.85, units="in", filename=file.path(root, "Figure1BBasePlot.jpeg"), dpi=300)

#Figure 1A & 1B together----
plot.legend <- ggplot() +
  geom_raster(data=base, aes(x=Time, y=Space, fill = Score)) +
  scale_fill_viridis_c(name="Difficulty of\nindividual identification", breaks=c(0, 1, 2), labels=c("Low", "Medium", "High")) +
  my.theme +
  theme(legend.text = element_text(size=10),
        legend.position = "bottom")

leg <- cowplot::get_legend(plot.legend)

ggsave(grid.arrange(plot.base, plot.method, leg,
                    widths = c(5,6),
                    heights = c(4,0.4),
                    layout_matrix = rbind(c(2,1),
                                          c(3,1))),
       filename=file.path(root, "Figure1StudyDesignAttributes.jpeg"), width = 10, height = 5.4)


#Figure 2. Ecological attributes####

#2A. Method attributes-----
method.eco <- rbind(method %>% 
                      mutate(eco = "low",
                             Score = Score*0.6666),
                    method %>% 
                      mutate(eco = "high",
                             Score = Score*0.6666 + 0.3333))

plot.method.eco <- ggplot(method.eco) +
  geom_raster(aes(x=Recording, y=Classification, fill = Score)) +
  scale_fill_viridis_c(name="Difficulty of\nindividual\nidentification", breaks=c(0, 1, 2), labels=c("Low", "Medium", "High")) +
  coord_cartesian(xlim = c(-0.25, 0.75), ylim=c(-0.25, 0.75), clip = 'off') +
  scale_x_continuous(breaks = c(0, 0.5), labels = c("Targeted", "Passive")) +
  scale_y_continuous(breaks = c(0, 0.5), labels = c("Open set", "Closed set")) +
  facet_wrap(~eco, nrow=2) +
  my.theme +
  theme(legend.text = element_text(size=10),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, margin = margin(0,-12,0,0)),
        axis.text.x = element_text(vjust = 2, size=10, margin = margin(-2,0,0,0)),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=14, face="bold"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none") +
  xlab("Recording method") +
  ylab("Individual classification method")
plot.method.eco

#2B. Extent Attributes----
base.eco <- rbind(base %>% 
                mutate(eco = "low",
                       Score = Score*0.6666),
              base %>% 
                mutate(eco = "high",
                       Score = Score*0.6666 + 0.66666))

eco.label <- data.frame(label = c("Low individual distinctiveness\nhigh animal density\nlow sound availability\nhigh animal movement", "High individual distinctiveness\nlow animal density\nhigh sound availability\nlow animal movement"), eco = c("low", "high"))

plot.base.eco <- ggplot() +
  geom_raster(data=base.eco, aes(x=Time, y=Space, fill = Score)) +
  scale_fill_viridis_c(name="Difficulty of\nindividual\nidentification", breaks=c(0, 1, 2), labels=c("Low", "Medium", "High")) +
  scale_x_continuous(breaks = c(0.15, 0.5, 0.85), labels = c("Single recording", "Multiple recordings\nin season", "Multiple recordings\nbetween years")) +
  scale_y_continuous(breaks = c(0.15, 0.5, 0.85), labels = c("Single location", "Multiple locations\nin population", "Multiple locations\nin meta-population")) +
  coord_cartesian(xlim = c(0, 1), ylim=c(0, 1), clip = 'off') +
  xlab("Temporal extent") +
  ylab("Spatial extent") +
  geom_text(data = eco.label, aes(x=1.12, y=0.5, label = label), size=5, angle = 270, fontface="bold") +
  my.theme +
  theme(axis.text.x = element_text(angle = 35, size=10, hjust = 1, vjust = 0.5, margin = margin(-40,0,20,0)),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(angle=35, size=10, vjust = 0, hjust=1, margin = margin(0,-12,0,0)),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(size=14, face="bold"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(1,75,1,1),
        legend.position = "") +
  facet_wrap(~eco, nrow=2)
plot.base.eco

#Put together-----
ggsave(grid.arrange(plot.base.eco, plot.method.eco, leg,
                    widths = c(5,7),
                    heights = c(8,0.4),
                    layout_matrix = rbind(c(2,1),
                                          c(3,1))),
       filename="Figure2EcologicalAttributes.jpeg", width = 10, height = 9.4)
