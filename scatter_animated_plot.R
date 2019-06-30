library(ggplot2)
library(gganimate)

load("lrdata.RData")

data <- subset(lrdata, subset = series %in%  c("LR", "RO2", "RO1", "RO3") &
               sample.type %in%  c("HeadwaterLakes","Reservoir", "River"),
               select = c("Rn", "distance.from.mouth", "pco2.headspace", "campaign", "year"))

#Graph theme

mytheme2 <- function() {
  theme(legend.position = c(0.25, 0.80), legend.direction="vertical",
        legend.title = element_text(size = 20),
        legend.text = element_text(size=20),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 20, family = "Arial"),
        text = element_text(size = 20, family = "Arial"),
        axis.title.y = element_text(size = 20, colour = "black", vjust = +1),
        axis.title.x = element_text(size = 20, colour = "black", vjust = -1),
        axis.text.x = element_text(colour="black", size = 20),
        axis.text.y = element_text(colour="black", size = 20))
}

#set the data for geom rect and text for the following year
year2 <- subset(data, year %in% c(2016,2017,2018))
year3 <- subset(data, year %in% c(2017,2018))

#Set the plot color
color4 <- c("#3db4ff", "#ffa43d", "#00ba38", "violetred1")

#plot
p1 <- ggplot(data, aes(x = distance.from.mouth, 
                       y = pco2.headspace,
                       color = factor(year))) +
  geom_point(shape = 19, 
             size = 2, 
             aes(group = seq_along(year))) +
  geom_rect(aes(xmin = 154, xmax = 85, ymin = 0, ymax = Inf, group = seq_along(year)),
           color = "black", alpha = 0.0019, size = 0.2) +
  geom_rect(year2, mapping = aes(xmin = 83, xmax = 51.5, ymin = 0, ymax = Inf, 
                                 group = seq_along(year)),
            color = "black", alpha = 0.0019, size = 0.2) +
  geom_rect(year3, mapping = aes(xmin = 191, xmax = 156, ymin = 0, ymax = Inf, 
                                 group = seq_along(year)),
            color = "black", alpha = 0.0019, size = 0.2) +
  geom_text(aes(x = 120 , y  = 4000, group = seq_along(year)),
            color = "black", label = "RO-2", size = 6) +
  geom_text(year2, mapping = aes(x = 67 , y  = 4000, group = seq_along(year)),
            color = "black", label = "RO-1", size = 6) +
  geom_text(year3, mapping = aes(x = 173 , y  = 4000, group = seq_along(year)),
            color = "black", label = "RO-3", size = 6) +
  geom_smooth(method = "loess", formula = "y ~ x", se = F, aes(group = factor(year))) +
  mytheme2()+
  scale_color_manual(values = color4) +
  scale_y_continuous(limits = c(0, 4000)) +
  scale_x_reverse(limits = c()) +
  labs(title = "Year: {frame_along}", 
       color = "Year",
       x = expression(bold(paste("Upriver                           Distance from mouth (km)                     Downriver"))), 
       y = expression(bold(paste("pCO"[2] , " (µatm)")))) +
  transition_reveal(year)

animate(p1, nframes = 120, end_pause = 60, rewind = FALSE, width = 900, height = 500)

anim_save("scatter_animated_plot.gif", dpi = 300) 



