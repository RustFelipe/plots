library(ggplot2)
library(cowplot)

load("lrdata_2.RData")

plot <- subset(lrdata2, series == 'RO2',
                select = c('campaign', 'year', 
                           'pco2.headspace', "series", 
                           "campaigns", "conc.co2", 
                           "conc.ch4", "pch4.headspace"))
 
#Graph theme
mytheme2 <- function() {
  theme(legend.position = c(0.05, 0.95), legend.direction="vertical",
        legend.title = element_blank(),
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

#top plot
p1 <- ggplot(plot, aes(x = campaigns, y = pco2.headspace)) +
  geom_boxplot(aes(group = campaigns, fill = factor(year))) +
  geom_vline(xintercept = 2.5, color = "#858585", size = 1.5) + 
  geom_vline(xintercept = 5.5, color = "#858585", size = 1.5) +
  geom_vline(xintercept = 8.5, color = "#858585", size = 1.5) +
  annotate(geom = "label", x = 1.25, y = 3200, label = "2015", 
           fill = "#858585", fontface = "bold", color = "white", size = 7) +
  annotate(geom = "label", x = 4, y = 3200, label = "2016", 
           fill = "#858585", fontface = "bold", color = "white", size = 7) +
  annotate(geom = "label", x = 7, y = 3200, label = "2017", 
           fill = "#858585", fontface = "bold", color = "white", size = 7) +
  annotate(geom = "label", x = 10, y = 3200, label = "2018", 
           fill = "#858585", fontface = "bold", color = "white", size = 7) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), 
                     labels =c ("June", "August", 
                              "June","August", "October",
                              "June","August", "October",
                              "June", "August", "October"))+
  labs(y = expression(bold(paste("pCO"[2] , " (µatm)")))) +
  geom_smooth(method = "loess", span = 0.25, colour = "red")+
  mytheme2()+
  theme(legend.position="none",
        axis.title.x =  element_blank())
print(p1)  

#bottom plot
p2 <- ggplot(plot, aes(x = campaigns, y = pch4.headspace)) +
  geom_boxplot(aes(group = campaigns, fill = factor(year))) +
  geom_vline(xintercept = 2.5, color = "#858585", size = 1.5) + 
  geom_vline(xintercept = 5.5, color = "#858585", size = 1.5) +
  geom_vline(xintercept = 8.5, color = "#858585", size = 1.5) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), 
                     labels =c ("June", "August", 
                                "June","August", "October",
                                "June","August", "October",
                                "June", "August", "October"))+
  scale_y_continuous(limits = c(0,350))+
  labs(y = expression(bold(paste("pCH"[4] , " (µatm)"))), 
       x = expression(bold(paste("Campaigns")))) +
  mytheme2()+
  geom_smooth(method = "loess", span = 0.25, colour = "red")+
  theme (legend.position = c(0.025, 0.65))+
  guides(color = guide_legend(title.position = 'left'))
print(p2)

arrange5 <- plot_grid(p1, p2, align = "v", nrow = 2) 
             
ggsave(file="example_box_plot_smoothed.png",  width = 14, height = 7, dpi = 300, arrange5)
print(arrange5)
