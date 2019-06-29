library(ggplot2)
library(cowplot)

load("lrdata.RData")

#subset the data
data <- subset(lrdata, subset = series %in%  c("LR") & 
                 year %in%  c(2015)   & distance.from.mouth > 154 &
                 sample.type %in%  c("HeadwaterLakes","Reservoir", "River", "Tributary", 
                                     "Stream"),
               select = c("Rn", "distance.from.mouth", "pco2.headspace", "campaign"))

#Change the campaign variable to factor
data$campaign <- factor(data$campaign,
                        labels = c("June", "August"))

#Graph theme
mytheme2 <- function() {
  theme(legend.position = c(0.05, 0.95), legend.direction="horizontal",
        legend.title = element_text(size = 16),
        legend.text = element_text(size=16),
        axis.line = element_line(size=0.5, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 20, family = "Arial"),
        text = element_text(size = 20, family = "Arial"),
        axis.title.y = element_text(size = 16, colour = "black", vjust = +1),
        axis.title.x = element_text(size = 16, colour = "black", vjust = -1),
        axis.text.x = element_text(colour="black", size = 16),
        axis.text.y = element_text(colour="black", size = 16))
}

#Set the points color
color1 <- c("#3db4ff", "#ffa43d")

(p1 <- ggplot(data, aes(x = distance.from.mouth, 
                        y = pco2.headspace,
                        color = campaign)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 400, 
               linetype = "dashed", 
               color = "red", 
               size = 1) +
    scale_y_continuous(limits = c(300, 1000)) +
    scale_x_reverse(limits = c()) +
    scale_color_manual(values = color1) +
    labs(x = "",
         y = expression(bold(paste("pCO"[2] , " (µatm)")))) +
    mytheme2()+
    theme (legend.position = "none")) 

    
(p2 <- ggplot(data, aes(x = distance.from.mouth, 
                        y = Rn,
                        color = campaign)) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 200)) +
    scale_x_reverse(limits = c()) +
    scale_color_manual(values = color1) +
    labs(x = expression(bold(paste("Upriver                           Distance from mouth (km)                     Downriver"))), 
         y = expression(bold(paste("Rn  (Bq/m"^3, ")")))) +
    mytheme2()+
    labs(color='Month')+
    guides(color = guide_legend(title.position = 'left')))

arrange2 <- plot_grid(p1, p2, align = "v", nrow = 2)
ggsave(file="co2vsrn_2.png",  width = 9, height = 6, dpi = 300, arrange2 )
print(arrange2 )