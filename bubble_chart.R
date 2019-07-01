library(ggplot2)

load("lrdata_2.RData")

#Set the data

graph <- subset(lrdata2, subset = series == 'RO2',
                select = c('campaign',  "campaigns", 
                           "conc.co2", "do.cor.perc.ysi", 
                           "temp.water.ysi"))
graph$campaign <- factor(graph$campaign,
                         labels = c("June  ", "August  ", "October "))

mytheme2 <- function(){
  theme( legend.position = "right", legend.direction = "vertical",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.spacing.y = unit(-0.005, 'cm'),
        axis.line = element_line(size = 0.5, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 16, family = "Arial", face = "bold"),
        text = element_text(size = 16, family = "Arial"),
        axis.title.y = element_text(size = 16, colour = "black", vjust = +1),
        axis.title.x = element_text(size = 16, colour = "black", vjust = -1),
        axis.text.x = element_text(colour="black", size = 16),
        axis.text.y = element_text(colour="black", size = 16))
} 

#Graph 1 - CO2 vs Oxigen 

p1 <- ggplot(graph, aes(x = temp.water.ysi, 
                        y = do.cor.perc.ysi,
                        color = campaign)) +
  geom_point(alpha = 0.25, shape = 19, 
             aes(size = conc.co2)) +
  scale_y_continuous(limits = c(60, 120)) +
  scale_x_continuous(limits = c()) +
  labs(x = expression(bold(paste("Water temperature (°C)"))), 
       y = expression(bold(paste("Dissolved Oxygen (%)")))) +
  labs(size =  expression(paste("CO"[2], " (µM)")),
       color = 'Month') +
  scale_size_continuous(range = c(-2,12))+
  mytheme2 ()
  

print(p1)

ggsave("bubble_chart_example.png", width = 12, height = 5, dpi = 300)
