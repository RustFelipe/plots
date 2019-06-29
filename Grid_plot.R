library(ggplot2)
library(cowplot)

load("lrdata.RData")

data <- subset(lrdata, subset = series %in%  c("LR") & 
                 year %in%  c(2015)   & distance.from.mouth > 154 &
                 sample.type %in%  c("HeadwaterLakes","Reservoir", "River", "Tributary", 
                                     "Stream"),
               select = c("Rn", "distance.from.mouth", "pco2.headspace"))
