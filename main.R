library(XML)
source("./helper_fun.R")
library(ggplot2)


gpx_data <- readGPX("./export_584780/activities/10186153.gpx")

gpx_track <- as.data.frame(gpx_data$tracks)
colnames(gpx_track) <- c("lon", "lat", "ele", "time")

ggplot(gpx_track, aes(x = lon, y = lat)) + coord_quickmap() + geom_point()