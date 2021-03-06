library(XML)
source("./helper_fun.R")
library(ggplot2)
library(readr)
library(tools)
library(dplyr)
library(gganimate)
library(ggmap)
library(foreach)
library(doParallel)


strava_export_name <- "export_584780"

mt_hood_location <- c(45.37359, -121.69581)

activities <- read_csv(paste("./", strava_export_name, "/activities.csv", sep = ""), col_types = cols(`Activity.Date` = col_datetime(format = "%b %d, %Y, %I:%M:%S %p")), name_repair = "universal")

snow_sports <- activities[activities$`Activity.Type` %in% c("Nordic Ski", "Alpine Ski", "Backcountry Ski"),]

snow_tracks <- data.frame()

num_of_cores <- detectCores() -1
cl <- makeCluster(num_of_cores, type="FORK")  
registerDoParallel(cl)  

start <- proc.time()

snow_tracks <- foreach(i=icount(nrow(snow_sports)), .combine=rbind) %dopar% {
  fname <- snow_sports[i,]$Filename
  if(!is.na(fname) & file_ext(fname) == "gpx"){
    row_gpx <- readGPX(paste("./", strava_export_name, "/", fname, sep = ""))
    gpx_track <- as.data.frame(row_gpx$tracks)
    colnames(gpx_track) <- c("lon", "lat", "ele", "time")
    
    avg_lat <- mean(gpx_track$lat)
    avg_lon <- mean(gpx_track$lon)
    
    dist_to_hood <- haversine(mt_hood_location, c(avg_lat, avg_lon))
    
    if(dist_to_hood < 60) {
      gpx_track$Activity.ID <- snow_sports[i,]$Activity.ID
      gpx_track$Activity.Type <- snow_sports[i,]$Activity.Type
      gpx_track$time <- as.POSIXct(gpx_track$time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      start_time <- as.numeric(gpx_track[1,]$time)
      gpx_track$time_normal <- as.numeric(gpx_track$time) - start_time
      
      day_time <- strftime(gpx_track$time, format="%H:%M:%S")
      ref_day <- "2022-01-01"
      day_time <- as.POSIXct(paste(ref_day, day_time), tz = "PST")
      ref_time <- as.POSIXct(paste(ref_day, "00:00:00"), tz = "PST")
      gpx_track$time_24 <- as.numeric(day_time - ref_time)
      if(max(gpx_track$time_24 > 24)){
        gpx_track$time_24 <- gpx_track$time_24 / (60*60)
        gpx_track[gpx_track$time_24 > 23,]$time_24 <- gpx_track[gpx_track$time_24 > 23,]$time_24 -24
      }
      gpx_track
    }
  }
}

print(proc.time()-start)

registerDoSEQ()

# 
# ski_hood_plot <- ggplot(snow_tracks %>% filter(row_number() %% 10 == 1), aes(x = lon, y = lat, colour = factor(Activity.Type), group = Activity.ID)) + coord_quickmap() + geom_path(size = .2, alpha = .25) + theme_void()
# ski_hood_plot <- ski_hood_plot + theme(legend.position = "none")
# ski_hood_plot
# 
# ski_hood_animate <- ski_hood_plot + geom_point(size = 2) + transition_reveal(time_normal)
# 
# animate(ski_hood_animate, height = 800, width = 800)
# anim_save("Ski_Hood_samestart.gif")

ski_hood_plot <- ggplot(snow_tracks %>% filter(row_number() %% 10 == 1), aes(x = lon, y = lat, group = Activity.ID)) + coord_quickmap() + geom_path(size = .2, alpha = .75, aes(colour = factor(Activity.Type))) + theme_void()
ski_hood_plot <- ski_hood_plot + theme(legend.position = "none") + scale_color_manual(values = c("#FF9A3B", "#3BA0FF"))
ski_hood_plot <- ski_hood_plot + labs(title = "A day of skiing on Mt. Hood", caption = "https://github.com/cjensen506/gpx-viz") + theme(plot.title = element_text(size = 24))
ski_hood_plot

ggsave(filename = "Ski_Hood_day.png", units = "px", height = 2400, width = 2400, bg = "#FFFFFF", path = "./output")

ski_hood_animate <- ski_hood_plot + geom_point(size = 2) + transition_reveal(time_24)
ski_hood_animate <- ski_hood_animate + labs(subtitle = '{round(frame_along)}:00')
ski_hood_animate <- ski_hood_animate + theme(plot.subtitle = element_text(size = 18))

animate(ski_hood_animate, height = 800, width = 800, duration = 24, fps = 15, end_pause = 30)
anim_save("Ski_Hood_day.gif", path = "./output")


l_lon <- min(snow_tracks$lon)
b_lat <- min(snow_tracks$lat)
r_lon <- max(snow_tracks$lon)
t_lat <- max(snow_tracks$lat)

map_pad <-(t_lat-b_lat) * .05
map_coord <- c(left = l_lon - map_pad, bottom = b_lat - map_pad, right = r_lon + map_pad, top = t_lat + map_pad)

map <- get_stamenmap( bbox = map_coord, zoom = 12, maptype = "terrain-background")

map_plot <- ggmap(map, darken = c(0.6, "white")) + geom_path(aes(x = lon, y = lat, colour = factor(Activity.Type), group = Activity.ID), data = snow_tracks %>% filter(row_number() %% 10 == 1), size = .3, alpha = 1) + theme(legend.position = "none")
map_plot <- map_plot + theme_void() + theme(legend.position = "none")
map_plot <- map_plot + scale_color_manual(values = c("#FF9A3B", "#3BA0FF"))
map_plot <- map_plot + labs(title = "A day of skiing on Mt. Hood", caption = "https://github.com/cjensen506/gpx-viz") + theme(plot.title = element_text(size = 24))
map_plot

ggsave(filename = "Ski_Hood_day_map.png", units = "px", height = 2400, width = 2400, bg = "#FFFFFF", path = "./output")

ski_hood_animate_map <- map_plot + geom_point(aes(x = lon, y = lat, group = Activity.ID), size = 2, data = snow_tracks %>% filter(row_number() %% 10 == 1)) + transition_reveal(time_24)
ski_hood_animate_map <- ski_hood_animate_map + labs(subtitle = '{round(frame_along)}:00')
ski_hood_animate_map <- ski_hood_animate_map + theme(plot.subtitle = element_text(size = 18))

animate(ski_hood_animate_map, height = 800, width = 800, duration = 24, fps = 15, end_pause = 30)
anim_save("Ski_Hood_day_map.gif", path = "./output")
