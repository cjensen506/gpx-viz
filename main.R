library(XML)
source("./helper_fun.R")
library(ggplot2)
library(readr)
library(tools)

strava_export_name <- "export_584780"
mt_hood_lat <- 45.37359
mt_hood_lon <- -121.69581
mt_hood_location <- c(45.37359, -121.69581)

#gpx_data <- readGPX("./export_584780/activities/10186153.gpx")

#gpx_track <- as.data.frame(gpx_data$tracks)
#colnames(gpx_track) <- c("lon", "lat", "ele", "time")



activities <- read_csv("./" + strava_export_name + "/activities.csv", col_types = cols(`Activity.Date` = col_datetime(format = "%b %d, %Y, %I:%M:%S %p")), name_repair = "universal")

snow_sports <- activities[activities$`Activity.Type` %in% c("Nordic Ski", "Alpine Ski", "Backcountry Ski"),]

snow_tracks <- data.frame()

for(i in 1:nrow(snow_sports)){
  print(snow_sports[i,]$Activity.Name)
  fname <- snow_sports[i,]$Filename
  print(fname)
  if(!is.na(fname) & file_ext(fname) == "gpx"){
   #print(snow_sports[i,])
    row_gpx <- readGPX(paste("./", strava_export_name, "/", fname, sep = ""))
    gpx_track <- as.data.frame(row_gpx$tracks)
    colnames(gpx_track) <- c("lon", "lat", "ele", "time")
    avg_lat <- mean(gpx_track$lat)
    avg_lon <- mean(gpx_track$lon)
    
    dist_to_hood <- haversine(mt_hood_location, c(avg_lat, avg_lon))
    print(dist_to_hood)
    if(dist_to_hood < 60) {
      gpx_track$Activity.ID <- snow_sports[i,]$Activity.ID
      gpx_track$time <- as.POSIXct(gpx_track$time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      start_time <- as.numeric(gpx_track[1,]$time)
      gpx_track$time_normal <- as.numeric(gpx_track$time) - start_time
      snow_tracks <- rbind(snow_tracks, gpx_track)
    }
  }
}

ggplot(snow_tracks, aes(x = lon, y = lat)) + coord_quickmap() + geom_point()
