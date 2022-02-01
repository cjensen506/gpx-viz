readGPX <- function(
  gpx.file,
  metadata = TRUE,
  bounds = TRUE,
  waypoints = TRUE, 
  tracks = TRUE,  
  routes = TRUE   
)
  
{    
  opt <- options(warn=-1)    
  if(!file.exists(gpx.file)) stop("The file '", gpx.file, "'\n  does not exist in ", getwd() )
  
  if(metadata==TRUE) { metadata <- .readGPX.element(gpx.file, "name") }    
  if(bounds==TRUE) { bounds <- .readGPX.element(gpx.file, "bounds") }    
  if(waypoints==TRUE) { waypoints <- .readGPX.element(gpx.file, "wpt") }
  if(tracks==TRUE) { tracks <- .readGPX.element(gpx.file, "trk") }
  if(routes==TRUE) { routes <- .readGPX.element(gpx.file, "rte") }
  
  gpx <- list(metadata=metadata, bounds=bounds, waypoints=waypoints, tracks=tracks, routes=routes)
  return(gpx)
  on.exit(options(opt))
}

## Read various elements from a *.gpx file:

.readGPX.element <- function(gpx.file, element) {
  # element = "metadata", "wpt", "rte", "trk"
  
  ret <- xmlTreeParse(gpx.file, useInternalNodes = TRUE)
  # top structure: 
  top <- xmlRoot(ret)
  
  # check if there is any content:
  if(any(grep(element, names(top)))) {
    
    # tracks:
    if(element=="trk"){   
      ret <- NULL
      nu <- which(names(top) %in% element)
      for(c in seq_along(nu)){
        lst <- which(names(top[[nu[c]]]) %in% "trkseg")
        nm <- names(top[[nu[c]]][[lst[1]]][[1]])
        ret[[c]] <- list(NULL)
        for(i in seq_along(lst)) {
          trkpt <- top[[nu[c]]][[lst[i]]]
          ret[[c]][[i]] <- data.frame(NULL)
          ## get columns (https://www.topografix.com/GPX/1/1/#type_wptType)
          lon <- as.numeric(xmlSApply(trkpt, xmlGetAttr, "lon"))
          lat <- as.numeric(xmlSApply(trkpt, xmlGetAttr, "lat"))
          ret[[c]][[i]][1:length(lon),"lon"] <- lon
          ret[[c]][[i]][1:length(lat),"lat"] <- lat
          if(!nm[[1]]=="NULL"){
            for(j in 1:length(nm)){
              xm <- as.character(sapply(sapply(xmlChildren(trkpt), function(x) x[[nm[[j]]]]), xmlValue))
              ret[[c]][[i]][1:length(xm), nm[[j]]] <- xm 
            }
          } 
        }
        names(ret[[c]]) <- xmlValue(top[[nu[c]]][["name"]])
      }   
    }
    
    if(element=="wpt"){
      ret <- data.frame(NULL)
      nu <- which(names(top) %in% element)
      nm <- names(top[[nu[1]]])
      for(i in seq_along(nu)) {
        # coordinates:
        ret[i, "lon"] <- as.numeric(xmlGetAttr(top[[nu[i]]], "lon"))
        ret[i, "lat"] <- as.numeric(xmlGetAttr(top[[nu[i]]], "lat"))
        if(!nm[[1]]=="NULL"){
          for(j in 1:length(nm)){
            ret[i, nm[[j]]] <- xmlValue(xmlChildren(top[[nu[i]]])[[nm[[j]]]])
          }  
        }
      }
    }
    
    if(element=="rte"){
      ret <- NULL
      nu <- which(names(top) %in% element)
      for(c in seq_along(nu)){
        ret[[c]] <- data.frame(NULL)
        lst <- which(names(top[[nu[c]]]) %in% "rtept")
        nm <- names(top[[nu[c]]][[lst[1]]])
        for(i in seq_along(lst)) {
          rtept <- top[[nu[c]]][[lst[i]]]
          ret[[c]][i, "lon"] <- as.numeric(xmlGetAttr(rtept, "lon"))
          ret[[c]][i, "lat"] <- as.numeric(xmlGetAttr(rtept, "lat"))
          if(!nm[[1]]=="NULL"){
            for(j in c("name","cmt","desc","sym","type")){
              try(ret[[c]][i, j] <- xmlValue(rtept[[j]]), silent = TRUE)
            }
          } 
        }
        names(ret)[c] <- xmlValue(top[[nu[c]]][["name"]])
      }
    }
    
    # bounds
    if(element=="bounds"){
      nu <- which(names(top) %in% element)
      ret <- matrix(rep(NA, 4), nrow=2, dimnames = list(c("lat", "lon"), c("min", "max")))
      # coordinates:
      ret[1,1] <- as.numeric(xmlGetAttr(top[[nu[1]]], "minlon"))
      ret[1,2] <- as.numeric(xmlGetAttr(top[[nu[1]]], "maxlon"))
      ret[2,1] <- as.numeric(xmlGetAttr(top[[nu[1]]], "minlat"))
      ret[2,2] <- as.numeric(xmlGetAttr(top[[nu[1]]], "maxlat"))
    }
    
    # metadata
    if(element=="name"){
      lst <- c("name","desc","author","email","url","urlname","time")
      nu <- which(names(top) %in% lst)
      if(!nu[[1]]=="NULL"){      
        ret <- data.frame(NULL)
        for(i in seq_along(lst)) {
          try(ret[1,lst[i]] <- xmlValue(top[[nu[[i]]]]), silent = TRUE)
        }
      }
    }
    
  }
  else { ret <- NULL }
  
  return(ret)
}

#source https://github.com/cran/pracma/blob/master/R/haversine.R

haversine <- function(loc1, loc2, R = 6371.0) {
  if (is.character(loc1)) {
    locs <- strsplit(loc1, ',')[[1]]
    lat1 <- locs[1]; lon1 <- locs[2]
    lat1 <- .check_coords(lat1) * pi / 180
    lon1 <- .check_coords(lon1) * pi / 180
  } else if (is.numeric(loc1)) {
    if (length(loc1) != 2)
      stop("Coordinate input not in correct format.")
    lat1 <- loc1[1]; lon1 <- loc1[2]
    if (abs(lat1) > 90 || abs(lon1) > 180)
      stop("Coordinate input not in correct format.")
    lat1 <- lat1 * pi /180; lon1 <- lon1 * pi / 180
  } else {
    stop("Location must be given as string 'lat lon'.")
  }
  if (is.character(loc2)) {
    locs <- strsplit(loc2, ',')[[1]]
    lat2 <- locs[1]; lon2 <- locs[2]
    lat2 <- .check_coords(lat2) * pi / 180
    lon2 <- .check_coords(lon2) * pi / 180
  } else if (is.numeric(loc2)) {
    if (length(loc2) != 2)
      stop("Coordinate input not in correct format.")
    lat2 <- loc2[1]; lon2 <- loc2[2]
    if (abs(lat2) > 90 || abs(lon2) > 180)
      stop("Coordinate input not in correct format.")
    lat2 <- lat2 * pi /180; lon2 <- lon2 * pi / 180
  } else {
    stop("Location must be given as string 'lat, lon'.")
  }
  # R <- 6371.0  # average earth radius [km]
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  # Haversine formula
  a <- sin(dlat/2)^2 + 
    cos(lat1) * cos(lat2) * sin(dlon/2)^2;
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  return(R * c)
}


.check_coords <- function(s) {
  m <- gregexpr("^\\s*(\\d+)\\s+(\\d+)\\s+(\\d*)\\s*(\\w?)\\s*$",
                s, perl=TRUE)
  m <- m[[1]]
  if (m[1] != 1) stop("Coordinate input not in correct format.")
  
  strt <- attr(m, "capture.start")
  lngt <- attr(m, "capture.length")
  c1 <- as.numeric( substr(s, strt[1], strt[1]+lngt[1]-1) )
  c2 <- as.numeric( substr(s, strt[2], strt[2]+lngt[2]-1) )
  c3 <- as.numeric( substr(s, strt[3], strt[3]+lngt[3]-1) )
  
  c0 <- c1 + c2/60 + c3/3600
  if (c1 > 180 || c2 >= 60 || c3 >= 60)
    stop("Coordinate input not in correct format.")
  
  c4 <- substr(s, strt[4], strt[4]+lngt[4]-1)
  if (c4 == 'S' || c4 == 'W') {
    c0 <- -1 * c0
  } else if (c4 != 'N' && c4 != 'E') {
    stop("Coordinate input not in correct format.")
  }
  return(c0)
}
