# Import AURN data using openair
# install.packages(c('BH', 'stringr', 'png', 'RJSONIO', 'lazyeval', 'dplyr', 
#                    'maps', 'plyr', 'reshape2', 'latticeExtra', 'RColorBrewer', 
#                    'mapproj', 'hexbin', 'mapdata', 'RgoogleMaps', 'Rcpp'))
# install.packages("openair")

x <- data.frame(matrix(NA,ncol=14, nrow = 0))
for (i in list.files("data/Pollution/UKAIR/DEFRA_England/")){
  
  x <- rbind(x, read.csv(paste("data/Pollution/UKAIR/DEFRA_England/",i,sep="")))
  
}
allStations <- x[!is.na(x$Latitude) & !is.na(x$Longitude),]
# allStationsIDs <- as.character(unique(x$UK.AIR.ID))

openairStations <- read.csv("data/Pollution/UKAIR/OpenAirStations.csv", header = FALSE)
names(openairStations) <- c("ID", "Name", "Type")
openairStations$Latitude <- NA
openairStations$Longitude <- NA

# add coordinates
for (i in 1:length(openairStations$Name)){
  
  Name <- as.character(openairStations$Name)[i]
  myRow <- try(which(allStations$Site.Name %in% Name), silent=TRUE)
  
  # if(inherits(myRow,"try-error")) { next } # only true if an error occurs
  if(length(myRow)==0) { next }
  
  openairStations$Latitude[i] <- allStations$Latitude[myRow]
  openairStations$Longitude[i] <- allStations$Longitude[myRow]
  
}
openairStations <- openairStations[!is.na(openairStations$Latitude) & !is.na(openairStations$Longitude),]

saveRDS(openairStations, "data/Pollution/openairStations.rds")

library(openair)
# Download a file for each station.
# Data could be downloaded from 1973 to 2015

for(i in as.character(openairStations$ID)){
    
  stationDATA <- try(importAURN(site = i, year = 1973:2015), silent=TRUE)
  
  if(inherits(stationDATA,"try-error")) { next } # only true if an error occurs
  
  if (is.null(dim(stationDATA))){
    next
  }else{
    saveRDS(stationDATA, paste("data/Pollution/UKAIR/openairAURN/", i, ".rds", sep=""))
  }
  
  closeAllConnections()
  
}

# Or download all station info in one file 
source('~/kehra/scripts/GetData/importAURN.R')
x <- importAURN(site = as.character(openairStations$ID), year = 1998:2013)
# Aggregate data by day (mean)

# names(x)
# Order by Date
x <- x[with(x, order(date)), ]
# add coordinates
x$Latitude <- NA
x$Longitude <- NA
for (i in 1:length(unique(x$site))){
  
  Name <- as.character(unique(x$site)[i])
  myRow <- try(which(allStations$Site.Name %in% Name), silent=TRUE)
  myXRows <- try(which(x$site %in% Name), silent=TRUE)
  
  # if(inherits(myRow,"try-error")) { next } # only true if an error occurs
  if(length(myRow)==0) { next }
  
  x$Latitude[myXRows] <- allStations$Latitude[myRow]
  x$Longitude[myXRows] <- allStations$Longitude[myRow]
  
}
x <- x[!is.na(x$Latitude) & !is.na(x$Longitude),]
saveRDS(x, "data/Pollution/UKAIR/EnglandAURNdata1998_2013.rds")

# library(ggmap)
# p <- get_map("England", zoom = 6)
# ggmap(p) + geom_point(data=siteDetails, aes(x=longitude, y=latitude), color="red", size=3, alpha=0.5) + 
#   geom_point(data=x, aes(x=Longitude, y=Latitude), color="blue", size=3, alpha=0.5)

library(leaflet)
leaflet(data = openairStations) %>% addTiles() %>% addCircleMarkers(radius = 5, popup = ~Name)
