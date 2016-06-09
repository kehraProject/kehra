library(foreign)
library(rgeos)
library(leaflet)

adm0 <- readRDS("data/adm0.rds")

adm1 <- readRDS("data/adm1.rds")
# For labels
# centroids <- gCentroid(adm1, byid=TRUE)
# centroidLons <- coordinates(centroids)[,1]
# centroidLats <- coordinates(centroids)[,2]
# adm2 <- readRDS("data/adm2.rds")
AralSea <- readRDS("data/AralSea.rds")
SleepingTowns <- readRDS("data/SleepingTowns.rds")
mineralResources <- readRDS("data/mineralResources.rds")

bbox <- data.frame("Name"=adm0@data$Code0,  
                   "latmin"=c(40.417, 48.5), 
                   "latmax"=c(55.428, 64.067), 
                   "longmin"=c(46.583, -13.683), 
                   "longmax"=c(90, 3.858),
                   "latmid"=c(50,55),
                   "longmid"=c(65,-5),
                   "zoomlevel"=c(5,6))

load("data/nuclearSites.rda")
populatedPlaces <- read.dbf("data/ne_10m_populated_places.dbf", as.is = FALSE)
populatedPlaces <- populatedPlaces[with(populatedPlaces, order(NAME)),]
monitoredCities <- populatedPlaces[which(populatedPlaces$NAME %in% 
                                           c("Almaty", "Astana", "Atyrau",
                                             "Borovoye", "Oral",
                                             "Petropavlovsk", "Taraz")),]

listOfDatasets <- data.frame(Label=c("Vulnerability Index"),
                             Location=c("data/VulnerabilityIndex.rds"),
                             stringsAsFactors = FALSE)
