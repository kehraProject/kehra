setwd("~/Dropbox/Projects/kehra/data/GEO/UK/MonitoringSites")

# ignore the number of networks!
AllStations <- read.csv("uk-air-search-results-all.csv")
print(paste("Found", dim(AllStations)[1], "stations across", 
            length(unique(AllStations$Networks)), "networks"))

O3Stations <- read.csv("uk-air-search-results-O3.csv")
print(paste("Found", dim(O3Stations)[1], "stations across", 
            length(unique(O3Stations$Networks)), "networks"))

PM2p5Stations <- read.csv("uk-air-search-results-PM2p5.csv")
print(paste("Found", dim(PM2p5Stations)[1], "stations across", 
            length(unique(PM2p5Stations$Networks)), "networks"))

PM10Stations <- read.csv("uk-air-search-results-PM10.csv")
print(paste("Found", dim(PM10Stations)[1], "stations across", 
            length(unique(PM10Stations$Networks)), "networks"))

COStations <- read.csv("uk-air-search-results-CO.csv")
print(paste("Found", dim(COStations)[1], "stations across", 
            length(unique(COStations$Networks)), "networks"))

SO2Stations <- read.csv("uk-air-search-results-SO2.csv")
print(paste("Found", dim(SO2Stations)[1], "stations across", 
            length(unique(SO2Stations$Networks)), "networks"))

NOxStations <- read.csv("uk-air-search-results-NOx.csv")
print(paste("Found", dim(NOxStations)[1], "stations across", 
            length(unique(NOxStations$Networks)), "networks"))
