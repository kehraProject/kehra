################################################################################
### POLLUTION DATA obtained from DEFRA website #################################
################################################################################

################# ALL MONITORING STATIONS ######################################

# GET all DEFRA stations (active & closed):
# 1. Go to: http://uk-air.defra.gov.uk/networks/find-sites?view=advanced
# 2. Pollutant: any pollutant
# 3. Monitoring network: Search all available networks
# 4. tick the box: include closed monitoring sites in search
# 5. Leave the remaining fields empty/default
# 6. Click on Search networks and save the csv to:
# ~/Dropbox/Projects/kehra/data/Pollution/uk-air-search-results-all.csv

DEFRAstations <- read.csv("data/Pollution/uk-air-search-results-all.csv")

data <- DEFRAstations[-c(which(is.na(DEFRAstations$Longitude) | 
                                 is.na(DEFRAstations$Latitude))),]

saveRDS(data,"data/Pollution/DEFRAcatalogue.rds")
rm(DEFRAstations)

### THERE ARE SOME INCONSISTENCIES, BUILD THE CATALOGUE FOR ENGLAND ONLY ###

library(rgdal)
library(sp)
library(ggplot2)

adm1 <- as(readRDS("data/GEO/UK/AdminBoundaries/NUTS_2013/UK_nuts1.rds"),"SpatialPolygons")

### All sites (include closed monitoring) ###
temp <- read.csv("data/Pollution/DEFRA_England_all/uk-air-stations_London_UKI.csv")
temp$UKNUTS1 <- "UKI"
x <- temp
temp <- read.csv("data/Pollution/DEFRA_England_all/uk-air-stations_SouthEast_UKJ.csv")
temp$UKNUTS1 <- "UKJ"
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_all/uk-air-stations_SouthWest_UKK.csv")
temp$UKNUTS1 <- "UKK"
temp[temp$UK.AIR.ID=="UKA00034","UKNUTS1"] <- "UKM" # !!! inconsistency !!!
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_all/uk-air-stations_Eastern_UKH.csv")
temp$UKNUTS1 <- "UKJ"
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_all/uk-air-stations_EastMidlands_UKF.csv")
temp$UKNUTS1 <- "UKF"
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_all/uk-air-stations_NorthEast_UKC.csv")
temp$UKNUTS1 <- "UKC"
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_all/uk-air-stations_NorthWest_UKD.csv")
temp$UKNUTS1 <- "UKD"
temp <- temp[complete.cases(temp[,c("Latitude","Longitude")]),]
which(temp$Longitude>-2.1)
j <- c(54,55,141,142,143,144,145,146,147)
temp[j,"UKNUTS1"] <- "UKC" # !!! inconsistency !!!
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_all/uk-air-stations_WestMidlands_UKG.csv")
temp$UKNUTS1 <- "UKG"
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_all/uk-air-stations_Yorkshire_UKE.csv")
temp$UKNUTS1 <- "UKE"
x <- rbind(x, temp)

# TESTS
temp <- temp[complete.cases(temp[,c("Latitude","Longitude")]),]
spdf <- SpatialPointsDataFrame(coords = data.frame(temp$Longitude, 
                                                   temp$Latitude),
                               data = temp,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
spdf2 <- as(spdf, "SpatialPoints")
plot(adm1) ; plot(spdf, col="red", add=TRUE)
#plot(spdf2[j], col="blue", add=TRUE)

x <- x[complete.cases(x[,c("Latitude","Longitude")]),]
spdf <- SpatialPointsDataFrame(coords = data.frame(x$Longitude, 
                                                   x$Latitude),
                               data = x,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
saveRDS(spdf,"~/Dropbox/Projects/kehra/data/Pollution/DEFRAcatEng_all.rds")

# There are a total number of stations:
dim(spdf@data)[1] # 2794

# Only active monitoring
temp <- read.csv("data/Pollution/DEFRA_England_active/uk-air-GreaterLondon.csv")
temp$UKNUTS1 <- "UKI"
x <- temp
temp <- read.csv("data/Pollution/DEFRA_England_active/uk-air-SouthEast.csv")
temp$UKNUTS1 <- "UKJ" # !!! unidentified inconsistency !!!
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_active/uk-air-SouthWest.csv")
temp$UKNUTS1 <- "UKK"
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_active/uk-air-Eastern.csv")
temp$UKNUTS1 <- "UKJ"
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_active/uk-air-EastMidlands.csv")
temp$UKNUTS1 <- "UKF"
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_active/uk-air-NorthEast.csv")
temp$UKNUTS1 <- "UKC"
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_active/uk-air-NorthWest.csv")
temp$UKNUTS1 <- "UKD"
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_active/uk-air-WestMidlands.csv")
temp$UKNUTS1 <- "UKG"
x <- rbind(x, temp)
temp <- read.csv("data/Pollution/DEFRA_England_active/uk-air-Yorkshire.csv")
temp$UKNUTS1 <- "UKE"
x <- rbind(x, temp)

# TESTS
x <- x[complete.cases(x[,c("Latitude","Longitude")]),]
spdf <- SpatialPointsDataFrame(coords = data.frame(x$Longitude, 
                                                   x$Latitude),
                               data = x,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
spdf2 <- as(spdf, "SpatialPoints")
plot(adm1) ; plot(spdf, col="red", add=TRUE)
# plot(spdf2[j], col="blue", add=TRUE)
saveRDS(spdf,"~/Dropbox/Projects/kehra/data/Pollution/DEFRAcatEng_active.rds")

# There are a total number of active stations:
dim(spdf@data)[1] # 143

### ONLY ACTIVE SITES BELONGING TO AURN
x <- read.csv("data/Pollution/uk-air-AURN_England_active.csv")
x <- x[complete.cases(x[,c("Latitude","Longitude")]),]
spdf <- SpatialPointsDataFrame(coords = data.frame(x$Longitude, 
                                                   x$Latitude),
                               data = x,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
saveRDS(spdf,"~/Dropbox/Projects/kehra/data/Pollution/DEFRAcatEng_active_AURN.rds")

######### MAKE A MAP OF THE AURN ACTIVE STATIONS ###############################
library(ggmap)

# for theme_map
devtools::source_gist("33baa3a79c5cfef0f6df")

# Load data
df <- readRDS("data/Pollution/DEFRAcatEng_active_AURN.rds")

myMAP <- get_map("birmingham, uk",zoom=6)
ggmap(myMAP)+
  geom_point(data = data.frame(df), aes(x = Longitude, y = Latitude),
             alpha=0.5 , color = "red") +
  xlab("Longitude") + ylab("Latitude")

# OR

ggmap(get_googlemap(center = as.numeric(geocode("england")),   #"london, uk"
                    zoom = 6), # color = 'bw', 
      extent = "device") +
  geom_point(aes(x=Longitude, y=Latitude ),
             data=data.frame(df), color="red", size=2)

################### OBSERVED POLLUTANTS: HARLINGTON STATION ####################
library(openair)
library(zoo)

## the labels - same for all species 
labels <- c("1 - Low", "2 - Low", "3 - Low", "4 - Moderate", 
            "5 - Moderate", "6 - Moderate", "7 - High", 
            "8 - High", "9 - High", "10 - Very High")
o3.breaks <-c(0, 34, 66, 100, 121, 141, 160, 188, 214, 240, 500) 
no2.breaks <- c(0, 67, 134, 200, 268, 335, 400, 468, 535, 600, 1000) 
pm10.breaks <- c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000) 
pm25.breaks <- c(0, 12, 24, 35, 42, 47, 53, 59, 65, 70, 1000)

dat <- importAURN(site = "HRL", year = 2009:2015, hc = TRUE)

## no2 index example 
calendarPlot(dat, year = 2015, pollutant = "no2", labels = labels, 
             breaks = no2.breaks, statistic = "max", cols = "jet")
## for PM10 or PM2.5 we need the daily mean concentration (fixed 24 hour mean)
calendarPlot(dat, year = 2009, pollutant = "pm10", labels = labels, 
             breaks = pm10.breaks, statistic = "mean", cols = "jet",
             annotate = "value", lim =50, col.lim = c("black", "orange"))
## for ozone, need the rolling 8-hour mean 
dat <- rollingMean(dat, pollutant = "o3", hours = 8) 
calendarPlot(dat, year = 2015, pollutant = "rolling8o3", labels = labels, 
             breaks = o3.breaks, statistic = "max", cols = "jet")

# TREND ANALYSIS
TheilSen(dat, pollutant = "o3", ylab = "ozone (ppb)", deseason = TRUE)

# CORRELATION OF POLLUTANTS
## now it is possible to see the hydrocarbons that behave most 
## similarly to one another 
corPlot(dat[,-c(8:11,17:20)], dendrogram = TRUE)

################# DAILY MAX CONCENTRATION YEAR 2014 ENGLAND ####################
library(reshape2)

stationCatalogue <- "~/Dropbox/Projects/kehra/data/Pollution/DEFRAcatalogue.rds"

cleanupDEFRAdata <- function(inputFile, stationCatalogue){
  
  # For testing
  # inputFile <- "~/Dropbox/Projects/kehra/data/Pollution/6504701367.csv"
  # stationCatalogue <- "~/Dropbox/Projects/kehra/data/Pollution/DEFRAcatalogue.rds"
  
  # Extract station names, dates and recorded values
  data <- readRDS(stationCatalogue)
  stationNames <- read.csv(inputFile, skip=3, nrows = 1, header = FALSE)
  stationValues <- read.csv(inputFile, skip=6, header = FALSE)
  myDates <- as.character(stationValues[,1])
  
  # Get 
  nonNAindexes <- which(!is.na(stationNames)) # recordings
  stationNames <- as.character(unlist(stationNames[nonNAindexes]))
  stationIndexes <- which(data$Site.Name %in% stationNames)
  
  pollutant <- data.frame(cbind(myDates,stationValues[,nonNAindexes]))
  names(pollutant) <- c("Date", as.character(data$UK.AIR.ID[stationIndexes]))
  pollutant <- melt(pollutant,id.vars = "Date")
  pollutant$value <- as.numeric(as.character(pollutant$value))
  
  pollutant$Z <- pollutant$Region <- pollutant$Site <-
    pollutant$Latitude <- pollutant$Longitude <- NA
  for (i in 1:dim(pollutant)[1]){
    idx <- as.numeric(as.character(which(data$UK.AIR.ID %in% pollutant[i,"variable"])))
    pollutant$Z[i] <- as.numeric(as.character(data$Altitude..m.[idx])) # altitude
    pollutant$Region[i] <- as.character(data$Zone[idx]) # region, e.g. south
    pollutant$Site[i] <- as.character(data$Site.Name[idx]) # region, e.g. south
    pollutant$Longitude[i] <- as.numeric(as.character(data$Longitude[idx])) # x
    pollutant$Latitude[i] <- as.numeric(as.character(data$Latitude[idx])) # y
  }
  
  return(pollutant)
  
}

################# PARTICULATE MATTER 10 ########################################
inputFile <- "~/Dropbox/Projects/kehra/data/Pollution/6504701367.csv"
pollutant <- cleanupDEFRAdata(inputFile, stationCatalogue)
pollutant <- cbind(pollutant,"Pollutant"="PM10")

saveRDS(pollutant, "~/Dropbox/Projects/kehra/data/Pollution/PM10.rds")

################# PARTICULATE MATTER 2.5 #######################################
inputFile <- "~/Dropbox/Projects/kehra/data/Pollution/6504711268.csv"
pollutant <- cleanupDEFRAdata(inputFile, stationCatalogue)
pollutant <- cbind(pollutant,"Pollutant"="PM2p5")

saveRDS(pollutant, "~/Dropbox/Projects/kehra/data/Pollution/PM2p5.rds")

################# CARBON MONOXIDE ##############################################
inputFile <- "~/Dropbox/Projects/kehra/data/Pollution/6504731310.csv"
pollutant <- cleanupDEFRAdata(inputFile, stationCatalogue)
pollutant <- cbind(pollutant,"Pollutant"="CO")

saveRDS(pollutant, "~/Dropbox/Projects/kehra/data/Pollution/CO.rds")

################# OZONE ########################################################
inputFile <- "~/Dropbox/Projects/kehra/data/Pollution/65047413812.csv"
pollutant <- cleanupDEFRAdata(inputFile, stationCatalogue)
pollutant <- cbind(pollutant,"Pollutant"="O3")

saveRDS(pollutant, "~/Dropbox/Projects/kehra/data/Pollution/O3.rds")

################# NITROGEN DIOXIDE #############################################
inputFile <- "~/Dropbox/Projects/kehra/data/Pollution/65047513414.csv"
pollutant <- cleanupDEFRAdata(inputFile, stationCatalogue)
pollutant <- cbind(pollutant,"Pollutant"="NO2")

saveRDS(pollutant, "~/Dropbox/Projects/kehra/data/Pollution/NO2.rds")

################# SULPHUR DIOXIDE ##############################################
inputFile <- "~/Dropbox/Projects/kehra/data/Pollution/65047611415.csv"
pollutant <- cleanupDEFRAdata(inputFile, stationCatalogue)
pollutant <- cbind(pollutant,"Pollutant"="SO2")

saveRDS(pollutant, "~/Dropbox/Projects/kehra/data/Pollution/SO2.rds")

########### BUILD POLLUTION DATASET ############################################
pm10 <- readRDS("~/Dropbox/Projects/kehra/data/Pollution/PM10.rds")
pm25 <- readRDS("~/Dropbox/Projects/kehra/data/Pollution/PM2p5.rds")
co   <- readRDS("~/Dropbox/Projects/kehra/data/Pollution/CO.rds")
o3   <- readRDS("~/Dropbox/Projects/kehra/data/Pollution/O3.rds")
no2  <- readRDS("~/Dropbox/Projects/kehra/data/Pollution/NO2.rds")
so2  <- readRDS("~/Dropbox/Projects/kehra/data/Pollution/SO2.rds")

temp <- rbind(pm10, pm25, co, o3, no2, so2)

library(tidyr)
df <- spread(temp, Pollutant, value)
names(df) <- c("Date", "id", "Longitude", "Latitude", "Site", "Region", "Z", 
               "PM10", "PM2p5", "CO", "O3", "NO2", "SO2")

saveRDS(df, "~/Dropbox/Projects/kehra/data/Pollution/allPollutants.rds")

################################################################################
### VISUALISATION (OPTIONAL) ###################################################
################################################################################

######### BOX PLOTS ############################################################
#pollutant <- "O3" # can be "PM10", "PM2p5", "CO", "O3", "NO2", "SO2"

library(manipulate)

plotPollutants <- function(pollutant, type){
  
  df <- readRDS("~/Dropbox/Projects/kehra/data/Pollution/allPollutants.rds")
  df <- df[!is.na(eval(parse(text=paste("df$",pollutant,sep="")))),]
  
  if (pollutant == "PM10") threshold <- 40 # ug/m3 (50 can be exceeded 35 d/y)
  if (pollutant == "PM2p5") threshold <- 25 # ug/m3 
  if (pollutant == "CO") threshold <- 10 # ug/m3 
  if (pollutant == "O3") threshold <- 120 # ug/m3 (can be exceeded 25 d/3y)
  if (pollutant == "NO2") threshold <- 40 # ug/m3
  if (pollutant == "SO2") threshold <- 125 # ug/m3 (can be exceeded 3 d/y)
  
  if (type == "Regions") {
    p <- ggplot(df, aes_string(x = "Region", y = pollutant),
                environment = environment()) + # use variables in the call env
      geom_boxplot() +
      geom_hline(aes(yintercept=threshold, col="red")) + 
      xlab("") + ggtitle(paste(pollutant,"\n")) +
      ylab(expression(paste("Concentration, ", mu, "g/", m^3, sep=""))) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  if (type == "Sites") {
    p <- ggplot(df, aes_string(x = "Site", y = pollutant),
                environment = environment()) + # use variables in the call env
      geom_boxplot() +
      geom_hline(aes(yintercept=threshold, col="red")) + 
      xlab("") + ggtitle(paste(pollutant,"\n")) +
      ylab(expression(paste("Concentration, ", mu, "g/", m^3, sep=""))) +
      facet_wrap(~ Region, scales = "free") +  
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  plot(p)
  
}

manipulate(plotPollutants(pollutant, type), 
           pollutant = picker("PM10", "PM2p5", "CO", "O3", "NO2", "SO2"),
           type = picker("Regions", "Sites"))


pdf(file=paste("~/Dropbox/Projects/kehra/images/boxplots", pollutant,
               "_regions.pdf", sep=""), width=10, height=7)
ggplot(df, aes_string(x = "Region", y = pollutant)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=threshold, col="red")) + 
  xlab("") + ggtitle(paste(pollutant,"\n")) +
  ylab(expression(paste("Concentration, ", mu, "g/", m^3, sep=""))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

pdf(file=paste("~/Dropbox/Projects/kehra/images/boxplots", pollutant,
               "_sites.pdf", sep=""), width=10, height=7)
ggplot(df, aes_string(x = "Site", y = pollutant)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=threshold, col="red")) + 
  xlab("") + ggtitle(paste(pollutant,"\n")) +
  ylab(expression(paste("Concentration, ", mu, "g/", m^3, sep=""))) +
  facet_wrap(~ Region, scales = "free")
dev.off()

####

```{r, eval=TRUE, include=FALSE}
library(devtools)
install_github("cvitolo/r_rdefra", subdir = "rdefra")

library(rdefra)
# All the stations ever used are (as per Feb 2016)
# allS <- read.csv("~/kehra/data/Pollution/uk-air-search-results-all.csv")
# monitoringStations <- allS
# save(monitoringStations, file="~/kehra/scripts/Packages/RDEFRA/data/monitoringStations.rda", compress='xz')
data(stations)

# Check whether there are hourly data available
IDstationHdata <- stations$SiteID[ which(!is.na(stations$SiteID)) ]
# Catalogue of stations with hourly data
stationsHdata <- stations[which(!is.na(stations$SiteID)),]

library(openair)
stationsHdataOpenair1 <- importMeta(source = "aurn", all = TRUE)
stationsHdataOpenair2 <- importMeta(source = "kcl", all = TRUE)
stationsHdataOpenair3 <- importMeta(source = "saqn", all = TRUE)
openairAll <- c(stationsHdataOpenair1$code, stationsHdataOpenair2$code, stationsHdataOpenair3$code)
length(unique(openairAll))
all(stationsHdata$SiteID %in% openairAll)

stationsHdata$SiteID[which(!(stationsHdata$SiteID %in% openairAll))]

stationsHdata$SiteID[which(!(stationsHdata$SiteID %in% stationsHdataOpenair1$code))]

all(stationsHdataOpenair1$code %in% stationsHdataOpenair2$code)
which(!(stationsHdataOpenair1$code %in% stationsHdataOpenair2$code))


# 
library(parallel)
library(plyr)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

system.time(myList <- parLapply(cl, IDstationHdata, 
                                get1Hdata, years=1999:2016))

stopCluster(cl)

df <- rbind.fill(myList)



# Openair stations containg hourly measurements are
x <- importMeta(source = "aurn", all = TRUE); x$From <- "AURN"
# y <- importMeta(source = "kcl", all = TRUE); y$From <- "KCL"
# w <- importMeta(source = "saqn", all = TRUE); w$From <- "SAQN"
# z <- rbind.fill(x,y,w)
# stations <- z[with(z, order(code)), ]
stations <- x[with(x, order(code)), ]
stations <- stations[complete.cases(stations[,c("latitude", "longitude")]),]
# saveRDS("~/kehra/data/Pollution/openairStations.rds")
```

There are `r dim(allS)[1]` stations within the UK-AIR (Air Information Resource) database, but only `r dim(allSxy)[1]` of them have coordinates of exact location and are shown in the map below.

```{r}
leaflet(data = allSxy) %>% addTiles() %>% 
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 0.5)
```

We then build a catalogue of all the site information available:
  ```{r}
selectedRows <- match(stations$site, allSxy$Site.Name)
stations <- stations[!is.na(selectedRows),]
selectedRows <- selectedRows[!is.na(selectedRows)]
stations$UKAIRID <- allSxy$UK.AIR.ID[selectedRows]
stations$EnvType <- allSxy$Environment.Type[selectedRows]
stations$Zone <- allSxy$Zone[selectedRows]
stations$StartDate <- allSxy$Start.Date[selectedRows]
stations$EndDate <- allSxy$End.Date[selectedRows]
stations$Altitude <- allSxy$Altitude..m.[selectedRows]
stations$Network <- allSxy$Networks[selectedRows]
stations$EndDate <- allSxy$End.Date[selectedRows]
stations$Region <- NA
for (x in 1:dim(stations)[1]){
  
  zone <- as.character(stations$Zone[x])
  
  if (stations$Zone[x] == "South West" | stations$Zone[x] == "Bristol Urban Area" | stations$Zone[x] == "Bournemouth Urban Area") {
    stations$Region[x] <- "South West"
  }
  
  if (stations$Zone[x] == "South East" | stations$Zone[x] == "Brighton/Worthing/Littlehampton" | stations$Zone[x] == "Reading/Wokingham Urban Area" | stations$Zone[x] == "Portsmouth Urban Area" | stations$Zone[x] == "Southampton Urban Area") {
    stations$Region[x] <- "South East"
  }
  if (stations$Zone[x] == "Greater London Urban Area") {
    stations$Region[x] <- "London"
  }
  if (stations$Zone[x] == "East" | stations$Zone[x] == "Eastern" | stations$Zone[x] == "Southend Urban Area") {
    stations$Region[x] <- "East"
  }
  if (stations$Zone[x] == "East Midlands" | stations$Zone[x] == "Nottingham Urban Area" | stations$Zone[x] == "Leicester Urban Area") {
    stations$Region[x] <- "East Midlands"
  }
  if (stations$Zone[x] == "West Midlands" | stations$Zone[x] == "West Midlands Urban Area" | stations$Zone[x] == "Coventry/Bedworth" | stations$Zone[x] == "The Potteries" ) {
    stations$Region[x] <- "West Midlands"
  }
  if (stations$Zone[x] == "Yorkshire & Humberside" | stations$Zone[x] == "West Yorkshire Urban Area" | stations$Zone[x] == "Kingston upon Hull" | stations$Zone[x] == "Sheffield Urban Area") {
    stations$Region[x] <- "Yorkshire and The Humber"
  }
  if (stations$Zone[x] == "North West" | stations$Zone[x] == "Greater Manchester Urban Area" | stations$Zone[x] == "North West & Merseyside" | stations$Zone[x] == "Birkenhead Urban Area" | stations$Zone[x] == "Preston Urban Area" | stations$Zone[x] == "Blackpool Urban Area" | stations$Zone[x] == "Liverpool Urban Area") {
    stations$Region[x] <- "North West"
  }
  if (stations$Zone[x] == "North East" | stations$Zone[x] == "Teesside Urban Area" | stations$Zone[x] == "Tyneside") {
    stations$Region[x] <- "North East"
  }
  if (stations$Zone[x] == "Belfast Metropolitan Urban Area" | stations$Zone[x] == "Northern Ireland" | stations$Zone[x] == "West Ireland") {
    stations$Region[x] <- "Northern Ireland"
  }
  if (stations$Zone[x] == "Cardiff Urban Area" | stations$Zone[x] == "North Wales" | stations$Zone[x] == "South Wales" | stations$Zone[x] == "Swansea Urban Area") {
    stations$Region[x] <- "Wales"
  }
  if (stations$Zone[x] == "Central Scotland" | stations$Zone[x] == "Edinburgh Urban Area" | stations$Zone[x] == "Glasgow Urban Area" | stations$Zone[x] == "Highland" | stations$Zone[x] == "North East Scotland" | stations$Zone[x] == "Scottish Borders") {
    stations$Region[x] <- "Scotland"
  }
  
}
#stations <- stations[,c("UKAIRID", "From", "Network", "code", "site", "site.type", "zone_name", "zone_id", "la_region", "la_region_id", "Address", "la_id", "Zone", "Region", "EnvType", "Network", "StartDate", "EndDate", "date_started", "date_ended", "OpeningDate", "ClosingDate", "ratified_to", "Authority", "os_grid_x", "os_grid_y", "latitude", "longitude", "altitude", "parameter")]
```

Hourly measurements of PM10, PM2.5, O3, CO, NO2 and SO2 are available only in a limited number of stations, the vast majority of which belongs to the Automatic Urban and Rural Monitoring Network (AURN), some others where retrieved from the KCL and SAQN databases and are shown below.

```{r}
leaflet(data = stations) %>% addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 0.5, popup=~site)
```

Extract time series for the above stations.
```{r, message=FALSE, warning=FALSE, include=FALSE}
# source('~/kehra/scripts/GetData/importAURN.R')
pollutionDATA <- importAURN(site = stations$code[stations$From == "AURN"][1:30], year = 1979:2014)
# saveRDS(pollutionDATA, "~/kehra/data/Pollution/pollution.rds")
```

