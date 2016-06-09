# Check for missing dependencies and install+load them
packs <- c("devtools", "ggplot2", "leaflet", "dygraphs", "leaflet", "openair",
           "plyr", "dplyr")
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

################################################################################
# POLLUTION DATA ###############################################################
################################################################################

# Pollution data in the UK is available from the DEFRA UK-AIR website
# Here is the list of major stations
library(dplyr)
library(devtools)
install_github("cvitolo/r_rdefra", subdir="rdefra")
install_github("davidcarslaw/openair")

# Get metadata from the openair package ########################################
library(openair)

stationsOPENAIR1 <- importMeta(source = "aurn", all = TRUE) %>%
  mutate(sourceDB = "aurn") 
stationsOPENAIR2 <- importMeta(source = "kcl", all = TRUE) %>%
  mutate(sourceDB = "kcl") 
stationsOPENAIR3 <- importMeta(source = "saqn", all = TRUE) %>%
  mutate(sourceDB = "saqn") 

# join tables 
commonColumns <- c("code", "site", "latitude", "longitude", "sourceDB")
stationsOPENAIR <- full_join(stationsOPENAIR1, stationsOPENAIR2, by = commonColumns) %>%
  full_join(stationsOPENAIR3, by = commonColumns)

stationsOPENAIR <- stationsOPENAIR[!is.na(stationsOPENAIR$latitude) | !is.na(stationsOPENAIR$longitude),]

stationsOPENAIR <- stationsOPENAIR[,c("code", "site", "latitude", "longitude", 
                                      "altitude.x", "site.type.x", "zone_name", 
                                      "la_region.x", "sourceDB")]
names(stationsOPENAIR) <- c("SiteID", "Site.Name", "Latitude", "Longitude",
                            "Altitude", "Environment.Type", "Zone", "Region", 
                            "sourceDB")

stationsOPENAIR <- stationsOPENAIR[-which(duplicated(stationsOPENAIR$SiteID)),] 
stationsOPENAIR$Altitude <- as.numeric(stationsOPENAIR$Altitude)

rm(stationsOPENAIR1, stationsOPENAIR2, stationsOPENAIR3, commonColumns)

# Get metadata from the rdefra package #########################################
library(rdefra)

data(stationsNew)

stationsNew$sourceDB <- 'rdefra' 
stationsNew$Region <- NA

stationsNew <- stationsNew[!is.na(stationsNew$SiteID),]
stationsNew <- stationsNew[!is.na(stationsNew$Latitude) | 
                             !is.na(stationsNew$Longitude),]

stationsNew <- stationsNew[,c("SiteID", "Site.Name", "Latitude", "Longitude",
                              "Altitude..m.", "Environment.Type", "Zone", 
                              "Region", "sourceDB")]

names(stationsNew) <- c("SiteID", "Site.Name", "Latitude", "Longitude",
                        "Altitude", "Environment.Type", "Zone", "Region", 
                        "sourceDB")

stationsNew$Site.Name <- as.character(stationsNew$Site.Name)
stationsNew$Environment.Type <- as.character(stationsNew$Environment.Type)
stationsNew$Zone <- as.character(stationsNew$Zone)
stationsNew$Region <- as.character(stationsNew$Region)

commonColumns <-  c("SiteID", "Site.Name", "Latitude", "Longitude",
                    "Altitude", "Environment.Type", "Zone", "Region", 
                    "sourceDB")
stations <- full_join(stationsNew, stationsOPENAIR, by = commonColumns)

length(unique(stations$SiteID))

x <- data.frame('SiteID' = union(unique(stationsOPENAIR$SiteID), 
                                 stationsNew$SiteID))

stationsNew
names(stationsNew)

names(shortAURNmetadata)[which(names(shortAURNmetadata)=="site_name")] <- "Site.Name"
names(shortAURNmetadata)[which(names(shortAURNmetadata)=="latitude")] <- 'Latitude'
names(shortAURNmetadata)[which(names(shortAURNmetadata)=="longitude")] <- 'Longitude'
names(shortAURNmetadata)[which(names(shortAURNmetadata)=="altitude")] <- 'Altitude'
names(stationsNew)[which(names(stationsNew)=="Altitude..m.")] <- 'Altitude'
names(shortAURNmetadata)[which(names(shortAURNmetadata)=="site_id")] <- 'SiteID'

library(dplyr)
y <- left_join(x, stationsNew, by = "SiteID")
y <- left_join(y, shortAURNmetadata, by = "SiteID")
y <- as.data.frame(as.matrix(y),stringsAsFactors=F)
y <- y[,-which(names(y)=="UK.AIR.ID")]
y <- y[,-which(names(y)=="EU.Site.ID")]
y <- y[,-which(names(y)=="EMEP.Site.ID")]
y <- y[,-which(names(y)=="Start.Date")]
y <- y[,-which(names(y)=="date_started")]
y <- y[,-which(names(y)=="End.Date")]
y <- y[,-which(names(y)=="date_ended")]
y <- y[,-which(names(y)=="ratified_to")]
y <- y[,-which(names(y)=="Networks")]
y <- y[,-which(names(y)=="AURN.Pollutants.Measured")]
y <- y[,-which(names(y)=="Site.Description")]
y <- y[,-which(names(y)=="la_region_id")]
y <- y[,-which(names(y)=="zone_id")]
names(y)[which(names(y)=="la_region")] <- 'Region'
str(y)

y[which(y$Site.Name.x != y$Site.Name.y), c("Site.Name.x","Site.Name.y")]
y <- y[,-which(names(y)=="Site.Name.y")]
names(y)[which(names(y)=="Site.Name.x")] <- 'Site.Name'

y[which(y$Latitude.x != y$Latitude.y), c("Latitude.x","Latitude.y")]
rows2Change <- which(is.na(y$Latitude.x)==TRUE)
y$Latitude.x[rows2Change] <- y$Latitude.y[rows2Change]
y <- y[,-which(names(y)=="Latitude.y")]
names(y)[which(names(y)=="Latitude.x")] <- 'Latitude'

y[which(y$Longitude.x != y$Longitude.y), c("Longitude.x","Longitude.y")]
rows2Change <- which(is.na(y$Longitude.x)==TRUE)
y$Longitude.x[rows2Change] <- y$Longitude.y[rows2Change]
y <- y[,-which(names(y)=="Longitude.y")]
names(y)[which(names(y)=="Longitude.x")] <- 'Longitude'

y[which(y$Altitude.x != y$Altitude.y), c("SiteID", "Altitude.x","Altitude.y")]
rows2Change <- which(is.na(y$Altitude.x)==TRUE)
y$Altitude.x[rows2Change] <- y$Altitude.y[rows2Change]
y <- y[,-which(names(y)=="Altitude.y")]
names(y)[which(names(y)=="Altitude.x")] <- 'Altitude'

y[which(y$Zone != y$zone_name), c("SiteID", "Zone","zone_name")]
rows2Change <- which(is.na(y$Zone)==TRUE)
y$Zone[rows2Change] <- y$zone_name[rows2Change]
y <- y[,-which(names(y)=="zone_name")]

y[which(y$Environment.Type != y$location_type), c("SiteID", "Environment.Type","location_type")]
y[which(y$Environment.Type == "Background Urban"), "Environment.Type"] <- "Urban Background"
y[which(y$Environment.Type == "Urban traffic"), "Environment.Type"] <- "Traffic Urban"
y[which(y$Environment.Type == "Urban Industrial"), "Environment.Type"] <- "Industrial Urban"
y[which(y$Environment.Type == "Suburban Background"), "Environment.Type"] <- "Background Suburban"
y[which(y$Environment.Type == "Rural Background"), "Environment.Type"] <- "Background Rural"
rows2Change <- which(is.na(y$Environment.Type)==TRUE)
y$Environment.Type[rows2Change] <- y$location_type[rows2Change]
y <- y[,-which(names(y)=="location_type")]

y$Latitude <- as.numeric(y$Latitude)
y$Longitude <- as.numeric(y$Longitude)
y$Altitude <- as.numeric(y$Altitude)
str(y)
stations <- y[,c("SiteID", "Site.Name", "Zone", "Region", 
                 "Environment.Type", "Latitude", "Longitude", "Altitude")]
rm(shortAURNmetadata, stationsNew, x, y, rows2Change)

library(raster) 
adm <- getData('GADM', country='GBR', level=1)
England <- adm[adm$NAME_1=='England',]
stationsSP <- SpatialPoints(stations[, c('Longitude', 'Latitude')], 
                            proj4string=CRS(proj4string(England)))

library(sp)
x <- over(stationsSP, England)[,1]
x <- which(!is.na(x))
stations <- stations[x,]
rm(adm,England,stationsSP,x)

library(leaflet)
leaflet(data = stations) %>% addTiles() %>% 
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude, 
                   radius = 0.5, color="red", popup = ~SiteID)

# Get missing altitude via point inspection
# or this online tool : https://www.daftlogic.com/sandbox-google-maps-find-altitude.htm
stations$Altitude[stations$SiteID == "BHAM"] <- 109
stations$Altitude[stations$SiteID == "CAMB"] <- 118
stations$Altitude[stations$SiteID == "MAHG"] <- 67
stations$Altitude[stations$SiteID == "NWBV"] <- 43

# Getting missing site name from defra website
stations$Site.Name[stations$SiteID == "BHAM"] <- "Birmingham Kerbside"
stations$Site.Name[stations$SiteID == "BARN"] <- "Barnsley"
stations$Site.Name[stations$SiteID == "BIR"] <- "Bircotes"
stations$Site.Name[stations$SiteID == "CAMB"] <- "Cambridge"
stations$Site.Name[stations$SiteID == "CAN"] <- "London Canvey"
stations$Site.Name[stations$SiteID == "CHIL"] <- "Chilworth"
stations$Site.Name[stations$SiteID == "CLL"] <- "Central London"
stations$Site.Name[stations$SiteID == "FEA"] <- "Featherstone"
stations$Site.Name[stations$SiteID == "HARR"] <- "London Harrow"
stations$Site.Name[stations$SiteID == "ISL"] <- "London Islington"
stations$Site.Name[stations$SiteID == "LDS"] <- "Leeds Potternewton"
stations$Site.Name[stations$SiteID == "RUGE"] <- "Rugeley"
stations$Site.Name[stations$SiteID == "STE"] <- "Stevenage"
stations$Site.Name[stations$SiteID == "WRAY"] <- "Wray"

stations <- stations[with(stations, order(SiteID)), ]
row.names(stations) <- NULL

# Fix few empty info
View(stations[with(stations, order(Latitude, Longitude)), 
              c("Site.Name", "Region", "Latitude", "Longitude")])

library(leaflet)
leaflet(data = stations[c(84, 85, 86),]) %>% addTiles() %>% addMarkers(~Longitude, ~Latitude, popup = ~as.character(paste(SiteID,Site.Name)))

which(is.na(stations$Site.Name))

stations$Region[157] <- 'Stockton-on-Tees'
stations$Region[168] <- 'Sunderland District'
stations$Region[c(93, 97)] <- 'Leicester City Council'
stations$Region[127] <- 'Nottingham City Council'
stations$Region[162] <- 'Stoke-on-trent City Council'
stations$Region[100] <- 'Lincoln City Council'
stations$Region[48] <- 'Chesterfield District'
stations$Region[190] <- 'Liverpool City Council'
stations$Region[110] <- 'Manchester City Council'
stations$Region[149] <- 'Sheffield City Council'
stations$Region[59] <- 'Doncaster'
stations$Region[35] <- 'Bury District'
stations$Region[58] <- 'Oldham Council'
stations$Region[17] <- 'Blackburn with Darwen'
stations$Region[86] <- 'Kingston-upon-hull City Council'
stations$Region[7] <- 'Bradford District'
stations$Region[179] <- 'Walsall District'
stations$Region[20] <- 'Sandwell District'
stations$Region[2] <-'Birmingham District'
stations$Region[144] <- 'Caradon District'
stations$Region[76] <- 'Honiton'
stations$Region[33] <- 'Brighton & Hove'
stations$Region[142] <- 'Southampton City Council'
stations$Region[24] <- 'Barnstaple'
stations$Region[46] <- 'Chilbolton'
stations$Region[172] <- 'London Borough of Richmond Upon Thames'
stations$Region[136] <- 'Reading'
stations$Region[60] <- 'London Borough of Ealing'
stations$Region[72] <- 'London Borough of Haringey'
stations$Region[105] <- 'Luton'
stations$Region[124:125] <- 'Northampton District'
stations$Region[92] <-'Warwick District'
stations$Region[53] <- 'Coventry City Council'

saveRDS(stations, "~/kehra/data/Pollution/stationsKEHRA.rds")

###############################################################################

stations <- readRDS("~/kehra/data/Pollution/stationsKEHRA.rds")

# Get time series
counter <- 0
for (id in stations$SiteID){
  print(id)
  counter <- counter + 1
  x <- get1Hdata(id, years = 1981:2014)
  if (counter == 1){
    myList <- x
  }else{
    myList <- rbind.fill(myList, x)
  }
}

saveRDS(myList, "~/kehra/data/Pollution/pollution_England_1981_2014.rds")

head(myList)
as.data.frame(names(myList))
# remove not relevant variables
pollutionDB <- myList[, c(1, 2, 8, 4, 15, 6, 10, 12, 3)]
names(pollutionDB) <- c("Date","time","SiteID",
                        "PM10","PM2.5","NO2","O3", "SO2", "CO")
pollutionDB$Date <- as.character(pollutionDB$Date)
pollutionDB$time <- as.character(pollutionDB$time)
pollutionDB$SiteID <- as.character(pollutionDB$SiteID)
pollutionDB$PM10h <- as.numeric(as.character(pollutionDB$PM10h))
pollutionDB$PM2.5h <- as.numeric(as.character(pollutionDB$PM2.5h))
pollutionDB$NO2 <- as.numeric(as.character(pollutionDB$NO2))
pollutionDB$O3 <- as.numeric(as.character(pollutionDB$O3))
pollutionDB$SO2 <- as.numeric(as.character(pollutionDB$SO2))
pollutionDB$CO <- as.numeric(as.character(pollutionDB$CO))

# remove rows with all NA
ind <- apply(pollutionDB[,4:9], 1, function(x) all(is.na(x)))
pollutionDB <- pollutionDB[!ind, ]
saveRDS(pollutionDB, "~/kehra/data/Pollution/pollutionDB.rds")

# Average temporal coverage

library(dplyr)
byStation <- group_by(pollutionDB, SiteID)
tempVariability <- summarise(byStation, countAll = n()/365/24)
dim(tempVariability)[1] # how many stations?
# min(tempVariability$count)
mean(tempVariability$count)
# max(tempVariability$count)

myRows <- which(!is.na(pollutionDB$O3))
byStation <- group_by(pollutionDB[myRows,], SiteID)
tempVariability <- summarise(byStation, count = n()/365/24)
dim(tempVariability)[1] # how many stations measure this pollutant?
# min(tempVariability$count)
mean(tempVariability$count)
# max(tempVariability$count)

myRows <- which(!is.na(pollutionDB$PM2.5))
byStation <- group_by(pollutionDB[myRows,], SiteID)
tempVariability <- summarise(byStation, count = n()/365/24)
dim(tempVariability)[1] # how many stations measure this pollutant?
# min(tempVariability$count)
mean(tempVariability$count)
# max(tempVariability$count)

myRows <- which(!is.na(pollutionDB$PM10))
byStation <- group_by(pollutionDB[myRows,], SiteID)
tempVariability <- summarise(byStation, count = n()/365/24)
dim(tempVariability)[1] # how many stations measure this pollutant?
# min(tempVariability$count)
mean(tempVariability$count)
# max(tempVariability$count)

myRows <- which(!is.na(pollutionDB$SO2))
byStation <- group_by(pollutionDB[myRows,], SiteID)
tempVariability <- summarise(byStation, count = n()/365/24)
dim(tempVariability)[1] # how many stations measure this pollutant?
# min(tempVariability$count)
mean(tempVariability$count)
# max(tempVariability$count))

myRows <- which(!is.na(pollutionDB$NO2))
byStation <- group_by(pollutionDB[myRows,], SiteID)
tempVariability <- summarise(byStation, count = n()/365/24)
dim(tempVariability)[1] # how many stations measure this pollutant?
# min(tempVariability$count)
mean(tempVariability$count)
# max(tempVariability$count))

myRows <- which(!is.na(pollutionDB$CO))
byStation <- group_by(pollutionDB[myRows,], SiteID)
tempVariability <- summarise(byStation, count = n()/365/24)
dim(tempVariability)[1] # how many stations measure this pollutant?
# min(tempVariability$count)
mean(tempVariability$count)
# max(tempVariability$count))

################################################################################
# ADD CLIMATE DATA #############################################################
################################################################################

rm(list=ls(all=TRUE))

# Load pre-calculated data
climate <- readRDS("~/kehra/data/Pollution/pollutionDB.rds")
stations <- readRDS("~/kehra/data/Pollution/stationsKEHRA.rds")
ids <- unique(climate$SiteID) # 164 stations

# CLIMATE DATA
# Climate data is available from ECMWF's ERA-INTERIM website 
library(ncdf4)
# library(RNetCDF)
library(raster)
# Time series
library(xts)

# Functions to calculate wind speed and direction
windSpeed <- function(u, v) {
sqrt(u^2 + v^2) 
}
windDir <- function(u, v) {
if (is.na(v) | is.na(u)) {
wd <- NA
}else{
if(v > 0)         wd <- ((180 / pi) * atan(u/v) + 180)
if(u < 0 & v < 0) wd <- ((180 / pi) * atan(u/v) + 0)
if(u > 0 & v < 0) wd <- ((180 / pi) * atan(u/v) + 360)
}
return(wd)
}

### ADD CLIMATE VARIABLES TO POLLUTION DATA ####################################

# For testing only
# myYear <- 1997
# id <- ids[1]

# add placeholders to store climate information
climate$TEMP <- climate$WS <- climate$WD <- climate$BLH <- climate$PREC <- climate$SR <- NA
climate$Year <- format(as.Date(climate$Date, format = "%d-%m-%Y"), "%Y")
climate$datetime <- as.POSIXlt(paste(climate$Date, climate$time), 
                               format = "%d-%m-%Y %H:%M")

# Loop through years and station IDs
for (myYear in 1981:2014){
  
  for (id in ids){
    
    print(paste(myYear,id))
    
    myrows <- which(climate$SiteID == id & climate$Year == myYear)
    
    if (length(myrows) > 0){
      
      counter <- which(stations$SiteID == id)
      points <- SpatialPoints(stations[counter, c('Longitude', 'Latitude')], 
                              proj4string=CRS('+proj=longlat +datum=WGS84'))
      
      # Set name of file containing UVT for given year
      fname <- paste("~/kehra/data/Climate/UVT", myYear, ".nc", sep="")
      
      # TEMPERATURE ############################################################
      # Extract temperature at the station and set the length of dates vectors
      
      # 2 m Temperature (K)
      t2m <- rotate(brick(x = fname, varname="t2m"))
      tmp <- as.vector(extract(t2m, points))
      
      # Dates vectors (this dates will be used with all variables)
      dates1H <- seq(as.POSIXct(paste(myYear,"-01-01 01:00:00", sep = "")), 
                     as.POSIXct(paste(myYear,"-12-31 24:00:00", sep = "")),
                     length.out = length(tmp)*6)
      dates6H <- seq(as.POSIXct(paste(myYear,"-01-01 06:00:00", sep = "")), 
                     as.POSIXct(paste(myYear,"-12-31 24:00:00", sep = "")),
                     length.out = length(tmp))
      dates12H <- seq(as.POSIXct(paste(myYear,"-01-01 12:00:00", sep = "")), 
                      as.POSIXct(paste(myYear,"-12-31 24:00:00", sep = "")),
                      length.out = length(tmp)/2)
      
      # Generate TS from t2m
      tmpTS1 <- xts(tmp, order.by = dates6H)
      # Generate empty TS from climate dataframe (this is used for all vars)
      emptyTS <- xts(climate$TEMP[myrows], order.by = climate$datetime[myrows])
      # Right join to get data from t2m for the dates in climate dataframe
      tmpTS <- merge.xts(tmpTS1, emptyTS, join = "right")
      # Fill in the climate dataframe with t2m values
      climate$TEMP[myrows] <- as.numeric(tmpTS[,1])
      
      rm(t2m, tmpTS, tmpTS1)
      
      # WIND ###################################################################
      # Wind U component (m/s)
      u10 <- rotate(brick(x = fname, varname="u10"))
      # Wind V component (m/s)
      v10 <- rotate(brick(x = fname, varname="v10"))
      
      u <- as.vector(extract(u10, points))
      v <- as.vector(extract(v10, points))
      
      # Generate TS for wind speed from u10 and v10
      tmpTS1 <- xts(windSpeed(u,v), order.by = dates6H)
      # Right join to get data for the dates in climate dataframe
      tmpTS <- merge.xts(tmpTS1, emptyTS, join = "right")
      # Fill in the climate dataframe with ws values
      climate$WS[myrows] <- as.numeric(tmpTS[,1])
      rm(tmpTS, tmpTS1)
      
      # Generate TS for wind direction from u10 and v10
      wd <- c()
      for (i in 1:length(u)) {
        wd[i] <- windDir(u[i], v[i])
      }
      tmpTS1 <- xts(windSpeed(u,v), order.by = dates6H)
      # Right join to get data for the dates in climate dataframe
      tmpTS <- merge.xts(tmpTS1, emptyTS, join = "right")
      # Fill in the climate dataframe with ws values
      climate$WD[myrows] <- as.numeric(tmpTS[,1])
      rm(u, v, u10, v10, tmpTS, tmpTS1, fname)
      
      # PRECIPITATION ##########################################################
      fname <- paste("~/kehra/data/Climate/PBR", myYear, ".nc", sep="")
      # Total precipitation (m)
      tp <- rotate(brick(x = fname, varname="tp"))
      tmp <- as.vector(extract(tp, points))
      
      # Generate TS for precipitation from tp
      tmpTS1 <- xts(tmp, order.by = dates12H)
      # Right join to get data for the dates in climate dataframe
      tmpTS <- merge.xts(tmpTS1, emptyTS, join = "right")
      # Fill in the climate dataframe with ws values
      climate$PREC[myrows] <- as.numeric(tmpTS[,1])
      climate$PREC[myrows][which(climate$PREC[myrows] < 0)] <- 0
      rm(tp, tmp, tmpTS, tmpTS1)
      
      # BOUNDARY LAYER HEIGHT ##################################################
      # Boundary layers heights (m)
      blh <- rotate(brick(x = fname, varname="blh"))
      tmp <- as.vector(extract(blh, points))
      
      # Generate TS for precipitation from blh
      tmpTS1 <- xts(tmp, order.by = dates12H)
      # Right join to get data for the dates in climate dataframe
      tmpTS <- merge.xts(tmpTS1, emptyTS, join = "right")
      # Fill in the climate dataframe with ws values
      climate$BLH[myrows] <- as.numeric(tmpTS[,1])
      rm(blh, tmp, tmpTS, tmpTS1)
      
      # SOLAR RADIATION ########################################################
      # Surface net solar radiation (W/m2s)
      sr <- rotate(brick(x = fname, varname="ssr"))
      tmp <- as.vector(extract(sr, points))
      
      # Generate TS for precipitation from blh
      tmpTS1 <- xts(tmp, order.by = dates12H)
      # Right join to get data for the dates in climate dataframe
      tmpTS <- merge.xts(tmpTS1, emptyTS, join = "right")
      # Fill in the climate dataframe with ws values
      climate$SR[myrows] <- as.numeric(tmpTS[,1])
      rm(sr, tmp, tmpTS, tmpTS1)
      
    }
    
  }
  
}

# saveRDS(climate, "~/kehra/data/Climate/climate_England_1981_2014.rds")

################################################################################
# ADD HEALTH DATA ##############################################################
################################################################################

# Health data in the UK is available from the Office for National Statistics (for all the experiments run with the data, see the script __~/kehra/scripts/GetData/HealthData.R__)

```{r}
library(gdata)

Deaths <- read.xls("~/kehra/data/Health/ONS_purchasedData/20012005.xls", 
sheet = "Table 1", header = TRUE, skip=2)
# Look for ambigous dates
rows <- sort(union(which(Deaths$Month == 0), which(Deaths$Day == 0)))
Deaths <- Deaths[-rows,]
# Remove them, for the time being
CVDcancer <- Deaths[Deaths$Cause == "CVD and cancer",]
LiverDiseases <- Deaths[Deaths$Cause == "Liver diseases",]

Deaths <- read.xls("~/kehra/data/Health/ONS_purchasedData/20062010.xls", 
sheet = "Table 2", header = TRUE, skip=2)
# Look for ambigous dates
rows <- sort(union(which(Deaths$Month == 0), which(Deaths$Day == 0)))
Deaths <- Deaths[-rows,]
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver diseases",])

Deaths <- read.xls("~/kehra/data/Health/ONS_purchasedData/20112014.xls", 
sheet = "Table 3", header = TRUE, skip=2)
# Look for ambigous dates
rows <- sort(union(which(Deaths$Month == 0), which(Deaths$Day == 0)))
Deaths <- Deaths[-rows,]
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver diseases",])

rm(Deaths, rows)
CVDcancer$DateByDay <- as.Date(paste(CVDcancer$Year., CVDcancer$Month, CVDcancer$Day, sep="-"))
LiverDiseases$DateByDay <- as.Date(paste(LiverDiseases$Year., LiverDiseases$Month, LiverDiseases$Day, sep="-"))
CVDcancer$Over0 <- apply(X = na.omit(CVDcancer[, 7:26]), MARGIN = 1, FUN = sum)
CVDcancer$Over20 <- apply(X = na.omit(CVDcancer[, 12:26]), MARGIN = 1, FUN = sum)
CVDcancer$Over40 <- apply(X = na.omit(CVDcancer[, 16:26]), MARGIN = 1, FUN = sum)
CVDcancer$Over60 <- apply(X = na.omit(CVDcancer[, 20:26]), MARGIN = 1, FUN = sum)
CVDcancer <- CVDcancer[,c("DateByDay", "Region", 
"Over0", "Over20", "Over40", "Over60")]
CVDcancer$Year <- format(as.Date(CVDcancer$DateByDay), "%Y")
LiverDiseases$Over0 <- apply(X = na.omit(LiverDiseases[, 7:26]), MARGIN = 1, FUN = sum)
LiverDiseases$Over20 <- apply(X = na.omit(LiverDiseases[, 12:26]), MARGIN = 1, FUN = sum)
LiverDiseases$Over40 <- apply(X = na.omit(LiverDiseases[, 16:26]), MARGIN = 1, FUN = sum)
LiverDiseases$Over60 <- apply(X = na.omit(LiverDiseases[, 20:26]), MARGIN = 1, FUN = sum)
LiverDiseases <- LiverDiseases[,c("DateByDay", "Region", 
"Over0", "Over20", "Over40", "Over60")]
LiverDiseases$Year <- format(as.Date(LiverDiseases$DateByDay), "%Y")

# Rename
names(CVDcancer) <- c("DateByDay", "Region", "CVDcancerOver0", "CVDcancerOver20", "CVDcancerOver40", "CVDcancerOver60", "Year")
names(LiverDiseases) <- c("DateByDay", "Region", "LiverOver0", "LiverOver20", "LiverOver40", "LiverOver60", "Year")

# Join
Health <- CVDcancer %>% left_join(LiverDiseases)

rm(CVDcancer,LiverDiseases)
```

# SOCIO-ECONOMIC DATA
Socio-economic data in the UK is available from the Office for National Statistics 
```{r}
library(reshape2)
populationEstimates <- readRDS("~/kehra/data/SocioEconomic/PopulationEstimatesRegions1971_2014.rds")
populationEstimates <- melt(data = populationEstimates)
names(populationEstimates) <- c("Code", "Region","Year", "YearlyPopEst")
populationEstimates$Year <- substr(populationEstimates$Year, 5,8)

HealthSocio <- Health %>% left_join(populationEstimates)

# Standardise mortality counts
HealthSocio$CVD00 <- HealthSocio$CVDcancerOver0/HealthSocio$YearlyPopEst
HealthSocio$CVD20 <- HealthSocio$CVDcancerOver20/HealthSocio$YearlyPopEst
HealthSocio$CVD40 <- HealthSocio$CVDcancerOver40/HealthSocio$YearlyPopEst
HealthSocio$CVD60 <- HealthSocio$CVDcancerOver60/HealthSocio$YearlyPopEst

HealthSocio$LIV00 <- HealthSocio$LiverOver0/HealthSocio$YearlyPopEst
HealthSocio$LIV20 <- HealthSocio$LiverOver20/HealthSocio$YearlyPopEst
HealthSocio$LIV40 <- HealthSocio$LiverOver40/HealthSocio$YearlyPopEst
HealthSocio$LIV60 <- HealthSocio$LiverOver60/HealthSocio$YearlyPopEst

# Fix inconsistency with name of regions
HealthSocio$Region <- as.character(HealthSocio$Region)
HealthSocio$Region[HealthSocio$Region=="North East"] <- "North East (England)"
HealthSocio$Region[HealthSocio$Region=="South East"] <- "South East (England)"
HealthSocio$Region[HealthSocio$Region=="North West"] <- "North West (England)"
HealthSocio$Region[HealthSocio$Region=="South West"] <- "South West (England)"
HealthSocio$Region[HealthSocio$Region=="East Midlands"] <- "East Midlands (England)"
HealthSocio$Region[HealthSocio$Region=="West Midlands"] <- "West Midlands (England)"

HealthSocio <- HealthSocio[, c("DateByDay", "Region", "Year",
"CVD00", "CVD20", "CVD40", "CVD60", 
"LIV00", "LIV20", "LIV40", "LIV60")]

# saveRDS(HealthSocio, "~/kehra/data/HealthSocio_England_1979_2014.rds")
rm(Health, populationEstimates)
```

# Build England database

```{r}
HealthSocio <- readRDS("~/kehra/data/HealthSocio_England_1979_2014.rds") 
exposure <- readRDS("~/kehra/data/exposure_England_1979_2014.rds")
# Join exposure and healthsocio
EnglandDB <- exposure %>% left_join(HealthSocio)
str(EnglandDB)

# For the analysis, variables must be either numeric, factors or ordered factors
EnglandDB$altitude <- as.numeric(EnglandDB$altitude)
EnglandDB$la_region_id <- factor(EnglandDB$la_region_id)
EnglandDB$Region <- factor(EnglandDB$Region)
EnglandDB$code <- factor(EnglandDB$code)
EnglandDB$site <- factor(EnglandDB$site)
EnglandDB$Year <- factor(EnglandDB$Year)
EnglandDB$Month <- factor(format(EnglandDB$DateByDay, "%m"))
EnglandDB$Day <- factor(format(EnglandDB$DateByDay, "%d"))
EnglandDB$Time <- factor(format(EnglandDB$date, "%H"))

EnglandDB <- EnglandDB[,c(# "date", "DateByDay"
"Year", "Month", "Day", "Time",
"code", "site", "site.type", "Region",
"zone_name", "zone_id", "la_region", "la_region_id",
#"date_started", "date_ended", "ratified_to",
"latitude", "longitude", "altitude", "ws", "wd", 
"o3", "so2", "co", "pm10", "pm2.5", "nv2.5", "v2.5",
"nv10", "v10", "no", "no2", "nox",
"TEMP", "WS", "WD", "BLH", "PREC",
"CVD00", "CVD20", "CVD40", "CVD60", 
"LIV00", "LIV20", "LIV40", "LIV60")]

# Check variable types (factor/categorical or numeric)
str(EnglandDB)

# saveRDS(EnglandDB, "~/kehra/data/EnglandDB_1979_2014.rds")  
```
