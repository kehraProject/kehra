################################################################################
######################## CLIMATE DATASET FROM ECMWF ERA-INTERIM ################
################################################################################

# Use the web app at:
# http://apps.ecmwf.int/datasets/data/interim-full-daily
# Or run the following python script:
# ~/Dropbox/Projects/kehra/scripts/GetData/ecmwf.py

library(ncdf4)
library(RNetCDF)
# Fixed bug with rotate (?)
# install.packages("raster", repos="http://R-Forge.R-project.org")
library(raster)

# Functions to calculate wind speed and direction
windSpeed <- function(u, v) {
  sqrt( u^2 + v^2) 
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

# load pollution data
pollution <- readRDS("~/kehra/data/Pollution/pollution_England_1979_2014.rds")

# Get list of IDs
ids <- as.character(unique((stationsEngland$code))) # 159 stations

# Initialise data frame
days <- seq.Date(from = as.Date("1979-01-01"), 
                 to = as.Date("2014-12-31"), by = "day") 
climate <- data.frame(NA, nrows = length(days)*length(ids), ncols = 5)
names(climate) <- c("DateByDay", "Year", )

# add placeholders to store climate information
df <- cbind(pollution, "Y"= format(as.Date(pollution$date), "%Y"), 
            "TEMP"=NA, "WS"=NA, "WD"=NA, "BLH"=NA, "PREC"=NA)

# Loop through years and station IDs
for (myYear in 1979:2014){
  
  myFile <- paste("data/Climate/climate", myYear, ".nc", sep="")
  
  # Temperature
  t2m <- rotate(brick(x = myFile, varname="t2m"))
  # Total precipitation
  tp <- rotate(brick(x = myFile, varname="tp"))
  # Boundary layers heights
  blh <- rotate(brick(x = myFile, varname="blh"))
  # Wind U component
  u10 <- rotate(brick(x = myFile, varname="u10"))
  # Wind V component
  v10 <- rotate(brick(x = myFile, varname="v10"))
  
  ### ADD CLIMATE VARIABLES TO POLLUTION DATA ##################################
  for (id in ids){
    
    print(paste(myYear,id))
    
    myrows <- which(df$code == id & df$Y == myYear)
    
    if (length(myrows) > 0){
      
      # In df[myrows,] the coordinates are the same, I can just keep the first.
      points <- SpatialPoints(df[myrows[1], c('Longitude', 'Latitude')], 
                              proj4string=CRS('+proj=longlat +datum=WGS84'))
      # Check whether crs(points) == crs(u10) == crs(v10) == crs(t2m)
      # plot(points, add=TRUE, col="red")
      
      df$TEMP[myrows] <- rep(as.vector(extract(t2m, points)), each=24)
      df$PREC[myrows] <- rep(as.vector(extract(tp, points)), each=24)
      df$BLH[myrows]  <- rep(as.vector(extract(blh, points)), each=24)
      u <- rep(as.vector(extract(u10, points)), each=24)
      v <- rep(as.vector(extract(v10, points)), each=24)
      df$WS[myrows] <- windSpeed(u,v)
      wd <- c()
      for (i in 1:length(myrows)) {
        wd[i] <- windDir(u[i], v[i])
      }
      df$WD[myrows] <- wd
    } 
    
  }
  
  # saveRDS(df, "data/exposure.rds")
  
  rm(t2m, tp, blh, u10, v10)
  
}

# saveRDS(df, "data/exposure.rds")

wsDF <- data.frame("AURN"=df$ws, "ECMWF"=df$WS)
wsDF <- wsDF[complete.cases(wsDF),]

myRange <- 1:1000
plot(wsDF$AURN[myRange], type="l")
lines(wsDF$ECMWF[myRange], col="red")

################################################################################

# Station Environmental Type and Zone 
df$EnvType <- NA
df$Zone <- NA
df$Region <- NA
allStations <- read.csv("data/Pollution/uk-air-search-results-all.csv") 
for (id in ids){
  
  myrows <- which(df$code == id)
  
  x <- which(as.character(allStations$Site.Name) == as.character(df$site[myrows[1]]))
  
  df$EnvType[myrows] <- as.character(allStations$Environment.Type[x])
  df$Zone[myrows] <- as.character(allStations$Zone[x])
  if (allStations$Zone[x] == "South West" | allStations$Zone[x] == "Bristol Urban Area" | allStations$Zone[x] == "Bournemouth Urban Area") {
    df$Region[myrows] <- "South West"
  }
  if (allStations$Zone[x] == "South East" | allStations$Zone[x] == "Brighton/Worthing/Littlehampton" | allStations$Zone[x] == "Reading/Wokingham Urban Area" | allStations$Zone[x] == "Portsmouth Urban Area" | allStations$Zone[x] == "Southampton Urban Area") {
    df$Region[myrows] <- "South East"
  }
  if (allStations$Zone[x] == "Greater London Urban Area") {
    df$Region[myrows] <- "London"
  }
  if (allStations$Zone[x] == "East" | allStations$Zone[x] == "Eastern" | allStations$Zone[x] == "Southend Urban Area") {
    df$Region[myrows] <- "East"
  }
  if (allStations$Zone[x] == "East Midlands" | allStations$Zone[x] == "Nottingham Urban Area" | allStations$Zone[x] == "Leicester Urban Area") {
    df$Region[myrows] <- "East Midlands"
  }
  if (allStations$Zone[x] == "West Midlands" | allStations$Zone[x] == "West Midlands Urban Area" | allStations$Zone[x] == "Coventry/Bedworth" | allStations$Zone[x] == "The Potteries" ) {
    df$Region[myrows] <- "West Midlands"
  }
  if (allStations$Zone[x] == "Yorkshire & Humberside" | allStations$Zone[x] == "West Yorkshire Urban Area" | allStations$Zone[x] == "Kingston upon Hull" | allStations$Zone[x] == "Sheffield Urban Area") {
    df$Region[myrows] <- "Yorkshire and The Humber"
  }
  if (allStations$Zone[x] == "North West" | allStations$Zone[x] == "Greater Manchester Urban Area" | allStations$Zone[x] == "North West & Merseyside" | allStations$Zone[x] == "Birkenhead Urban Area" | allStations$Zone[x] == "Preston Urban Area" | allStations$Zone[x] == "Blackpool Urban Area" | allStations$Zone[x] == "Liverpool Urban Area") {
    df$Region[myrows] <- "North West"
  }
  if (allStations$Zone[x] == "North East" | allStations$Zone[x] == "Teesside Urban Area" | allStations$Zone[x] == "Tyneside") {
    df$Region[myrows] <- "North East"
  }
}

# saveRDS(df, "data/exposure.rds")

################################################################################

# Mortality rates by regions

# Read exposure
df <- readRDS("data/exposure.rds")
ids <- as.character(unique((df$code)))
# Read Mortality rates
mr <- readRDS("data/Health/MortalityRates.rds")
# Yearly population estimates
pop <- readRDS("data/SocioEconomic/PopulationEstimatesRegions1971_2014.rds")[,c(1,2,30:45)]

df$MortalityRate <- NA
df$Population <- NA
# match mortality to exposure
for (id in ids){
  
  myrows <- which(df$code == id)
  days <- unique(substr(df$date[myrows], 1, 10))
  
  for (myDay in days){
    
    print(paste(myDay,id))
    
    myDayRows <- which(substr(df$date[myrows], 1, 10) == myDay)
    
    if (length(myDayRows) > 0){
      
      zone <- df$Region[myrows][myDayRows][1]
      
      regionColumn <- which(names(mr)==zone)
      regionRow <- which(mr$Date==myDay)
      df$MortalityRate[myrows][myDayRows] <- mr[regionRow, regionColumn]
      
      year <- mr$Y[regionRow]
      popRow <- regionColumn - 1
      popCol <- which(substr(names(pop),5,8) == year)
      
      df$Population[myrows][myDayRows] <- pop[popRow, popCol]
      
    } 
  
  }
  
}

# saveRDS(df, "data/UKdataset.rds")
