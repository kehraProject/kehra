# Check for missing dependencies and install them
packs <- c("devtools", "plyr", "dplyr", "raster", "sp", "rgdal", "xts", "zoo",
           "parallel")
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(devtools)

# INSTALL KEHRA package
# install_github("cvitolo/r_kehra", subdir = "kehra")
library(kehra)
library(parallel)

# rm(new.packages, packs)

################################################################################
# POLLUTION DATA ###############################################################
################################################################################

# Pollution data in the UK is available from the DEFRA UK-AIR website
# Get metadata from the rdefra package
# install_github("cvitolo/r_rdefra", subdir="rdefra")
library(rdefra)
data(stationsNew)
stationsNew$Region <- NA

stationsNew <- stationsNew[!is.na(stationsNew$SiteID),]
stationsNew <- stationsNew[!is.na(stationsNew$Latitude) | 
                             !is.na(stationsNew$Longitude),]

stationsNew <- stationsNew[,c("SiteID", "Site.Name", "Latitude", "Longitude",
                              "Altitude..m.", "Environment.Type", "Zone", 
                              "Region")]

names(stationsNew) <- c("SiteID", "Site.Name", "Latitude", "Longitude",
                        "Altitude", "Environment.Type", "Zone", "Region")

stationsNew$Site.Name <- as.character(stationsNew$Site.Name)
stationsNew$Environment.Type <- as.character(stationsNew$Environment.Type)
stationsNew$Zone <- as.character(stationsNew$Zone)
stationsNew$Region <- as.character(stationsNew$Region)

# Which stations are in England?
library(raster) 
adm <- getData('GADM', country='GBR', level=1)
England <- adm[adm$NAME_1=='England',]
stationsSP <- SpatialPoints(stationsNew[, c('Longitude', 'Latitude')], 
                            proj4string=CRS(proj4string(England)))

library(sp)
x <- over(stationsSP, England)[,1]
x <- which(!is.na(x))
stations <- stationsNew[x,]
rm(adm,England,stationsSP,x, stationsNew)

# Which region are the stations in?
library(rgdal)
Regions <- readOGR(dsn = "/home/claudia/kehra/data/GEO/UK/AdminBoundaries/", layer = "Regions")
Regions <- spTransform(Regions, CRS("+init=epsg:4326"))
stationsSP <- SpatialPoints(stations[, c('Longitude', 'Latitude')], 
                            proj4string=CRS("+init=epsg:4326"))

for (reg in 1:length(Regions@data$NAME)){
  print(reg)
  x <- over(stationsSP, Regions[reg,])[,1]
  x <- which(!is.na(x))
  stations$Region[x] <- as.character(Regions@data$NAME[reg])
}

# Get time series
library(rdefra)
library(plyr)
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

# How many stations actually have datasets?
stations <- stations[which(stations$SiteID %in% unique(myList$site_id)),]
# saveRDS(stations, "~/kehra/data/Pollution/stations.rds")
# stations <- readRDS("~/kehra/data/Pollution/stations.rds")

# remove not relevant variables
pollutionDB <-myList[,c("Date", "time", "site_id",
                        "PM.sub.10..sub..particulate.matter..Hourly.measured.",
                        "PM.sub.2.5..sub..particulate.matter..Hourly.measured.",
                        "Nitrogen.dioxide", "Ozone", "Sulphur.dioxide", 
                        "Carbon.monoxide")]
# rm(myList)
names(pollutionDB) <- c("Date","time","SiteID",
                        "PM10","PM2.5","NO2","O3", "SO2", "CO")
# str(pollutionDB)
pollutionDB$Date <- as.character(pollutionDB$Date)
pollutionDB$time <- as.character(pollutionDB$time)
pollutionDB$datetime <- paste(pollutionDB$Date, " ", pollutionDB$time, sep = "")

# Keep only the relevant columns
pollutionDB <- pollutionDB[,c("datetime", "SiteID", 
                              "PM10","PM2.5", "NO2", "O3", "SO2", "CO")]

# saveRDS(pollutionDB, "~/kehra/data/Pollution/pollutionTEMP.rds")
# pollutionDB <- readRDS("~/kehra/data/Pollution/pollutionTEMP.rds")

# Infill missing values using linear interpolation (maxgap = 12)
library(parallel)
library(kehra)
PM10l <- mclapply(X = unique(pollutionDB$SiteID),
                  FUN = fillMissingValues, mc.cores = detectCores() - 1,
                  df = pollutionDB[, c("datetime", "SiteID", "PM10")],
                  parallel = TRUE)
PM10 <- do.call(rbind.data.frame, PM10l)
rm(PM10l); head(PM10)

PM25l <- mclapply(as.character(unique(pollutionDB$SiteID)),
                  FUN = fillMissingValues, mc.cores = detectCores() - 1,
                  pollutionDB[, c("datetime", "SiteID", "PM2.5")],
                  parallel = TRUE)
PM2.5 <- do.call(rbind.data.frame, PM25l)
rm(PM25l)

NO2l <- mclapply(as.character(unique(pollutionDB$SiteID)),
                  FUN = fillMissingValues, mc.cores = detectCores() - 1,
                  pollutionDB[, c("datetime", "SiteID", "NO2")],
                  parallel = TRUE)
NO2 <- do.call(rbind.data.frame, NO2l)
rm(NO2l)

O3l <- mclapply(as.character(unique(pollutionDB$SiteID)),
                FUN = fillMissingValues, mc.cores = detectCores() - 1,
                pollutionDB[, c("datetime", "SiteID", "O3")],
                parallel = TRUE)
O3 <- do.call(rbind.data.frame, O3l)
rm(O3l)

SO2l <- mclapply(as.character(unique(pollutionDB$SiteID)),
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 pollutionDB[, c("datetime", "SiteID", "SO2")],
                 parallel = TRUE)
SO2 <- do.call(rbind.data.frame, SO2l)
rm(SO2l)

COl <- mclapply(as.character(unique(pollutionDB$SiteID)),
                FUN = fillMissingValues, mc.cores = detectCores() - 1,
                pollutionDB[, c("datetime", "SiteID", "CO")],
                parallel = TRUE)
CO <- do.call(rbind.data.frame, COl)
rm(COl)

library(dplyr)
pollution <- left_join(PM10, PM2.5, by=c("datetime", "SiteID"))
pollution <- left_join(pollution, NO2, by=c("datetime", "SiteID"))
pollution <- left_join(pollution, O3, by=c("datetime", "SiteID"))
pollution <- left_join(pollution, SO2, by=c("datetime", "SiteID"))
pollution <- left_join(pollution, CO, by=c("datetime", "SiteID"))
# saveRDS(pollution, "~/kehra/data/Pollution/pollution.rds")

################################################################################
# ADD CLIMATE DATA #############################################################
################################################################################

rm(list=ls(all=TRUE))
stations <- readRDS("~/kehra/data/Pollution/stations.rds")

# CLIMATE DATA
library(ncdf4)
library(raster)
library(xts)

# extract data from ERA INTERIM netcdf files
years <- 1981:2014
t2mI <- mclapply(X = years, 
                 FUN = pointInspection, mc.cores = length(years),
                 points = stations, var = "t2m", prefix = "climate", 
                 path = "~/kehra/data/Climate", parallel = TRUE)
t2m <- do.call(rbind.data.frame, t2mI); rm(t2mI)

u10I <- mclapply(X = years, 
                 FUN = pointInspection, mc.cores = length(years),
                 points = stations, var = "u10", prefix = "climate", 
                 path = "~/kehra/data/Climate", parallel = TRUE)
u10 <- do.call(rbind.data.frame, u10I); rm(u10I)

v10I <- mclapply(X = years, 
                 FUN = pointInspection, mc.cores = length(years),
                 points = stations, var = "v10", prefix = "climate", 
                 path = "~/kehra/data/Climate", parallel = TRUE)
v10 <- do.call(rbind.data.frame, v10I); rm(v10I)

tpI <- mclapply(X = years, 
                 FUN = pointInspection, mc.cores = length(years),
                 points = stations, var = "tp", prefix = "climate", 
                 path = "~/kehra/data/Climate", parallel = TRUE)
tp <- do.call(rbind.data.frame, tpI); rm(tpI)

blhI <- mclapply(X = years, 
                 FUN = pointInspection, mc.cores = length(years),
                 points = stations, var = "blh", prefix = "climate", 
                 path = "~/kehra/data/Climate", parallel = TRUE)
blh <- do.call(rbind.data.frame, blhI); rm(blhI)

ssrI <- mclapply(X = years, 
                 FUN = pointInspection, mc.cores = length(years),
                 points = stations, var = "ssr", prefix = "climate", 
                 path = "~/kehra/data/Climate", parallel = TRUE)
ssr <- do.call(rbind.data.frame, ssrI); rm(ssrI)

# save(t2m, u10, v10, tp,blh,ssr, file = "~/kehra/data/Climate/climateTEMP.rda")
# rm(blh, ssr, )
# gc()
# load("~/kehra/data/Climate/climateTEMP.rda")

# Infill missing value using linear interpolation
t2mI <- mclapply(X = unique(t2m$SiteID), 
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 df = t2m, maxgap = 12, parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
t2m <- do.call(rbind.data.frame, t2mI); rm(t2mI)

u10I <- mclapply(X = unique(u10$SiteID), 
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 df = u10, maxgap = 12, parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
u10 <- do.call(rbind.data.frame, u10I); rm(u10I)

v10I <- mclapply(X = unique(v10$SiteID), 
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 df = v10, maxgap = 12, parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
v10 <- do.call(rbind.data.frame, v10I); rm(v10I)

tpI <- mclapply(X = unique(tp$SiteID), 
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 df = tp, maxgap = 12, parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
tp <- do.call(rbind.data.frame, tpI); rm(tpI)

blhI <- mclapply(X = unique(blh$SiteID), 
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 df = blh, maxgap = 12, parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
blh <- do.call(rbind.data.frame, blhI); rm(blhI)

ssrI <- mclapply(X = unique(ssr$SiteID), 
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 df = ssr, maxgap = 12, parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
ssr <- do.call(rbind.data.frame, ssrI); rm(ssrI)

# Calculate wind speed and direction from u10 and v10
wsI <- windSpeed(u10$u10, v10$v10)
ws <- u10[, 1:2]; ws$ws <- wsI
wdI <- mapply(windDirection, u10$u10, v10$v10)
wd <- u10[, 1:2]; wd$wd <- wdI

# save(t2mI, ws, wd, tpI, blhI, ssrI, file = "~/kehra/data/Climate/climateTEMP2.rda")
# rm(list=ls(all=TRUE))
# gc()
# load("~/kehra/data/Climate/climateTEMP2.rda")

library(dplyr)
# Fix data types before joining tables to avoid problems with factors
str(t2mI); str(ws)
t2mI$datetime <- as.character(t2mI$datetime)
t2mI$SiteID <- as.character(t2mI$SiteID)
t2mI$t2m <- as.numeric(as.character(t2mI$t2m))
ws$datetime <- as.character(ws$datetime)
ws$SiteID <- as.character(ws$SiteID)
# Start joining the columns from the more densely populated (if not filled in)
clima <- left_join(t2mI, ws, by=c("datetime", "SiteID"))

str(clima); str(wd)
wd$datetime <- as.character(wd$datetime)
wd$SiteID <- as.character(wd$SiteID)
clima <- left_join(clima, wd, by=c("datetime", "SiteID"))

str(clima); str(tpI)
tpI$datetime <- as.character(tpI$datetime)
tpI$SiteID <- as.character(tpI$SiteID)
tpI$tp <- as.numeric(as.character(tpI$tp))
clima <- left_join(clima, tpI, by=c("datetime", "SiteID"))

str(clima); str(blhI)
blhI$datetime <- as.character(blhI$datetime)
blhI$SiteID <- as.character(blhI$SiteID)
blhI$blh <- as.numeric(as.character(blhI$blh))
clima <- left_join(clima, blhI, by=c("datetime", "SiteID"))

str(clima); str(ssrI)
ssrI$datetime <- as.character(ssrI$datetime)
ssrI$SiteID <- as.character(ssrI$SiteID)
ssrI$ssr <- as.numeric(as.character(ssrI$ssr))
clima <- left_join(clima, ssrI, by=c("datetime", "SiteID"))

# saveRDS(clima, "~/kehra/data/Climate/clima_England_1981_2014.rds")
# clima <- readRDS("~/kehra/data/Climate/clima_England_1981_2014.rds")

# rm(blhI, ssrI, t2mI, tpI, wd, ws)
# gc()

# Load pollution data and join with climate data
pollution <- readRDS("~/kehra/data/Pollution/pollution.rds")
str(clima);str(pollution)
exposure <- full_join(clima, pollution, by=c("datetime", "SiteID"))
saveRDS(exposure, "~/kehra/data/exposure.rds")

# rm(clima, pollution)
# gc()

################################################################################
# ADD HEALTH DATA ##############################################################
################################################################################

rm(list=ls(all=TRUE))

# Health data in the UK is available from the Office for National Statistics 
# (for all the experiments run with the data, see the script 
# ~/kehra/scripts/GetData/HealthData.R)

library(gdata)

Deaths <- read.xls("~/kehra/data/Health/ONS_purchasedData/1981to1983.xls", 
                   sheet = "Table 1", header = TRUE, skip=7)
Deaths[1,]
# Create the first batch of data
CVDcancer <- Deaths[Deaths$Cause == "CVD and Cancer",]
LiverDiseases <- Deaths[Deaths$Cause == "Liver Diseases",]

Deaths <- read.xls("~/kehra/data/Health/ONS_purchasedData/1984to1987.xls", 
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

Deaths <- read.xls("~/kehra/data/Health/ONS_purchasedData/1988to1992.xls", 
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

Deaths <- read.xls("~/kehra/data/Health/ONS_purchasedData/1993to1995.xls", 
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

Deaths <- read.xls("~/kehra/data/Health/ONS_purchasedData/1996to2000.xls", 
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

Deaths <- read.xls("~/kehra/data/Health/ONS_purchasedData/20012005.xls", 
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

Deaths <- read.xls("~/kehra/data/Health/ONS_purchasedData/20062010.xls", 
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

Deaths <- read.xls("~/kehra/data/Health/ONS_purchasedData/20112014.xls", 
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

rm(Deaths)

# Aggregate Deaths for age groups
CVDcancer$DateByDay <- as.Date(paste(CVDcancer$Year, 
                                     CVDcancer$Month, CVDcancer$Day, sep="-"))
LiverDiseases$DateByDay <- as.Date(paste(LiverDiseases$Year, 
                                         LiverDiseases$Month, 
                                         LiverDiseases$Day, sep="-"))
CVDcancer$Over0  <- apply(X = na.omit(CVDcancer[, 7:26]), MARGIN = 1, 
                          FUN = sum, na.rm = TRUE)
CVDcancer$Over20 <- apply(X = na.omit(CVDcancer[, 12:26]), MARGIN = 1,
                          FUN = sum, na.rm = TRUE)
CVDcancer$Over40 <- apply(X = na.omit(CVDcancer[, 16:26]), MARGIN = 1,
                          FUN = sum, na.rm = TRUE)
CVDcancer$Over60 <- apply(X = na.omit(CVDcancer[, 20:26]), MARGIN = 1,
                          FUN = sum, na.rm = TRUE)
CVDcancer <- CVDcancer[,c("DateByDay", "Region", "Over0", "Over20", "Over40", "Over60")]

LiverDiseases$Over0 <- apply(X = na.omit(LiverDiseases[, 7:26]), 
                             MARGIN = 1, FUN = sum, na.rm = TRUE)
LiverDiseases$Over20 <- apply(X = na.omit(LiverDiseases[, 12:26]), 
                              MARGIN = 1, FUN = sum, na.rm = TRUE)
LiverDiseases$Over40 <- apply(X = na.omit(LiverDiseases[, 16:26]), 
                              MARGIN = 1, FUN = sum, na.rm = TRUE)
LiverDiseases$Over60 <- apply(X = na.omit(LiverDiseases[, 20:26]), 
                              MARGIN = 1, FUN = sum, na.rm = TRUE)
LiverDiseases <- LiverDiseases[,c("DateByDay", "Region", 
                                  "Over0", "Over20", "Over40", "Over60")]

# Rename
names(CVDcancer) <- c("DateByDay", "Region", "CVDOver0", "CVDOver20", "CVDOver40", "CVDOver60")
names(LiverDiseases) <- c("DateByDay", "Region", "LiverOver0", "LiverOver20", "LiverOver40", "LiverOver60")

# Join
Health <- CVDcancer %>% left_join(LiverDiseases, by = c("DateByDay", "Region"))
Health$Year <- format(as.Date(Health$DateByDay), "%Y")
Health <- Health[, c("DateByDay", "Year", "Region", 
                     "CVDOver0", "CVDOver20", "CVDOver40", "CVDOver60", 
                     "LiverOver0", "LiverOver20", "LiverOver40", "LiverOver60")]
Health[,4:11] <- sapply(Health[,4:11], as.numeric)
Health$Region <- as.character(Health$Region)
Health$DateByDay <- as.character(Health$DateByDay)
Health$Region[Health$Region=="Yorkshire and the Humber"] <- "Yorkshire and The Humber"
rm(CVDcancer,LiverDiseases)

# SOCIO-ECONOMIC DATA
# Socio-economic data in the UK is available from the Office for National Statistics 

library(reshape2)
populationEstimates <- readRDS("~/kehra/data/SocioEconomic/PopulationEstimatesRegions1971_2014.rds")
populationEstimates[,1:2] <- sapply(populationEstimates[,1:2], as.character)
populationEstimates[,3:46] <- sapply(populationEstimates[,3:46], as.numeric)
populationEstimates <- melt(data = populationEstimates[, 2:46])
names(populationEstimates) <- c("Region", "Year", "YearlyPopEst")
populationEstimates$Year <- substr(populationEstimates$Year, 5,8)
populationEstimates$Region <- as.character(populationEstimates$Region)

str(populationEstimates)
str(Health)

# Join and standardise mortality counts (per 10000 people)
HealthSocio <- Health %>% 
  left_join(populationEstimates, by = c("Year", "Region")) %>%
  mutate(CVD00 = CVDOver0/YearlyPopEst * 10000) %>%
  mutate(CVD20 = CVDOver20/YearlyPopEst * 10000) %>%
  mutate(CVD40 = CVDOver40/YearlyPopEst * 10000) %>%
  mutate(CVD60 = CVDOver60/YearlyPopEst * 10000) %>%
  mutate(LIV00 = LiverOver0/YearlyPopEst * 10000) %>%
  mutate(LIV20 = LiverOver20/YearlyPopEst * 10000) %>%
  mutate(LIV40 = LiverOver40/YearlyPopEst * 10000) %>%
  mutate(LIV60 = LiverOver60/YearlyPopEst * 10000)

# Fix inconsistency with name of regions
str(HealthSocio)
HealthSocio$Region[HealthSocio$Region=="London"] <- "Greater London Authority"
# CHECK: unique(stations$Region) %in% HealthSocio$Region

HealthSocio <- HealthSocio[, c("DateByDay", "Region", "Year", 
                               "CVD00", "CVD20", "CVD40", "CVD60",
                               "LIV00", "LIV20", "LIV40", "LIV60")]

# saveRDS(HealthSocio, "~/kehra/data/Health/HealthSocio.rds")
# rm(Health, populationEstimates)

# Build England database #######################################################

# rm(list=ls(all=TRUE))

# Check available memory
system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)
# Do garbage collection, if necessary
gc()

# Reload all the necessary datasets
# HealthSocio <- readRDS("~/kehra/data/Health/HealthSocio.rds") 
# exposure <- readRDS("~/kehra/data/exposure.rds")
# stations <- readRDS("~/kehra/data/Pollution/stations.rds")

# Join exposure and station metadata
EnglandDB <- exposure %>% left_join(stations, by = "SiteID")
str(EnglandDB)
EnglandDB$DateByDay <- as.character(format(as.POSIXlt(EnglandDB$datetime), "%Y-%m-%d"))
EnglandDB$Year <- as.character(format(as.POSIXlt(EnglandDB$datetime), "%Y"))
EnglandDB$Month <- as.character(format(as.POSIXlt(EnglandDB$datetime), "%m"))
EnglandDB$Day <- as.character(format(as.POSIXlt(EnglandDB$datetime), "%d"))
EnglandDB$Hour <- as.character(format(as.POSIXlt(EnglandDB$datetime), "%H"))
EnglandDB$Season <- as.character(getSeason(as.Date(EnglandDB$DateByDay)))
head(EnglandDB)

# Join exposure and healthsocio
EnglandDB <- EnglandDB %>% left_join(HealthSocio, 
                                     by = c("DateByDay", "Region", "Year"))

# For the analysis, variables must be either numeric, factors or ordered factors
# Check variable types (factor/categorical or numeric)
str(EnglandDB); as.data.frame(names(EnglandDB))
# reorder the info (categorical first, then continouous)
EnglandDB <- EnglandDB[,c("SiteID", "Region", "Zone", "Environment.Type", 
                          "Year", "Season", "Month", "Day", "Hour",
                          "Latitude", "Longitude", "Altitude", 
                          "t2m", "ws", "wd", "tp", "blh", "ssr",
                          "PM10", "PM2.5", "NO2", "O3", "SO2", "CO",
                          "CVD00", "CVD20", "CVD40", "CVD60",           
                          "LIV00", "LIV20", "LIV40", "LIV60")]
names(EnglandDB)[4] <- "Type"

# Fix data types
str(EnglandDB)
EnglandDB$SiteID <- factor(EnglandDB$SiteID)
EnglandDB$Region <- factor(EnglandDB$Region)
EnglandDB$Zone   <- factor(EnglandDB$Zone)
EnglandDB$Type   <- factor(EnglandDB$Type)
EnglandDB$Year   <- factor(EnglandDB$Year)
EnglandDB$Season <- factor(EnglandDB$Season)
EnglandDB$Month  <- factor(EnglandDB$Month)
EnglandDB$Day    <- factor(EnglandDB$Day)
EnglandDB$Hour   <- factor(EnglandDB$Hour)

# saveRDS(EnglandDB, "~/kehra/data/EnglandDB.rds")