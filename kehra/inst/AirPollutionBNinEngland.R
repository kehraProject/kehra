################################################################################
# Modelling air pollution, climate and health using Bayesian Networks:         #
# a case study of the English regions                                          #
#                                                                              #
# Author: Claudia Vitolo                                                       #
# Last updated: June 2016                                                      #
################################################################################

# Create data folder in the home folder and set it as working directory
system("mkdir ~/data")
setwd("~/data")

# Check for missing dependencies and install them
# packs <- c("devtools", "plyr", "dplyr", "raster", "sp", "rgdal", "xts", "zoo",
#            "parallel", "openair", "ncdf4", "gdata", "reshape2", "ggmap")
# new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# rm(new.packages, packs)
library(devtools)

# Install/load RDEFRA and KEHRA packages
# install_github("cvitolo/r_rdefra", subdir="rdefra")
library(rdefra)
# install_github("cvitolo/r_kehra", subdir="kehra")
library(kehra)

################################################################################
# GET POLLUTION DATA (from the DEFRA UK-AIR website) ###########################
################################################################################

# Get spatial regions and station metadata using the rdefra package
data(regions)
data(stations)

# Add empty column to stations to store Region name
stations$Region <- NA

stations <- stations[!is.na(stations$SiteID),]
stations <- stations[!is.na(stations$Latitude) |
                       !is.na(stations$Longitude),]

stations <- stations[,c("SiteID", "Site.Name", "Latitude", "Longitude",
                        "Altitude..m.", "Environment.Type", "Zone",
                        "Region")]
names(stations) <- c("SiteID", "Site.Name", "Latitude", "Longitude",
                     "Altitude", "Environment.Type", "Zone", "Region")

stations$Site.Name <- as.character(stations$Site.Name)
stations$Environment.Type <- as.character(stations$Environment.Type)
stations$Zone <- as.character(stations$Zone)
stations$Region <- as.character(stations$Region)

# Which stations are in England?
library(raster)
adm <- getData('GADM', country='GBR', level=1)
England <- adm[adm$NAME_1=='England',]
stationsSP <- SpatialPoints(stations[, c('Longitude', 'Latitude')],
                            proj4string=CRS(proj4string(England)))

library(sp)
x <- over(stationsSP, England)[,1]
x <- which(!is.na(x))
stationsSP <- stationsSP[x,]
stations <- stations[x,]
rm(adm,England,x)

# Which region are the stations in?
library(rgdal)
for (reg in 1:length(regions@data$NAME)){
  print(reg)
  x <- over(stationsSP, regions[reg,])[,1]
  x <- which(!is.na(x))
  stations$Region[x] <- as.character(regions@data$NAME[reg])
}
rm(stationsSP, x, reg, regions)

# Load modified openair::importAURN() that allows to retrieve data without
# exceeding max number of open connections
importAURN_CV <- function(site = "my1", year = 2009,
                          pollutant = "all", hc = FALSE) {
  
  site <- toupper(site)
  
  files <- lapply(site, function (x) paste(x, "_", year, sep = ""))
  
  files <- do.call(c, files)
  
  
  loadData <- function(x) {
    tryCatch({
      fileName <- paste("http://uk-air.defra.gov.uk/openair/R_data/",
                        x, ".RData", sep = "")
      con <- url(fileName, method = "libcurl")
      load(con)
      
      closeAllConnections()
      
      dat <- get(x)
      
      return(dat)
    },
    error = function(ex) {cat(x, "does not exist - ignoring that one.\n")})
    
  }
  
  
  thedata <- plyr::ldply(files, loadData)
  
  if (nrow(thedata) == 0) return() ## no data
  
  ## suppress warnings for now - unequal factors, harmless
  
  if (is.null(thedata)) stop("No data to import - check site codes and year.",
                             call. = FALSE)
  
  thedata$site <- factor(thedata$site, levels = unique(thedata$site))
  
  ## change names
  names(thedata) <- tolower(names(thedata))
  
  ## change nox as no2
  id <- which(names(thedata) %in% "noxasno2")
  if (length(id) == 1) names(thedata)[id] <- "nox"
  
  
  ## should hydrocarbons be imported?
  if (hc) {
    thedata <- thedata
  } else {
    ## no hydrocarbons - therefore select conventional pollutants
    theNames <- c("date",
                  "co", "nox", "no2", "no", "o3", "so2", "pm10", "pm2.5",
                  "v10", "v2.5", "nv10", "nv2.5", "ws", "wd", "code", "site")
    
    thedata <- thedata[,  which(names(thedata) %in% theNames)]
  }
  
  ## if particular pollutants have been selected
  if (pollutant != "all") thedata <- thedata[, c("date", pollutant,
                                                 "site", "code")]
  
  
  ## warning about recent, possibly unratified data
  timeDiff <- difftime(Sys.time(),  max(thedata$date), units='days')
  if (timeDiff < 180) {
    warning(paste("You have selected some data that is less than 6-months old.",
                  "\n This most recent data is not yet ratified and may be",
                  "changed\n during the QA/QC process. For complete",
                  "information about the \nratification status of a data set,",
                  "please use the online tool at:\n",
                  paste("http://www.airquality.co.uk/data_and_statistics.php?",
                        "action=da_1&go=Go", sep = ""))
  }
  
  ## make sure it is in GMT
  attr(thedata$date, "tzone") <- "GMT"
  
  # make sure class is correct for lubridate
  class(thedata$date) <- c("POSIXct" , "POSIXt")
  
  thedata
}

# Get time series using OPENAIR
OPENAIRts <- importAURN_CV(site = stations$SiteID, year = 1981:2014,
                           pollutant = c("pm10","pm2.5","no2","o3","so2","co"),
                           hc = FALSE)

# How many stations actually have datasets?
stations <- stations[which(stations$SiteID %in% unique(OPENAIRts$code)),]
# saveRDS(stations, "stations.rds")

# Clean up OPENAIRts
pollution <- OPENAIRts[, c("code", "date",
                           "pm10", "pm2.5", "no2", "o3", "so2", "co")]
names(pollution) <- c("SiteID", "datetime",
                      "pm10", "pm2.5", "no2", "o3", "so2", "co")
pollution$SiteID <- as.character(pollution$SiteID)
pollution$datetime <- as.character(pollution$datetime)
# saveRDS(pollution, "pollution.rds")

rm(importAURN_CV, OPENAIRts)

################################################################################
# GET CLIMATE DATA (FROM ECMWF ERA-INTERIM) ####################################
################################################################################

# Run MARS request to get netcdf files (split by year) for the following weather
# variables: 2 metre temperature (t2m), 10 metre wind in u (u10) and v (v10)
# direction, total precipitation (tp), boundary layer height (blh) and surface
# net solar radiation (ssr).
# This requires to set up the ecmwfapi.

py_path <- 'ecmwf.py'
fileConn <- file(paste(py_path))

writeLines(c(
  paste('from ecmwfapi import ECMWFDataServer'),
  paste('server = ECMWFDataServer()'),
  paste('for x in range(2003, 2015):'),
  
  paste('    server.retrieve({'),
  
  paste('        "class"     : "ei",'),
  paste('        "dataset"   : "interim",'),
  paste('        "date"      : str(x) + "-01-01/to/" + str(x) + "-12-31",'),
  paste('        "expver"    : "1",'),
  paste('        "grid"      : "0.75/0.75",'),
  paste('        "levtype"   : "sfc",'),
  paste('        "param"     : "159.128/176.128/228.128/165.128/166.128/167.128",'),
  paste('        "step"      : "3/6/9/12",'),
  paste('        "stream"    : "oper",'),
  paste('        "time"      : "00:00:00/12:00:00",'),
  paste('        "type"      : "fc",'),
  
  paste('        "format"    : "netcdf",'),
  paste('        "target"    : "climate" + str(x) + ".nc"'),
  paste('    })')
  
), fileConn, sep = "\n")
close(fileConn)

system(paste('python', py_path))
rm(fileConn, py_path)

# extract data from ERA INTERIM netcdf files
library(ncdf4)
library(xts)
library(parallel)

years <- 1981:2014
t2mI <- mclapply(X = years,
                 FUN = pointInspection, mc.cores = length(years),
                 points = stations, var = "t2m", prefix = "climate",
                 path = ".", parallel = TRUE)
t2m <- do.call(rbind.data.frame, t2mI); rm(t2mI)

u10I <- mclapply(X = years,
                 FUN = pointInspection, mc.cores = length(years),
                 points = stations, var = "u10", prefix = "climate",
                 path = ".", parallel = TRUE)
u10 <- do.call(rbind.data.frame, u10I); rm(u10I)

v10I <- mclapply(X = years,
                 FUN = pointInspection, mc.cores = length(years),
                 points = stations, var = "v10", prefix = "climate",
                 path = ".", parallel = TRUE)
v10 <- do.call(rbind.data.frame, v10I); rm(v10I)

tpI <- mclapply(X = years,
                FUN = pointInspection, mc.cores = length(years),
                points = stations, var = "tp", prefix = "climate",
                path = ".", parallel = TRUE)
tp <- do.call(rbind.data.frame, tpI); rm(tpI)

blhI <- mclapply(X = years,
                 FUN = pointInspection, mc.cores = length(years),
                 points = stations, var = "blh", prefix = "climate",
                 path = ".", parallel = TRUE)
blh <- do.call(rbind.data.frame, blhI); rm(blhI)

ssrI <- mclapply(X = years,
                 FUN = pointInspection, mc.cores = length(years),
                 points = stations, var = "ssr", prefix = "climate",
                 path = ".", parallel = TRUE)
ssr <- do.call(rbind.data.frame, ssrI); rm(ssrI)

# Infill missing values (3 hour gap) using linear interpolation
t2mI <- mclapply(X = unique(t2m$SiteID),
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 df = t2m, maxgap = 3,
                 parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
t2m <- do.call(rbind.data.frame, t2mI); rm(t2mI)

u10I <- mclapply(X = unique(u10$SiteID),
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 df = u10, maxgap = 3,
                 parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
u10 <- do.call(rbind.data.frame, u10I); rm(u10I)

v10I <- mclapply(X = unique(v10$SiteID),
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 df = v10, maxgap = 3,
                 parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
v10 <- do.call(rbind.data.frame, v10I); rm(v10I)

tpI <- mclapply(X = unique(tp$SiteID),
                FUN = fillMissingValues, mc.cores = detectCores() - 1,
                df = tp, maxgap = 3,
                parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
tp <- do.call(rbind.data.frame, tpI); rm(tpI)

blhI <- mclapply(X = unique(blh$SiteID),
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 df = blh, maxgap = 3,
                 parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
blh <- do.call(rbind.data.frame, blhI); rm(blhI)

ssrI <- mclapply(X = unique(ssr$SiteID),
                 FUN = fillMissingValues, mc.cores = detectCores() - 1,
                 df = ssr, maxgap = 3,
                 parallel = TRUE, formatDT = "%Y-%m-%d %H:%M")
ssr <- do.call(rbind.data.frame, ssrI); rm(ssrI)

# Calculate wind speed and direction from u10 and v10
wsI <- windSpeed(u10$u10, v10$v10)
wdI <- mapply(windDirection, u10$u10, v10$v10)
# Build data frame starting from the basic columns: "datetime", "SiteID"
wind <- u10[, 1:2]
# then add wind speed (ws) and direction (wd)
wind$ws <- wsI
wind$wd <- wdI

library(dplyr)
# Fix data types before joining tables to avoid problems with factors
# str(t2m); str(wind) ...

# Start joining the columns from the more densely populated (if not filled in)
climate <- left_join(t2m, wind, by=c("datetime", "SiteID"))
climate <- left_join(climate, tp, by=c("datetime", "SiteID"))
climate <- left_join(climate, blh, by=c("datetime", "SiteID"))
climate <- left_join(climate, ssr, by=c("datetime", "SiteID"))

# saveRDS(climate, "climate.rds")
rm(wsI, wdI, years, u10, v10, blh, t2m, tp, wind, ssr); gc()

################################################################################
# GET HEALTH DATA (from the Office for National Statistics) ####################
################################################################################

# Health data in the UK is available from the Office for National Statistics
# The data used for this work was purchased under the KEHRA BC-grant.

library(gdata)

# Period 1981-1983
Deaths <- read.xls("ONS_purchasedData/1981to1983.xls",
                   sheet = "Table 1", header = TRUE, skip=7)
# Create the first batch of data
CVDcancer <- Deaths[Deaths$Cause == "CVD and Cancer",]
LiverDiseases <- Deaths[Deaths$Cause == "Liver Diseases",]

# Period 1984-1987
Deaths <- read.xls("ONS_purchasedData/1984to1987.xls",
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

# Period 1988-1992
Deaths <- read.xls("ONS_purchasedData/1988to1992.xls",
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

# Period 1993-1995
Deaths <- read.xls("ONS_purchasedData/1993to1995.xls",
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

# Period 1996-2000
Deaths <- read.xls("ONS_purchasedData/1996to2000.xls",
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

# Period 2001-2005
Deaths <- read.xls("ONS_purchasedData/2001to2005.xls",
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

# Period 2006-2010
Deaths <- read.xls("ONS_purchasedData/2006to2010.xls",
                   sheet = "Table 1", header = TRUE, skip=7)
# Append new data
CVDcancer <- rbind(CVDcancer, Deaths[Deaths$Cause == "CVD and Cancer",])
LiverDiseases <- rbind(LiverDiseases, Deaths[Deaths$Cause == "Liver Diseases",])

# Period 2011-2014
Deaths <- read.xls("ONS_purchasedData/20112014.xls",
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
CVDcancer <- CVDcancer[,c("DateByDay", "Region",
                          "Over0", "Over20", "Over40", "Over60")]

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
names(CVDcancer) <- c("DateByDay", "Region",
                      "CVDOver0", "CVDOver20", "CVDOver40", "CVDOver60")
names(LiverDiseases) <- c("DateByDay", "Region",
                          "LiverOver0", "LiverOver20",
                          "LiverOver40", "LiverOver60")

# Join
Health <- CVDcancer %>% left_join(LiverDiseases, by = c("DateByDay", "Region"))
Health$Year <- format(as.Date(Health$DateByDay), "%Y")
Health <- Health[, c("DateByDay", "Year", "Region",
                     "CVDOver0", "CVDOver20", "CVDOver40", "CVDOver60",
                     "LiverOver0", "LiverOver20", "LiverOver40", "LiverOver60")]
Health[,4:11] <- sapply(Health[,4:11], as.numeric)
Health$Region <- as.character(Health$Region)
Health$DateByDay <- as.character(Health$DateByDay)
Health$Region[Health$Region=="Yorkshire and the Humber"] <-
  "Yorkshire and The Humber"
rm(CVDcancer,LiverDiseases)

# Population estimates (MYEDE, Office for National Statistics)
# Downloaded from:
# http://web.ons.gov.uk/ons/data/dataset-finder/-/q/dcDetails/Social/MYEDE?p_p_lifecycle=1&_FOFlow1_WAR_FOFlow1portlet_dataset_navigation=datasetCollectionDetails

library(reshape2)
populationEstimates <- readRDS("PopulationEstimatesRegions1971_2014.rds")
populationEstimates[,1:2] <- sapply(populationEstimates[,1:2], as.character)
populationEstimates[,3:46] <- sapply(populationEstimates[,3:46], as.numeric)
populationEstimates <- melt(data = populationEstimates[, 2:46])
names(populationEstimates) <- c("Region", "Year", "YearlyPopEst")
populationEstimates$Year <- substr(populationEstimates$Year, 5,8)
populationEstimates$Region <- as.character(populationEstimates$Region)

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
HealthSocio$Region[HealthSocio$Region=="London"] <- "Greater London Authority"
# CHECK: unique(stations$Region) %in% HealthSocio$Region
HealthSocio <- HealthSocio[, c("DateByDay", "Region", "Year",
                               "CVD00", "CVD20", "CVD40", "CVD60",
                               "LIV00", "LIV20", "LIV40", "LIV60")]

# saveRDS(HealthSocio, "HealthSocio.rds")
rm(Health, populationEstimates); gc()

################################################################################
# ASSEMBLE DATABASE ############################################################
################################################################################

library(dplyr)

# Load pollution data and join with climate data
# climate <- readRDS("climate.rds")
# pollution <- readRDS("pollution.rds")
# Join pollution and climate data
exposure <- full_join(climate, pollution, by=c("datetime", "SiteID"))
rm(climate, pollution); gc()

# Expand time variables
datetime <- as.POSIXlt(exposure$datetime)
exposure$DateByDay <- as.character(format(datetime, "%Y-%m-%d"))
exposure$Year      <- as.character(format(datetime, "%Y"))
exposure$Month     <- as.character(format(datetime, "%m"))
exposure$Day       <- as.character(format(datetime, "%d"))
exposure$Hour      <- as.character(format(datetime, "%H"))
exposure$Season    <- as.character(getSeason(as.Date(exposure$DateByDay)))
# saveRDS(exposure, "exposure.rds")

# Join with stations to get the region/zone/type of monitoring
# stations <- readRDS("stations.rds")
expStation <- exposure %>% left_join(stations, by = "SiteID")

# Join exposure and healthsocio
# HealthSocio <- readRDS("HealthSocio.rds")
df <- expStation %>% left_join(HealthSocio,
                               by = c("DateByDay", "Region", "Year"))

# For the analysis, variables must be either numeric, factors or ordered factors
# Check variable types (factor/categorical or numeric) using str(df)
# Fix data types
df$SiteID           <- factor(df$SiteID)
df$Region           <- factor(df$Region)
df$Zone             <- factor(df$Zone)
df$Environment.Type <- factor(df$Environment.Type)
df$Year             <- factor(df$Year)
df$Season           <- factor(df$Season)
df$Month            <- factor(df$Month)
df$Day              <- factor(df$Day)
df$Hour             <- factor(df$Hour)

# Order the info (categorical first, then continouous), as.data.frame(names(df))
db <- df[,c("SiteID", "Region", "Zone", "Environment.Type",
            "Year", "Season", "Month", "Day", "Hour",
            "Latitude", "Longitude", "Altitude",
            "t2m", "ws", "wd", "tp", "blh", "ssr",
            "pm10", "pm2.5", "no2", "o3", "so2", "co",
            "CVD00", "CVD20", "CVD40", "CVD60",
            "LIV00", "LIV20", "LIV40", "LIV60")]
names(db)[4] <- "Type"

# saveRDS(db, "database.rds")
rm(exposure, stations, datetime, df, expStation, HealthSocio); gc()

################################################################################
# MAKE A MAP OF THE ACTIVE STATIONS ############################################
################################################################################
library(devtools)
library(ggmap)

# for theme_map
source_gist("33baa3a79c5cfef0f6df")

# Load data
stations <- readRDS("stations.rds")

myMAP <- get_map("birmingham, uk",zoom=6)
ggmap(myMAP) +
  geom_point(data = data.frame(stations), aes(x = Longitude, y = Latitude),
             alpha=0.5 , color = "red") +
  xlab("Longitude") + ylab("Latitude")

rm(stations, myMAP, theme_map); gc()

################################################################################
# FIND OUT TEMPORAL COVERAGE ###################################################
################################################################################

# db <- readRDS("database.rds")

# How many stations?
length(unique(db$SiteID)) # 162

# Range of temporal coverage
# remove all the rows containing only NAs in pollution features
ind <- apply(db[, c("pm10", "pm2.5", "no2", "o3", "so2", "co")], 1,
             function(y) all(is.na(y)))
# Group by site
grouped <- group_by(db[!ind, ], SiteID)
x <- summarise(grouped, uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 12 years

# How many stations measured O3? For how many years?
length(unique(db$SiteID[!is.na(db$o3)])) # 95 stations
x <- group_by(db[!is.na(db$o3),], SiteID) %>%
  summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 14 years

# How many stations measured CO? For how many years?
length(unique(db$SiteID[!is.na(db$co)])) # 80 stations
x <- group_by(db[!is.na(db$co),], SiteID) %>%
  summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 11 years

# How many stations measured NO2? For how many years?
length(unique(db$SiteID[!is.na(db$no2)])) # 146 stations
x <- group_by(db[!is.na(db$no2),], SiteID) %>%
  summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 12 years

# How many stations measured SO2? For how many years?
length(unique(db$SiteID[!is.na(db$so2)])) # 85 stations
x <- group_by(db[!is.na(db$so2),], SiteID) %>%
  summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 12 years

# How many stations measured PM10? For how many years?
length(unique(db$SiteID[!is.na(db$pm10)])) # 83 stations
x <- group_by(db[!is.na(db$pm10),], SiteID) %>%
  summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 11 years

# How many stations measured PM2.5? For how many years?
length(unique(db$SiteID[!is.na(db$pm2.5)])) # 64 stations
x <- group_by(db[!is.na(db$pm2.5),], SiteID) %>%
  summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 6 years

rm(grouped, x, ind)

################################################################################
# SPLIT THE DATASET INTO TRAINING AND TESTING SETS #############################
################################################################################

# db <- readRDS("database.rds")

# Training set contains all the data up to 2005, testing is data for 2006-14
ind <- which(as.numeric(as.character(db$Year)) <= 2005)

training <- db[ind, ] # ~74% round(dim(training)[1]/dim(db)[1],2)
testing <- db[-ind, ] # ~26% round(dim(testing)[1]/dim(db)[1],2)

saveRDS(training, "training.rds")
saveRDS(testing, "testing.rds")

rm(db, testing, ind); gc()

################################################################################
# PIE CHARTS ###################################################################
################################################################################

library(ggplot2)

# Pie Chart of Regions with Percentages
x <- as.data.frame(table(training$Region))
slicesX <- x$Freq
lblsX <- x$Var1
pctX <- round(slicesX/sum(slicesX)*100)
lblsX <- paste(lblsX, pctX) # add percents to labels
lblsX <- paste(lblsX,"%",sep="") # ad % to labels

# Pie Chart of Types with Percentages
y <- as.data.frame(table(training$Type))
slicesY <- y$Freq
lblsY <- y$Var1
pctY <- round(slicesY/sum(slicesY)*100)
lblsY <- paste(lblsY, pctY) # add percents to labels
lblsY <- paste(lblsY,"%",sep="") # ad % to labels

# Default size
size <- 480 # px

# chart Regions
png("Regions.png",
    width = size*3, height = size*2, units = "px", pointsize = 12, res=150)
pie(slicesX, labels = lblsX, col=rainbow(length(lblsX)),
    main="Regions")
dev.off()

# Chart Types
png(file = "Types.png",
    width = size*3, height = size*2, units = "px", pointsize = 12, res=150)
pie(slicesY,labels = lblsY, col=rainbow(length(lblsY)),
    main="Types")
dev.off()

# Use imageMagick to merge png images
# sudo apt-get install imagemagick
system('convert Regions.png Types.png -background none -append RegTypes.png')

rm(x, y, lblsX, lblsY, pctX, pctY, size, slicesX, slicesY)

################################################################################
# LEARN THE BAYESIAN NETWORK FROM OBSERVED FEATURES (in training set) ONLY #####
# WHILE USING THE EM ALGORITHM TO IMPUTE MISSING VALUES (USE BNLEARN PACKAGE) ##
################################################################################

# For bnlearn
# Bioconductor
# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")
# biocLite("RBGL")
# biocLite("Rgraphviz")

# CRAN
# install.packages(c("Matrix", "igraph", "Rcpp"))
# install.packages("gRbase")
# install.packages("gRain")
# install.packages("bnlearn")

library(bnlearn)

# Load the training set
# training <- readRDS("training.rds")

# Which columns do not contain NAs? colSums(is.na(training)) == 0
# Remove records with NAs in weather variables
training <- training[-which(is.na(training$ws)),]

### HYPOTHESIS: ONLY CVD CAUSES ARE RELEVANT ###################################
# (THIS WILL BE VERIFIED AT THE END!)
# Take a note of the column numbers using as.data.frame(names(training)), then
# remove SiteID, LIVXX columns and CDV00-40.
training <- training[, -c(1,25:27, 29:32)]
# remove NA from health variables
training <- training[-which(is.na(training$CVD60)), ]
# Re-order columns based on increasing number of NAs
training <- droplevels(training[, names(sort(colSums(is.na(training))))])
# saveRDS(training, "database_BeforeEM_allObsFeatures.rds")

### CORE SIMULATION ############################################################
# install.packages("~/bnlearn_4.1-20160803.tar.gz", repos = NULL, type = "source")
library(bnlearn)

training <- readRDS("/var/data/Modelling/UK/database_BeforeEM_allObsFeatures.rds")

# Define blacklist
bl <- data.frame("from" = c(rep("Region",10),
                            rep("Zone",10),
                            rep("Type",10),
                            rep("Year",10),
                            rep("Season",10),
                            rep("Month",10),
                            rep("Day",10),
                            rep("Hour",10),
                            rep("Latitude",10),
                            rep("Longitude",10),
                            rep("Altitude",10),
                            rep("CVD60",23),
                            rep("t2m",11),
                            rep("ws",11),
                            rep("wd",11),
                            rep("tp",11),
                            rep("blh",11),
                            rep("ssr",11),
                            rep("no2",11),
                            rep("so2",11),
                            rep("co",11),
                            rep("o3",11),
                            rep("pm10",11),
                            rep("pm2.5",11)),
                 "to" = c("Zone", "Type", "Year", "Season", "Month", "Day",
                          "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Type", "Year", "Season", "Month", "Day",
                          "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Year", "Season", "Month", "Day",
                          "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Season", "Month", "Day",
                          "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Month", "Day",
                          "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Day",
                          "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Month",
                          "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Month",
                          "Day", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Month",
                          "Day", "Hour", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Month",
                          "Day", "Hour", "Latitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Month",
                          "Day", "Hour", "Latitude", "Longitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "t2m","ws","wd","tp","blh","ssr",
                          "no2","o3","so2","co","pm10","pm2.5",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude",
                          "Region","Zone","Type",
                          "Year","Season","Month","Day","Hour",
                          "Latitude","Longitude","Altitude"))

# Complete observations
rowsCompleteObservations <- which(complete.cases(training))
completeObservations <- training[rowsCompleteObservations, ]

# Imputing Missing Data With Expectation – Maximization
# initialise using the empty graph and complete observations
dag <- empty.graph(names(completeObservations))
bnTarget <- bn.fit(dag, completeObservations)          # node.ordering(bnTarget)

saveRDS(object = dag, file = "~/currentDAG_loop0.rds")
saveRDS(object = bnTarget, file = "~/currentModel_loop0.rds")

# which variables have missing values?
which.missing <- names(which(sapply(training, function(x) anyNA(x))))

library(parallel)
# Calculate the number of cores and initiate cluster
cl <- makeCluster(detectCores() - 1, outfile="")
invisible(clusterEvalQ(cl, library(bnlearn)))

for(loopNumber in 1:5){
  
  print(paste("Loop n.", loopNumber, sep = ""))
  
  # E: replacing all missing values with their (E)xpectation.
  current <- training
  
  # Discard columns with zero or near-zero variance
  col2remove <- c()
  for (nCol in names(which(sapply(current, class) == 'factor'))){
    stringCol <- paste("bnTarget$", nCol, "$prob", sep = "")
    levelsCol <- names(which( eval(parse(text = stringCol)) != 0 ))
    if (!all(unique(current[,nCol]) %in% levelsCol)) {
      col2remove <- c(col2remove, nCol)
    }
  }
  col2remove <- which(names(current) %in% col2remove)
  current <- current[, -col2remove]
  
  # iterate over all the variables with missing data.
  res <- parSapply(cl, which.missing, function(myVar, current, bnTarget, n) {
    
    # variables that do no have any missing value for the observations/variable
    # combination we are imputing in this iteration.
    missing.to.predict = is.na(current[, myVar])
    complete.predictors = sapply(current, function(x) !any(is.na(x) & missing.to.predict))
    complete.predictors = names(which(complete.predictors))
    
    if (length(nbr(bnTarget, myVar)) == 0) {
      
      # if the node has no neighbours, there is no information to propagate
      # from other variables: just simulate from the marginal distribution
      # using a reduced standard deviation to match that from predict().
      rnorm(length(which(missing.to.predict)), mean = bnTarget[[myVar]]$coef,
            sd = bnTarget[[myVar]]$sd / sqrt(n))
      
    }
    else {
      
      predict(object = bnTarget,
              node = myVar,
              data = current[missing.to.predict, complete.predictors],
              method = "bayes-lw", n = n)
      
    }
    
  }, current = current, bnTarget = bnTarget, n = 50, simplify = FALSE)
  
  for (myVar in which.missing)
    current[is.na(current[, myVar]), myVar] = res[[myVar]]
  
  # ensure we garbage-collect what we have used until this point, as we will
  # not use it again.
  invisible(clusterEvalQ(cl, gc()))
  
  # M: learning the model that (M)aximises the score with the current data,
  # after we have imputed all the missing data in all the variables.
  imputedTraining <- data.frame(training[, 1:18, drop = FALSE],
                                current[, which.missing, drop = FALSE])
  
  # split the data and learn a collection of networks.
  set.seed(loopNumber)
  splitted <- split(sample(nrow(imputedTraining)), seq(length(cl)))
  # how many records are discarded? dim(training)[1] - 63*length(splitted[[1]])
  
  models <- parLapply(cl, splitted, function(samples, data, bl, dag) {
    
    hc(data[samples, , drop = FALSE], blacklist = bl, start = dag)
    
  }, data = imputedTraining, bl = bl, dag = dag) 
  
  # average the networks (and make sure the result is completely directed).
  dagNew <- averaged.network(custom.strength(models, nodes = names(imputedTraining)))
  dagNew <- cextend(dagNew)
  
  # ensure we garbage-collect what we have used until this point, as we will
  # not use it again.
  invisible(clusterEvalQ(cl, gc()))
  
  # learn the parameters of the averaged network.
  bnCurrent <- bn.fit(dagNew, imputedTraining, cluster = cl)
  
  # Save dag and fitted bn at each iteration
  saveRDS(object = dagNew, 
          file = paste("~/currentDAG_loop", loopNumber,".rds", sep = ""))
  saveRDS(object = bnCurrent, 
          file = paste("~/currentModel_loop", loopNumber,".rds", sep = ""))
  
  if (all.equal(dagNew, dag) == TRUE) {
    
    break
    
  }else{
    
    dag = dagNew
    bnTarget = bnCurrent
    
  }
  
}

stopCluster(cl)
