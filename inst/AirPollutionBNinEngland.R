################################################################################
# Modelling air pollution, climate and health using Bayesian Networks:         #
# a case study of the English regions                                          #
#                                                                              #
# Author: Claudia Vitolo                                                       #
# Last updated: December 2016                                                  #
################################################################################

# Check for missing dependencies and install them
# packs <- c("devtools", "plyr", "dplyr", "raster", "sp", "rgdal", "xts", "zoo",
#            "parallel", "openair", "ncdf4", "gdata", "reshape2", "ggmap")
# new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# rm(new.packages, packs)

# Install/load RDEFRA and KEHRA packages dev version
# devtools::install_github("ropenscilabs/rdefra")
library(rdefra)
# devtools::install_github("kehraProject/kehra")
library(kehra)

################################################################################
# GET POLLUTION DATA (from the DEFRA UK-AIR website) ###########################
################################################################################

# Get spatial regions and station metadata using the rdefra package
load("/var/data/GEO/regions.rda")
data("stations")

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
                        "action=da_1&go=Go", sep = "")))
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
setwd("/var/data/Climate/Climate3H/")
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

# saveRDS(climate, "/var/data/Modelling/UK/climate.rds")
rm(wsI, wdI, years, u10, v10, blh, t2m, tp, wind, ssr); gc()

################################################################################
# GET HEALTH DATA (from the Office for National Statistics) ####################
################################################################################

# Health data in the UK is available from the Office for National Statistics
# The data used for this work was purchased under the KEHRA BC-grant.

setwd("/var/data/Health/UK/ONS_purchasedData/")

library("dplyr")
library("XLConnect")
Deaths <- as.data.frame(matrix(NA, nrow = 0, ncol = 26))

for (myfile in list.files(path = ".")){

  print(myfile)

  wb <- loadWorkbook(myfile)
  ws <- readWorksheet(wb, sheet = "Table 1", header = TRUE,
                      startRow = 8, startCol = 1, endCol = 26)
  Deaths <- rbind(Deaths, ws)

}

# Separate data by causes
CVDcancer <- Deaths[tolower(Deaths$Cause) == "cvd and cancer",]
LiverDiseases <- Deaths[tolower(Deaths$Cause) == "liver diseases",]

rm(Deaths, wb, ws, myfile)

# Aggregate Deaths by age groups
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

library("reshape2")
populationEstimates <- readRDS("/var/data/GEO/PopulationEstimatesUK/PopulationEstimatesRegions1971_2014.rds")
populationEstimates[,1:2] <- sapply(populationEstimates[,1:2], as.character)
populationEstimates[,3:46] <- sapply(populationEstimates[,3:46], as.numeric)
populationEstimates <- melt(data = populationEstimates[, 2:46],
                            id.vars = "Geographic.Area")
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
HealthSocio <- HealthSocio[, c("DateByDay", "Region", "Year",
                               "CVD00", "CVD20", "CVD40", "CVD60",
                               "LIV00", "LIV20", "LIV40", "LIV60")]

# saveRDS(HealthSocio, "/var/data/Health/UK/HealthSocio.rds")
rm(Health, populationEstimates); gc()

################################################################################
# ASSEMBLE DATABASE ############################################################
################################################################################

library(dplyr)
library(kehra)

# Load pollution data and join with climate data
# climate <- readRDS("/var/data/Modelling/UK/climate.rds")
# pollution <- readRDS("/var/data/Pollution/AirPollutionUK/pollution.rds")
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
rm(datetime)
# saveRDS(exposure, "/var/data/Modelling/UK/exposure.rds")
# exposure <- readRDS("/var/data/Modelling/UK/exposure.rds")

# Join with stations to get the region/zone/type of monitoring
# stations <- readRDS("/var/data/Pollution/AirPollutionUK/stations.rds")
expStation <- exposure %>% left_join(stations, by = "SiteID")

# Join exposure and healthsocio
# HealthSocio <- readRDS("/var/data/Health/UK/HealthSocio.rds")
# CHECK: unique(stations$Region) %in% HealthSocio$Region
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

# Check whether health data were populated correctly
HealthSocio$CVD60[which(HealthSocio$Year == "2014")]
summary(db$CVD60[which(db$Year == "2014")], na.rm=TRUE)

# saveRDS(db, "/var/data/Modelling/UK/database.rds")
rm(exposure, stations, df, expStation, HealthSocio); gc()

################################################################################
# MAKE A MAP OF THE ACTIVE STATIONS ############################################
################################################################################
library(devtools)
library(ggmap)

# for theme_map
source_gist("33baa3a79c5cfef0f6df")

# Load data
stations <- readRDS("stations.rds")

myMAP <- get_map("birmingham, uk", zoom=6, maptype = "toner-lite")

pdf(file = "stations.pdf", width = 5, height = 5)
ggmap(myMAP) +
  geom_point(data = data.frame(stations), aes(x = Longitude, y = Latitude),
             alpha=0.3, color = "red") +
  xlab("Longitude") + ylab("Latitude")
dev.off()

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
# CLEAN UP THE DATABASE ########################################################
################################################################################

# Pre-processing
# db <- readRDS("database.rds")
# Which columns do not contain NAs? colSums(is.na(db)) == 0
# Remove records with NAs in weather variables
db2 <- db[-which(is.na(db$ws)),]

# Important HYPOTHESIS: ONLY CVD CAUSES ARE RELEVANT (TO BE VERIFIED!)
# Take a note of the column numbers using as.data.frame(names(db2)), then
# remove SiteID, LIVXX columns and CDV00-40.
db2 <- db2[, -c(1,25:27, 29:32)]
# remove NA from health variables
db2 <- db2[-which(is.na(db2$CVD60)), ]
# Re-order columns based on increasing number of NAs
db2 <- droplevels(db2[, names(sort(colSums(is.na(db2))))])
# saveRDS(db2, "/var/data/Modelling/UK/database_removedNAfromWeatherHealth.rds")
# db2 <- readRDS("/var/data/Modelling/UK/database_removedNAfromWeatherHealth.rds")

# Remove rows with negative (concentrations) artifacts
summary(db2$t2m)
min(db2$tp)
db2$tp[which(db2$tp < 0)] <- 0
min(db2$no2, na.rm = TRUE)
db2$no2[which(db2$no2 < 0)] <- NA
min(db2$o3, na.rm = TRUE)
db2$o3[which(db2$o3 < 0)] <- NA
min(db2$so2, na.rm = TRUE)
db2$so2[which(db2$so2 < 0)] <- NA
min(db2$pm10, na.rm = TRUE)
db2$pm10[which(db2$pm10 < 0)] <- NA
summary(db2$pm10)
min(db2$pm2.5, na.rm = TRUE)
db2$pm2.5[which(db2$pm2.5 < 0)] <- NA
summary(db2$pm2.5)
min(db2$co, na.rm = TRUE)
db2$co[which(db2$co < 0)] <- NA
summary(db2$co)

################################################################################
# SPLIT THE DATASET INTO TRAINING AND TESTING SETS #############################
################################################################################

# Training set contains all the data up to 2005, testing is data for 2006-14
ind <- which(as.numeric(as.character(db2$Year)) <= 2005)

training <- db2[ind, ] # ~74% round(dim(training)[1]/dim(db2)[1],2)
testing <- db2[-ind, ] # ~26% round(dim(testing)[1]/dim(db2)[1],2)

saveRDS(training, "/var/data/Modelling/UK/training.rds")
saveRDS(testing, "/var/data/Modelling/UK/testing.rds")

rm(list=ls(all=TRUE)); gc()

################################################################################
# PIE CHARTS ###################################################################
################################################################################

library(ggplot2)

training <- readRDS("/var/data/Modelling/UK/training.rds")

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
cbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# chart Regions
png("Regions.png",
    width = size*3, height = size*2, units = "px", pointsize = 12, res=150)
pie(slicesX, labels = lblsX, col=cbPalette,
    main="Regions")
dev.off()

# Chart Types
png(file = "Types.png",
    width = size*3, height = size*2, units = "px", pointsize = 12, res=150)
pie(slicesY,labels = lblsY, col=cbPalette[3:length(cbPalette)],
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

### CORE SIMULATION ############################################################
# Install dev version of bnlearn
# install.packages("bnlearn_4.1-20160803.tar.gz", repos = NULL, type = "source")
library(bnlearn)
library(parallel)

training<-readRDS("/var/data/Modelling/UK/training.rds")

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
bnTarget <- bn.fit(dag, completeObservations)

# which variables have missing values?
which.missing <- names(which(sapply(training, function(x) anyNA(x))))

print("Setting up a cluster")
cl <- makeCluster(getOption("cl.cores", 15))
invisible(clusterEvalQ(cl, library(bnlearn)))

for(loopNumber in 1:10){
  
  print(paste("********* Loop n.", loopNumber, sep = ""))
  
  print("E: replacing all missing values with their (E)xpectation.")
  current <- training
  
  print("Discard columns with zero or near-zero variance")
  col2remove <- c()
  for (nCol in names(which(sapply(current, class) == 'factor'))){
    stringCol <- paste("bnTarget$", nCol, "$prob", sep = "")
    levelsCol <- names(which( eval(parse(text = stringCol)) != 0 ))
    if (!all(unique(current[,nCol]) %in% levelsCol)) {
      col2remove <- c(col2remove, nCol)
    }
  }
  col2remove <- which(names(current) %in% col2remove)
  if (length(col2remove) >= 1) current <- current[, -col2remove]
  
  print("iterate over all the variables with missing data.")
  res <- parSapply(cl, which.missing, function(myVar, current, bnTarget, n) {
    
    # variables that do no have any missing value for the observations/variable
    # combination we are imputing in this iteration.
    missing.to.predict <- is.na(current[, myVar])
    complete.predictors <- sapply(current,
                                  function(x) !any(is.na(x) &
                                                     missing.to.predict))
    complete.predictors <- names(which(complete.predictors))
    
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
  
  print(paste("ensure we garbage-collect what we have used until this point,",
              "as we will not use it again."))
  invisible(clusterEvalQ(cl, gc()))
  
  print(paste("M: learning the model that (M)aximises the score with the",
              "current data, after we have imputed all the missing data in all",
              "the variables."))
  imputedTraining <- data.frame(training[, 1:18, drop = FALSE],
                                current[, which.missing, drop = FALSE])
  
  print("split the data and learn a collection of networks.")
  splitted <- split(sample(nrow(imputedTraining)), seq(length(cl)))
  
  models <- parLapply(cl, splitted, function(samples, data, bl, dag) {
    
    hc(data[samples, , drop = FALSE], blacklist = bl, start = dag)
    
  }, data = imputedTraining, bl = bl, dag = dag)
  
  print("average the networks and make sure the result is completely directed.")
  dagNew <- averaged.network(custom.strength(models,
                                             nodes = names(imputedTraining)))
  dagNew <- cextend(dagNew)
  
  print("Save the model at each iteration")
  saveRDS(object = dagNew,
          file = paste("~/currentDAG_loop", loopNumber,".rds", sep = ""))
  
  print(paste("ensure we garbage-collect what we have used until this point,",
              "as we will not use it again."))
  invisible(clusterEvalQ(cl, gc()))
  
  print("learn the parameters of the averaged network.")
  bnCurrent <- bn.fit(dagNew, imputedTraining, cluster = cl)
  
  print("Save fitted bn at each iteration")
  saveRDS(object = bnCurrent,
          file = paste("~/currentModel_loop", loopNumber,".rds", sep = ""))
  
  print("Prepare for next iteration")
  dag <- dagNew
  bnTarget <- bnCurrent
  
  rm(dagNew, bnCurrent)
  
}

print("Stopping the cluster")
stopCluster(cl)

# COMPARE DAGS AND CHECK FOR CONVERGENCE #######################################

library(bnlearn)

dag1 <- readRDS("/var/data/Modelling/UK/BN/currentDAG_loop1.rds")
dag2 <- readRDS("/var/data/Modelling/UK/BN/currentDAG_loop2.rds")
all.equal(dag1,dag2)
compare(dag1,dag2, arcs = TRUE)

dag3 <- readRDS("/var/data/Modelling/UK/BN/currentDAG_loop3.rds")
all.equal(dag2,dag3)
compare(dag2,dag3, arcs = TRUE)

dag4 <- readRDS("/var/data/Modelling/UK/BN/currentDAG_loop4.rds")
all.equal(dag3,dag4)
compare(dag3,dag4, arcs = TRUE)

dag5 <- readRDS("/var/data/Modelling/UK/BN/currentDAG_loop5.rds")
all.equal(dag4,dag5)
compare(dag4,dag5, arcs = TRUE)

dag6 <- readRDS("/var/data/Modelling/UK/BN/currentDAG_loop6.rds")
all.equal(dag5,dag6)
compare(dag5,dag6, arcs = TRUE)

dag7 <- readRDS("/var/data/Modelling/UK/BN/currentDAG_loop7.rds")
all.equal(dag6,dag7)
compare(dag6,dag7, arcs = TRUE)

dag8 <- readRDS("/var/data/Modelling/UK/BN/currentDAG_loop8.rds")
all.equal(dag7,dag8)
compare(dag7,dag8, arcs = TRUE)

# THERE IS NO ABSOLUTE CONVERGENCE ... WE TAKE THE LATEST NETWORK!
dag <- dag8

rm(list=ls(all=TRUE))

# PLOT NETWORK #################################################################
# (work in progress!)
library(bnlearn)
DAG <- readRDS("/var/data/Modelling/UK/BN/currentDAG_loop8.rds")
BN <- readRDS("/var/data/Modelling/UK/BN/currentModel_loop8.rds")
training <- readRDS("/var/data/Modelling/UK/old/database_BeforeEM_allObsFeatures.rds")

# source("https://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
library(graph)
library(Rgraphviz)

nodes <- names(DAG$nodes)
arcs <- DAG$arcs
layout <- "dot"
attrs <- list(node = list(fixedsize="FALSE", fillcolor=""),
              edge=list(color="grey"),
              graph=list(rankdir="LR"))

graph.obj = new("graphNEL",
                nodes = nodes,
                edgeL = bnlearn:::arcs2elist(arcs, nodes),
                edgemode = "directed")

svg(filename="BN_SVG.svg",
    width=10,
    height=10,
    pointsize=1)
plot(graph.obj, attrs = attrs)
dev.off()

# Generate sub-graphs
BN$CVD60$parents # "Region" "Year"   "Season" "Month"
BN$Region$parents
BN$Year$parents
BN$Season$parents
BN$Month$parents

# subgraphCV <- function(DAG, node){
#
#   nAttrs <- list(color = c(eval(parse(text = paste0(node, "=", "'red'")))),
#                  fontcolor = c(eval(parse(text = paste0(node, "=", "'red'")))))
#   plot(subGraph(c(node,
#                   eval(parse(text = paste0("BN$", node, "$parents"))),
#                   eval(parse(text = paste0("BN$", node, "$children"))),
#                   graph.obj)), nodeAttrs=nAttrs)
#
# }

nAttrs <- list(color = c("CVD60" = "red"), fontcolor = c("CVD60" = "red"))
plot(subGraph(c("CVD60", BN$CVD60$parents), graph.obj), nodeAttrs=nAttrs)

# SCENARIOS TESTING
nAttrs <- list(color = c("o3" = "red"), fontcolor = c("o3" = "red"))
plot(subGraph(c("o3", BN$o3$parents), graph.obj), nodeAttrs=nAttrs)

# What's the average temperature in Kelvin for the training dataset?
summary(training$t2m) # Mean = 282.8K
# What's the O3 average concentrationa in case of a 10% level increase?
summary(training$o3*1.1) # Mean = 44micrograms/m3

# OZONE
# What is the conditional probability that O3 increases of X% if T2M increases in average 2.4K?
set.seed(1)
newO3_noEvidence <- mean(unlist(cpdist(fitted = BN, nodes = "o3",  evidence = TRUE, n = 1000000)), na.rm = TRUE) 

print("Racherla and Adams (2006) & Avise et al. (submitted for publication) & Meleux et al. (2007) - Likely temp. range (deg C) 2-5.4")
newO3min <-  mean(unlist(cpdist(fitted = BN, nodes = "o3",  evidence = (t2m >= 282.8 + 2), n = 1000000)), na.rm = TRUE) 
newO3max <-  mean(unlist(cpdist(fitted = BN, nodes = "o3",  evidence = (t2m >= 282.8 + 5.4), n = 1000000)), na.rm = TRUE) 
print(paste("MIN:", newO3min, "(change", round(-100*(newO3_noEvidence - newO3min)/newO3_noEvidence,2), "%)"))  # increase 4.54%
print(paste("MAX:", newO3max, "(change", round(-100*(newO3_noEvidence - newO3max)/newO3_noEvidence,2), "%)"))  # increase 8.02%

print("Kunkel et al. (2007) first scenario & Lin et al. (2008a) - 2.4-6")
newO3min <-  mean(unlist(cpdist(fitted = BN, nodes = "o3",  evidence = (t2m >= 282.8 + 2.4), n = 1000000)), na.rm = TRUE) 
newO3max <-  mean(unlist(cpdist(fitted = BN, nodes = "o3",  evidence = (t2m >= 282.8 + 6), n = 1000000)), na.rm = TRUE) 
print(paste("MIN:", newO3min, "(change", round(-100*(newO3_noEvidence - newO3min)/newO3_noEvidence,2), "%)"))  # increase 4.89%
print(paste("MAX:", newO3max, "(change", round(-100*(newO3_noEvidence - newO3max)/newO3_noEvidence,2), "%)"))  # increase 8.71%

print("Kunkel et al. (2007) second scenario & Lin et al. (2008a) - 1.1-2.9")
newO3min <-  mean(unlist(cpdist(fitted = BN, nodes = "o3",  evidence = (t2m >= 282.8 + 1.1), n = 1000000)), na.rm = TRUE) 
newO3max <-  mean(unlist(cpdist(fitted = BN, nodes = "o3",  evidence = (t2m >= 282.8 + 2.9), n = 1000000)), na.rm = TRUE) 
print(paste("MIN:", newO3min, "(change", round(-100*(newO3_noEvidence - newO3min)/newO3_noEvidence,2), "%)"))  # increase 4.01%
print(paste("MAX:", newO3max, "(change", round(-100*(newO3_noEvidence - newO3max)/newO3_noEvidence,2), "%)"))  # increase 5.17%

print("Tagaris et al. (2007) & Nolte et al. (2008) & Wu et al. (2008a) - 1.7-4.4")
newO3min <-  mean(unlist(cpdist(fitted = BN, nodes = "o3",  evidence = (t2m >= 282.8 + 1.7), n = 1000000)), na.rm = TRUE) 
newO3max <-  mean(unlist(cpdist(fitted = BN, nodes = "o3",  evidence = (t2m >= 282.8 + 4.4), n = 1000000)), na.rm = TRUE) 
print(paste("MIN:", newO3min, "(change", round(-100*(newO3_noEvidence - newO3min)/newO3_noEvidence,2), "%)"))  # increase 4.42%
print(paste("MAX:", newO3max, "(change", round(-100*(newO3_noEvidence - newO3max)/newO3_noEvidence,2), "%)"))  # increase 6.90%

print("Meleux et al. (2007) - 1.4-3.8")
newO3min <-  mean(unlist(cpdist(fitted = BN, nodes = "o3",  evidence = (t2m >= 282.8 + 1.4), n = 1000000)), na.rm = TRUE) 
newO3max <-  mean(unlist(cpdist(fitted = BN, nodes = "o3",  evidence = (t2m >= 282.8 + 3.8), n = 1000000)), na.rm = TRUE) 
print(paste("MIN:", newO3min, "(change", round(-100*(newO3_noEvidence - newO3min)/newO3_noEvidence,2), "%)"))  # increase 4.22%
print(paste("MAX:", newO3max, "(change", round(-100*(newO3_noEvidence - newO3max)/newO3_noEvidence,2), "%)"))  # increase 6.16%

# PARTICULATE MATTER
# What is the conditional probability that pm10 increases of X% if T2M increases in average 2.4K?
set.seed(1)
newpm10_noEvidence <- mean(unlist(cpdist(fitted = BN, nodes = "pm10",  evidence = TRUE, n = 1000000)), na.rm = TRUE)
newpm25_noEvidence <- mean(unlist(cpdist(fitted = BN, nodes = "pm2.5",  evidence = TRUE, n = 1000000)), na.rm = TRUE) 

print("Liao et al., 2006 and Racherla and Adams, 2006 Likely temp. range 2 - 5.4")
newpm10min <-  mean(unlist(cpdist(fitted = BN, nodes = "pm10",  evidence = (t2m >= 282.8 + 2), n = 1000000)), na.rm = TRUE) 
newpm10max <-  mean(unlist(cpdist(fitted = BN, nodes = "pm10",  evidence = (t2m >= 282.8 + 5.4), n = 1000000)), na.rm = TRUE) 
print(paste("MIN:", newpm10min, "(change", round(-100*(newpm10_noEvidence - newpm10min)/newpm10_noEvidence,2), "%)")) # increase -0.15%
print(paste("MAX:", newpm10max, "(change", round(-100*(newpm10_noEvidence - newpm10max)/newpm10_noEvidence,2), "%)")) # increase -0.08%

print("Tagaris et al. (2007) Likely temp. range 1.7 - 4.4")
newpm25min <-  mean(unlist(cpdist(fitted = BN, nodes = "pm2.5",  evidence = (t2m >= 282.8 + 1.7), n = 1000000)), na.rm = TRUE) 
newpm25max <-  mean(unlist(cpdist(fitted = BN, nodes = "pm2.5",  evidence = (t2m >= 282.8 + 4.4), n = 1000000)), na.rm = TRUE) 
print(paste("MIN:", newpm25min, "(change", round(-100*(newpm25_noEvidence - newpm25min)/newpm25_noEvidence,2), "%)"))  # increase -0.01%
print(paste("MAX:", newpm25max, "(change", round(-100*(newpm25_noEvidence - newpm25max)/newpm25_noEvidence,2), "%)"))  # increase -0.02%

print("Unger et al. (2006) Likely temp. range 1.1 - 2.9")
newpm10min <-  mean(unlist(cpdist(fitted = BN, nodes = "pm10",  evidence = (t2m >= 282.8 + 1.1), n = 1000000)), na.rm = TRUE) 
newpm10max <-  mean(unlist(cpdist(fitted = BN, nodes = "pm10",  evidence = (t2m >= 282.8 + 2.9), n = 1000000)), na.rm = TRUE) 
print(paste("MIN:", newpm10min, "(change", round(-100*(newpm10_noEvidence - newpm10min)/newpm10_noEvidence,2), "%)"))  # increase -0.2%
print(paste("MAX:", newpm10max, "(change", round(-100*(newpm10_noEvidence - newpm10max)/newpm10_noEvidence,2), "%)"))  # increase -0.1%

print("Heald et al. (2008) & Spracklen et al. (submitted for publication) & Pye et al. (in press) Likely temp. range 1.7 - 4.4")
newpm10min <-  mean(unlist(cpdist(fitted = BN, nodes = "pm10",  evidence = (t2m >= 282.8 + 1.7), n = 1000000)), na.rm = TRUE) 
newpm10max <-  mean(unlist(cpdist(fitted = BN, nodes = "pm10",  evidence = (t2m >= 282.8 + 4.4), n = 1000000)), na.rm = TRUE) 
print(paste("MIN:", newpm10min, "(change", round(-100*(newpm10_noEvidence - newpm10min)/newpm10_noEvidence,2), "%)"))  # increase -0.11%
print(paste("MAX:", newpm10max, "(change", round(-100*(newpm10_noEvidence - newpm10max)/newpm10_noEvidence,2), "%)"))  # increase -0.10%

print("Avise et al. (submitted for publication) Likely temp. range 2 - 5.4")
newpm25min <-  mean(unlist(cpdist(fitted = BN, nodes = "pm2.5",  evidence = (t2m >= 282.8 + 2), n = 1000000)), na.rm = TRUE) 
newpm25max <-  mean(unlist(cpdist(fitted = BN, nodes = "pm2.5",  evidence = (t2m >= 282.8 + 5.4), n = 1000000)), na.rm = TRUE) 
print(paste("MIN:", newpm25min, "(change", round(-100*(newpm25_noEvidence - newpm25min)/newpm25_noEvidence,2), "%)"))  # increase -0.01%
print(paste("MAX:", newpm25max, "(change", round(-100*(newpm25_noEvidence - newpm25max)/newpm25_noEvidence,2), "%)"))  # increase 0%


################################################################################
# Analysis of residuals (TRAINING DATASET)
# Calculate RMSE which indicate the average deviation of the estimates from the actual values
for (i in 9:dim(training)[2]){
  x <- BN[[i]]
  RMSE <- sqrt(mean((x$fitted.values - training[,i])^2, na.rm = TRUE))
  normRMSE <- RMSE/(max(training[,i], na.rm = TRUE)-min(training[,i], na.rm = TRUE))
  print(paste(names(training)[i], round(normRMSE, 2)))
}

# Analysis of residuals (TESTING DATASET)
testing <- readRDS("/var/data/Modelling/UK/testing.rds")
print("Discard columns with zero or near-zero variance")
col2remove <- c()
for (nCol in names(which(sapply(testing, class) == 'factor'))){
  stringCol <- paste("BN$", nCol, "$prob", sep = "")
  levelsCol <- names(which( eval(parse(text = stringCol)) != 0 ))
  if (!all(unique(testing[,nCol]) %in% levelsCol)) {
    col2remove <- c(col2remove, nCol)
  }
}
col2remove <- which(names(testing) %in% col2remove)
names(testing)[col2remove]    # Disregard Year when predicting
if (length(col2remove) >= 1) testing <- testing[, -col2remove]

# Predict continuous variables from testing dataset
# as.data.frame(names(testing))
for (myVar in names(testing)[8:23]){
  temp <- testing[complete.cases(testing[, -which(names(testing) == myVar)]),]
  myCol <- which(names(temp) == myVar)
  newCVD60 <- predict(object = BN, node = myVar,
                      data = temp[, -myCol], method = "bayes-lw", n = 50)
  myVarCol <- temp[,myCol]
  RMSE <- sqrt(mean((newCVD60 - myVarCol)^2, na.rm = TRUE))
  normRMSE <- RMSE/(max(myVarCol, na.rm = TRUE)-min(myVarCol, na.rm = TRUE))
  print(paste(myVar, round(normRMSE, 2)))
}

### OTHER TESTS
BN$CVD60$dlevels$Region[3]
# What's the probability that CVD60 > 0.3, given the Region is London?
bnlearn::cpquery(BN, CVD60 > 0.1, Region == BN$CVD60$dlevels$Region[3])
bnlearn::cpquery(BN, CVD60 > 0.1, (Region == BN$CVD60$dlevels$Region[3] &
                                     Season == BN$CVD60$dlevels$Season[3]))
bnlearn::cpquery(BN, CVD60 > 0.3, Region == BN$CVD60$dlevels$Region[1])
bnlearn::cpquery(BN, CVD60 > 0.3, Region == BN$CVD60$dlevels$Region[2])
bnlearn::cpquery(BN, CVD60 > 0.3, Region == BN$CVD60$dlevels$Region[4])
bnlearn::cpquery(BN, CVD60 > 0.3, Region == BN$CVD60$dlevels$Region[5])
bnlearn::cpquery(BN, CVD60 > 0.3, Region == BN$CVD60$dlevels$Region[6])
bnlearn::cpquery(BN, CVD60 > 0.3, Region == BN$CVD60$dlevels$Region[7])
bnlearn::cpquery(BN, CVD60 > 0.3, Region == BN$CVD60$dlevels$Region[8])
bnlearn::cpquery(BN, CVD60 > 0.3, Region == BN$CVD60$dlevels$Region[9])


x <- BN$CVD60$fitted.values
bnlearn::cpquery(BN, CVD60 > 0.1, Region == BN$CVD60$dlevels$Region[3])


x <- BN$pm2.5$coefficients

y <- BN$pm2.5$configs



nAttrs <- list(color = c("Region" = "red"), fontcolor = c("Region" = "red"))
plot(subGraph(c("Region", BN$Region$children), graph.obj), nodeAttrs=nAttrs)

nAttrs <- list(color = c("pm2.5" = "red"), fontcolor = c("pm2.5" = "red"))
plot(subGraph(c("pm2.5", BN$pm2.5$parents, BN$pm2.5$children), graph.obj), nodeAttrs=nAttrs)

plot(subGraph(c("Region", BN$Region$children), graph.obj))
plot(subGraph(c("Year", BN$Year$children), graph.obj))
plot(subGraph(c("Month", BN$Month$children), graph.obj))
#plot(subGraph(c("Season", BN$Season$children), graph.obj))
plot(subGraph(c("blh", BN$blh$children), graph.obj))

nAttrs <- list(color = c("no2" = "red"), fontcolor = c("no2" = "red"))
plot(subGraph(c("no2", BN$no2$parents, BN$no2$children), graph.obj), nodeAttrs=nAttrs)
plot(subGraph(c("o3", BN$o3$parents, BN$o3$children), graph.obj), nodeAttrs=nAttrs)

hlight <- list(nodes = nodes(DAG), arcs = arcs(DAG),
               col = "grey", textCol = "grey")

pp <- graphviz.plot(DAG, highlight = hlight, layout = "dot",
                    shape = "circle", main = NULL, sub = NULL)

edgeRenderInfo(pp) <- list(col = c())
