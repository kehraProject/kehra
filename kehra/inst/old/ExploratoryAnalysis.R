# Locate stations in England, active between 1998 and 2013
# library(ggmap)
# p <- get_map("England", zoom = 6)
# ggmap(p) + geom_point(data=siteDetails, aes(x=longitude, y=latitude), color="red", size=3, alpha=0.5) + 
#   geom_point(data=x, aes(x=Longitude, y=Latitude), color="blue", size=3, alpha=0.5)
stations <- readRDS("data/Pollution/openairStations.rds")
library(leaflet)
leaflet(data = stations) %>% addTiles() %>% addCircleMarkers(radius = 5, popup = ~Name)

# Statistical analysis at 1 location: London Harlington (UKA00472)
# Data is available from ("http://uk-air.defra.gov.uk/data/flat_files?site_id=HRL"), from 2004 to 2015. 

rm(list = ls())
closeAllConnections()

require(INLA)
require(maptools)
require(lattice)
library(manipulate)

# install.packages("openair")
library(openair)
HRL <- importAURN(site = "HRL", year = 2004:2015) # Harlington
MY1 <- importAURN(site = "MY1", year = 2004:2015) # Marylebone Road
mySites <- c(HRL, MY1)

# DEFRA Daily Air Quality Index (DAQI)
## the labels - same for all species 
labels <- c("1 - Low", "2 - Low", "3 - Low", "4 - Moderate", "5 - Moderate", 
            "6 - Moderate", "7 - High", "8 - High", "9 - High", "10 - Very High")
o3.breaks <-c(0, 34, 66, 100, 121, 141, 160, 188, 214, 240, 500) 
no2.breaks <- c(0, 67, 134, 200, 268, 335, 400, 468, 535, 600, 1000) 
pm10.breaks <- c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000) 
pm25.breaks <- c(0, 12, 24, 35, 42, 47, 53, 59, 65, 70, 1000)

manipulate(calendarPlot(eval(parse(text=site)), year = x, 
                        pollutant = "pm10", annotate = "value", 
                        labels = labels, breaks = pm10.breaks,
                        lim = pm10.breaks[4], col.lim = c("black", "orange"),
                        statistic = "mean", cols = "jet"),
           x = slider(2004, 2015),
           site = picker("HRL", "MY1"))

## no2 index example 
manipulate(calendarPlot(eval(parse(text=site)), year = x, 
                        pollutant = "no2", annotate = "value", 
                        labels = labels, breaks = no2.breaks,
                        lim = no2.breaks[4], col.lim = c("black", "orange"),
                        statistic = "max", cols = "jet"),
           x = slider(2004, 2015),
           site = picker("HRL", "MY1"))
## for PM10 or PM2.5 we need the daily mean concentration 
manipulate(calendarPlot(eval(parse(text=site)), year = x, 
                        pollutant = "pm10", annotate = "value", 
                        labels = labels, breaks = pm10.breaks,
                        lim = pm10.breaks[4], col.lim = c("black", "orange"),
                        statistic = "mean", cols = "jet"),
           x = slider(2004, 2015),
           site = picker("HRL", "MY1"))
manipulate(calendarPlot(eval(parse(text=site)), year = x, 
                        pollutant = "pm25", annotate = "value", 
                        labels = labels, breaks = pm25.breaks,
                        lim = pm25.breaks[4], col.lim = c("black", "orange"),
                        statistic = "mean", cols = "jet"),
           x = slider(2004, 2015),
           site = picker("HRL", "MY1"))
## for ozone, need the rolling 8-hour mean 
manipulate(calendarPlot(rollingMean(eval(parse(text=site)), 
                                    pollutant = "o3", hours = 8),
                        year = x, 
                        pollutant = "rolling8o3", annotate = "value", 
                        labels = labels, breaks = o3.breaks,
                        lim = o3.breaks[4], col.lim = c("black", "orange"),
                        statistic = "max", cols = "jet"),
           x = slider(2004, 2015),
           site = picker("HRL", "MY1"))

# Let's try a summary plot for MY1
summaryPlot(MY1[,c("date", "co", "pm10", "no", "no2", "nox", "o3", "site", "code", "pm2.5", "ws", "wd")])
# Let's try a summary plot for HRL
summaryPlot(HRL[,c("date", "co", "pm10", "no", "no2", "nox", "o3", "site", "code", "pm2.5", "ws", "wd")])
# Correlation matrix + dendogram at HRL
corPlot(HRL[,c("date", "co", "pm10", "no", "no2", "nox", "o3", "site", "code", "pm2.5", "ws", "wd")], dendrogram = TRUE)

# Correlation matrix + dendogram for all AURN stations in 2014
df <- readRDS("data/Pollution/allPollutantsClima.rds")
corPlot(df, dendrogram = TRUE)

# Explore table
names(df)
# "Date", "id", "Longitude", "Latitude", "Site", "Region", "Z",
# "PM10", "PM2p5", "CO", "O3", "NO2", "SO2", "TEMP", "WS", "WD", "HMIX", "PREC"
head(df)

df1 <- na.omit(df[,c("Longitude", "Latitude", "Z",
                     "PM10", "PM2p5", "CO", "O3", "NO2", "SO2", 
                     "TEMP", "WS", "WD", "HMIX", "PREC")])

# There are some NAs in columns 9:13, remove rows with all NAs
temp <- df[,c("PM10", "PM2p5", "CO", "O3", "NO2", "SO2")]
df2 <- df[rowSums(is.na(temp)) != ncol(temp),]
# Keep only complete cases (rows with no NAs)
df3 <- df[complete.cases(temp),]

df0 <- df[complete.cases(df[,c("PM10", "TEMP", "PREC")]), 
          c("PM10", "TEMP", "WS", "WD", "HMIX", "PREC")]
