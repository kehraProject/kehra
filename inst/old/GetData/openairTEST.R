library(openair)

load(url("http://www.erg.kcl.ac.uk/downloads/Policy_Reports/AQdata/o3Measurements.RData"))
load(url("http://www.erg.kcl.ac.uk/downloads/Policy_Reports/AQdata/siteDetails.RData"))

## cut data into seasons 
## load plyr package 
library(plyr) 
o3Measurements <- cutData(o3Measurements, "season") 
## calculate means/maxes and merge... 
annual <- ddply(o3Measurements, .(site), numcolwise(mean), na.rm = TRUE) 
## by site AND season 
means <- ddply(o3Measurements, .(site, season), numcolwise(mean), na.rm = TRUE)
peaks <- ddply(o3Measurements, .(site, season), numcolwise(max), na.rm = TRUE) 
annual <- merge(annual, siteDetails, by = "site") 
means <- merge(means, siteDetails, by = "site") 
peaks <- merge(peaks, siteDetails, by = "site")
## now make first plot 
GoogleMapsPlot(annual, lat = "latitude", long = "longitude", pollutant = "o3", maptype = "roadmap", col = "jet")
GoogleMapsPlot(means, lat = "latitude", long = "longitude", pollutant = "o3", type = "season", maptype = "roadmap", col = "jet")


hc <- importAURN(site = "HRL", year = 2014, hc = TRUE) 
## now it is possible to see the hydrocarbons that behave most 
## similarly to one another 
corPlot(hc)
corPlot(hc, dendrogram = TRUE)

