################################################################################
######################## NUCLEAR TEST SITES ####################################
################################################################################

setwd('~/Dropbox/Projects/kehra')
# Get list of nuclear test sites from Wikipedia's page

# Code adapted from Carson Sievert's examples:
# http://cpsievert.github.io/slides/web-scraping
library(rvest)
theurl <- "https://en.wikipedia.org/wiki/List_of_nuclear_test_sites"
# First, grab the page source
content <- html(theurl) %>%
  # then extract the first node with class of wikitable
  html_node(".wikitable") %>% 
  # then convert the HTML table into a data frame
  html_table()
# Populate country-column for all the sites
for (i in 1:dim(content)[1]){
  if (content[i,1]!="") {
    country <- content[i,1]
  }else{
    content[i,1] <- country
  }
}
# remove sites without coordinates
content <- content[content$`Approx. location` != "",]
# get vector with coordinates
coords <- content$`Approx. location`
# split the strings into a list using "/" as separator
x <- strsplit(coords, "/")
# Keep only the third element of each list
tempCoords <- c()
for (i in 1:length(x)){
  tempCoords <- c(tempCoords, x[[i]][3])
}
# Extract decimal numbers from remaining strings
lat <- c()
lon <- c()
for (i in 1:length(x)){
  str <- tempCoords[[i]]
  strList <- gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",str)
  lat <- c(lat, as.numeric(unlist(regmatches(str,strList)))[1])
  lon <- c(lon, as.numeric(unlist(regmatches(str,strList)))[2])
}
nuclearSites <- data.frame("Country"=content$`Testing country`,
                           "Location"=content[,2],
                           "Site"=content[,3],
                           "Lat"=lat,
                           "Lon"=lon,
                           "Description"=content$Notes)

# Build SpatialPointsDataFrame
# library(sp)
# nuclearSitesSP <- nuclearSites
# coordinates(nuclearSitesSP) = ~Lon + Lat
save(nuclearSites, file="kehraApp/data/nuclearSites.rda")
