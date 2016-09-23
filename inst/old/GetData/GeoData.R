################################################################################
#################### DATA PREPARATION ##########################################
################################################################################

library(rgdal)
library(leaflet)
library(maptools)
library(data.table)
library(sp)         # classes for spatial data
library(raster)     # grids, rasters
library(rasterVis)  # raster visualisation
library(rgeos)
library(htmltools)
library(foreign)
library(gdata)      # handles xls file

setwd("~/Dropbox/Projects/kehra/data/")

# Bounding Boxes ###############################################################
bbox <- data.frame("Name"=c("KAZ", "GBR"),
                   "latmin"=c(40.417, 48.5),
                   "latmax"=c(55.428, 64.067),
                   "longmin"=c(46.583, -13.683),
                   "longmax"=c(90, 3.858),
                   "latmid"=c(50,55),
                   "longmid"=c(65,-5),
                   "zoomlevel"=c(5,6))

# dbWriteTable(con, c("demographics","bbox"), value=bbox, overwrite=TRUE)

# ADMINISTRATIVE BOUNDARIES #####################################################

# ALL Countries boundaries
# wget http://biogeo.ucdavis.edu/data/world/countries_shp.zip

world <- readShapePoints("countries_shp/countries.shp")

download.file(file.path('http://www.naturalearthdata.com/http/',
                        'www.naturalearthdata.com/download/50m/cultural',
                        'ne_50m_admin_0_countries.zip'),
              f <- tempfile())
unzip(f, exdir=tempdir())

world <- readOGR(tempdir(), 'ne_50m_admin_0_countries', encoding='UTF-8')
countries <- c("United Kingdom", "Kazakhstan")

# Test using leaflet
leaflet() %>% addTiles() %>%
  addPolygons(data=subset(world, name %in% countries), weight=2)

studyAreas <- subset(world, name %in% countries)
saveRDS(studyAreas, file = "data/studyAreas.rds")

# ADMIN LEVEL 0: Countries #####################################################
UK_adm0 <- readRDS("GEO/UK/AdminBoundaries/GADM/GBR_adm0.rds")
saveRDS(UK_adm0, file = "GEO/UK/AdminBoundaries/UK_adm0.rds")   # This is NUTS 0

# Cleanup
names(UK_adm0@data)
UK_adm0 <- UK_adm0[,c("ISO", "NAME_ISO")]
names(UK_adm0@data) <- c("Code","Name")
KZ_adm0 <- KZ_adm0[,c("ISO", "NAME_ISO")]
names(KZ_adm0@data) <- c("Code","Name")

# merge UK_adm0 and KZ_adm0
library(rgdal)
adm0 <- readRDS("~/Dropbox/Projects/kehra/kehraApp/data/adm0.rds")
writeOGR(obj = adm0, dsn = "/home/kz/Dropbox/Projects/kehra/kehraApp/data",
         layer = "adm0", driver="ESRI Shapefile")

# DEFINE NEW CORRESPONDENCE for lower admin levels #############################
# The admin boundaries obtained from GADM do not match the ONS data, we need to use the NUTS and LAO systems. Get NUTS shapefile from EUROSTAT, then in QGIS: vector -> dissolve on stat levels

# All ONS lookup tables are available here:
# http://www.ons.gov.uk/ons/guide-method/geography/products/census/lookup/index.html

UK_adm1 <- readShapePoly("GEO/UK/AdminBoundaries/NUTS_2013/NUTS_UK_1.shp")
saveRDS(UK_adm1, file = "GEO/UK/AdminBoundaries/UK_adm1.rds")   # This is NUTS 1

# ADMIN LEVEL 1: GOR vs NUTS1 ##################################################

UK_adm1 <- readRDS("GEO/UK/AdminBoundaries/UK_adm1.rds")

# NUTS1 names and codes
# Source: https://data.gov.uk/dataset/nuts-level-1-uk-jan-2012-names-and-codes#

# Based on this and the ONS reference tables,
# the following lookup table can be built:

NUTS1GOR <- data.frame("NUTS1_code" = c("UKC" ,"UKD", "UKE", "UKF", "UKG",
                                        "UKH", "UKI", "UKJ", "UKK", "UKL",
                                        "UKM", "UKN"),
                       "NUTS1_name" = c("North East (England)",
                                        "North West (England)",
                                        "Yorkshire and The Humber",
                                        "East Midlands (England)",
                                        "West Midlands (England)",
                                        "East of England",
                                        "London",
                                        "South East (England)",
                                        "South West (England)",
                                        "Wales",
                                        "Scotland",
                                        "Northern Ireland"),
                       "GOR_code" = c("A", "B", "D", "E", "F",
                                      "G", "H", "J", "K", NA, NA, NA),
                       "GOR_name" = c("North East",
                                      "North West",
                                      "Yorkshire and The Humber",
                                      "East Midlands",
                                      "West Midlands",
                                      "East of England",
                                      "London",
                                      "South East",
                                      "South West",
                                      NA, NA, NA))
# saveRDS(NUTS1GOR, file = "GEO/LookUpTables/NUTS1GOR.rds")

# Save the data row.names into an explicit variable
UK_adm1$rn <- row.names(UK_adm1)

# Create temporary data tables to work on the attributes
tmp <- data.table(UK_adm1@data)
NUTS1GOR <- data.table(NUTS1GOR)

# Say we want to merge data from table NUTS1GOR into tmp
setkey(tmp, NUTS1_code)
setkey(NUTS1GOR, NUTS1_code)
tmp <- NUTS1GOR[tmp]

# Then let's re-attach the table to the original SpatialPolygonsDataFrame
# (preserving the original order of the row.names)
setkey(tmp, rn)
UK_adm1@data <- tmp[row.names(UK_adm1)]

# Assign correct CRS
UK_adm1@proj4string@projargs <- UK_adm0@proj4string@projargs

# Export back to shapefile (or to any spatial format)
# writeOGR(UK_adm1, "./maps", "UK_adm1", driver="ESRI Shapefile")
# Or overwrite original RDS
saveRDS(UK_adm1, file = "GEO/UK/AdminBoundaries/UK_adm1.rds")

################################################################################
######################## MERGE DATA ############################################
################################################################################
# GADM database of Global Administrative Areas
# http://www.gadm.org/
# http://www.diva-gis.org/gdata

# KZ boundaries
KZ_adm0 <- readRDS("GEO/KZ/AdminBoundaries/KZ_adm0.rds") # Nation
KZ_adm1 <- readRDS("GEO/KZ/AdminBoundaries/KZ_adm1.rds") # Regions
KZ_adm2 <- readRDS("GEO/KZ/AdminBoundaries/KZ_adm2.rds") # Provinces

writeOGR()

plot(KZ_adm0)
plot(KZ_adm1, add=TRUE, col="red")
plot(KZ_adm2, add=TRUE, col="brown")

# UK boundaries
UK_adm0 <- readRDS("GEO/UK/AdminBoundaries/UK_adm0.rds") # GADM national level
UK_adm1 <- readRDS("GEO/UK/AdminBoundaries/UK_adm1.rds") # NUTS1 + GOR codes
writeOGR(UK_adm1, dsn="/home/kz/Dropbox/Projects/kehra/data/GEO/UK/AdminBoundaries/NUTS_2013/", layer = "GBR_adm_NUTS1GOR", driver="ESRI Shapefile")
UK_adm2 <- readRDS("GEO/UK/AdminBoundaries/UK_adm2.rds") # ONS LAO08CD codes
writeOGR(UK_adm2, dsn="/home/kz/Dropbox/Projects/kehra/data/GEO/UK/AdminBoundaries/UK_ONSboundaries/", layer = "GBR_adm_ONSLA", driver="ESRI Shapefile")

plot(UK_adm0)
plot(UK_adm1, add=TRUE, col="red")
plot(UK_adm2, add=TRUE, col="brown")

################################################################################
# Merge the vectors based on adm levels ########################################
################################################################################

source('../scripts/GetData/rbind.SpatialPolygonsDataFrame.R')

# LEVEL 0: read in #############################################################
UK_adm0 <- readRDS("GEO/UK/AdminBoundaries/UK_adm0.rds") # GADM national level
KZ_adm0 <- readRDS("GEO/KZ/AdminBoundaries/KZ_adm0.rds") # Nation

# Extract polygon ID's
p <- UK_adm0[,c("ISO", "NAME_ISO")]
pid <- sapply(slot(p, "polygons"), function(x) slot(x, "ID"))
# Create dataframe with correct rownames
p.df <- data.frame( p, row.names = pid)
# Try coersion again and check class
UK_adm0 <- SpatialPolygonsDataFrame(p, p.df)
# class(UK_adm0)

p <- KZ_adm0[,c("ISO", "NAME_ISO")]
pid <- sapply(slot(p, "polygons"), function(x) slot(x, "ID"))
# Create dataframe with correct rownames
p.df <- data.frame( p, row.names = pid)
# Try coersion again and check class
KZ_adm0 <- SpatialPolygonsDataFrame(p, p.df)
# class(KZ_adm0)

names(KZ_adm0@data) <- names(UK_adm0@data) <- c("Code0", "Name0")
adm0 <- rbind.SpatialPolygonsDataFrame(KZ_adm0, UK_adm0)
rownames(adm0@data) <- NULL
saveRDS(adm0,"~/Dropbox/Projects/kehra/kehraApp/data/adm0.rds")

# LEVEL 1: read in #############################################################
UK_adm1 <- readRDS("GEO/UK/AdminBoundaries/UK_adm1.rds")
KZ_adm1 <- readRDS("GEO/KZ/AdminBoundaries/KZ_adm1.rds")

# Extract polygon ID's
# UK
UK_adm1@data$Code0 <- "GBR"
UK_adm1@data$Name0 <- "UNITED KINGDOM"
p <- UK_adm1[,c("Code0", "Name0", "GOR_code", "GOR_name")]
pid <- sapply(slot(p, "polygons"), function(x) slot(x, "ID"))
# Create dataframe with correct rownames
p.df <- data.frame( p, row.names = pid)
# Try coersion again and check class
UK_adm1 <- SpatialPolygonsDataFrame(p, p.df)
# class(UK_adm1)
#KZ
KZ_adm1@data$Code0 <- "KAZ"
KZ_adm1@data$Name0 <- "KAZAKHSTAN"
p <- KZ_adm1[,c("Code0", "Name0", "ID_1", "NAME_1")]
pid <- sapply(slot(p, "polygons"), function(x) slot(x, "ID"))
# Create dataframe with correct rownames
p.df <- data.frame( p, row.names = pid)
# Try coersion again and check class
KZ_adm1 <- SpatialPolygonsDataFrame(p, p.df)
# class(KZ_adm1)

names(KZ_adm1@data) <- names(UK_adm1@data) <- c("Code0", "Name0",
                                                "Code1", "Name1")
adm1 <- rbind.SpatialPolygonsDataFrame(KZ_adm1, UK_adm1)
rownames(adm1@data) <- NULL
saveRDS(adm1,"~/Dropbox/Projects/kehra/kehraApp/data/adm1.rds")

# LEVEL 2: read in #############################################################
UK_adm2 <- readRDS("GEO/UK/AdminBoundaries/UK_adm2.rds")
KZ_adm2 <- readRDS("GEO/KZ/AdminBoundaries/KZ_adm2.rds")

# Extract polygon ID's
# UK
UK_adm2@data$Code0 <- "GBR"
UK_adm2@data$Name0 <- "UNITED KINGDOM"
UK_adm2@data$Code1 <- NA
UK_adm2@data$Name1 <- NA
UK_adm2@data$Name3 <- NA
p <- UK_adm2[,c("Code0", "Name0",
                "Code1", "Name1", "LAD08CD", "LAD08NM", "LAD11CD", "Name3")]
pid <- sapply(slot(p, "polygons"), function(x) slot(x, "ID"))
# Create dataframe with correct rownames
p.df <- data.frame( p, row.names = pid)
# Try coersion again and check class
UK_adm2 <- SpatialPolygonsDataFrame(p, p.df)
# class(UK_adm2)
#KZ
KZ_adm2@data$Code0 <- "KAZ"
KZ_adm2@data$Name0 <- "KAZAKHSTAN"
KZ_adm2@data$Code3 <- NA
KZ_adm2@data$Name3 <- NA
p <- KZ_adm2[,c("Code0", "Name0",
                "ID_1", "NAME_1", "ID_2", "NAME_2", "Code3", "Name3")]
pid <- sapply(slot(p, "polygons"), function(x) slot(x, "ID"))
# Create dataframe with correct rownames
p.df <- data.frame( p, row.names = pid)
# Try coersion again and check class
KZ_adm2 <- SpatialPolygonsDataFrame(p, p.df)
# class(KZ_adm2)

names(KZ_adm2@data) <- names(UK_adm2@data) <- c("Code0", "Name0",
                                                "Code1", "Name1",
                                                "Code2", "Name2",
                                                "Code3", "Name3")
adm2 <- rbind.SpatialPolygonsDataFrame(KZ_adm2, UK_adm2)
rownames(adm2@data) <- NULL
saveRDS(adm2,"~/Dropbox/Projects/kehra/kehraApp/data/adm2.rds")
