# Shapefile of Local Autorithies according to old LA code (pre-2009) ###########

library(rgdal)
library(RColorBrewer)
library(sp)
library(GISTools)
library(maptools)

# Set working directory
setwd("~/Dropbox/Projects/kehra/data/")

# Download the following files:
# https://geoportal.statistics.gov.uk/Docs/Lookups/Output_areas_(2011)_to_local_authority_districts_(2008)_E+W_lookup.zip
# https://geoportal.statistics.gov.uk/Docs/Boundaries/Output_areas_(E+W)_2011_Boundaries_(Full_Extent)_V2.zip
# Using QGIS: join tables and dissolve them based on LA08CD. 
# Resulting shapefile is in: ~/Dropbox/Projects/kehra/data/GEO/UK/AdminBoundaries/UK_ONSboundaries/LA.shp

# Otherwise, using R, the code below is based on the following post:
# http://rstudio-pubs-static.s3.amazonaws.com/3189_672a7ee7051c481796da41d87ca06bb5.html

# 2008-2011 LA codes lookup table
temp <- tempfile(fileext = ".zip")
download.file("https://geoportal.statistics.gov.uk/Docs/Lookups/Output_areas_(2011)_to_local_authority_districts_(2008)_E+W_lookup.zip", temp)
unzip(temp)
oa_lookup <- read.csv("OA11_LAD08_EW_LU.csv")
unlink(temp)

# 2011 OA shapefile
temp <- tempfile(fileext = ".zip")
download.file("https://geoportal.statistics.gov.uk/Docs/Boundaries/Output_areas_(E+W)_2011_Boundaries_(Full_Extent)_V2.zip", temp)
unzip(temp)
OA <- readOGR(".", "OA_2011_EW_BFE_V2")
unlink(temp)

rm(temp)

# Join 2008 data onto 2011 OA
OA@data = data.frame(OA@data, 
                     oa_lookup[match(OA@data[, "OA11CD"], 
                                     oa_lookup[, "OA11CD"]), 2:3])
# saveRDS(OA, file = "GEO/UK/AdminBoundaries/UK_ONSboundaries/OA.rds")

# Dissolve polygons based on the LA2008 code
OA <- readRDS("GEO/UK/AdminBoundaries/UK_ONSboundaries/OA.rds")
OAdissolvedLAO08CD <- unionSpatialPolygons(OA, OA@data$LAD08CD)
# saveRDS(OAdissolvedLAO08CD, file = "GEO/UK/AdminBoundaries/UK_ONSboundaries/OAdissolvedLAO08CD.rds")
OAdissolvedLAO11CD <- unionSpatialPolygons(OA, OA@data$LAD11CD)
# saveRDS(OAdissolvedLAO11CD, file = "GEO/UK/AdminBoundaries/UK_ONSboundaries/OAdissolvedLAO11CD.rds")

### Coerce into spatial polygon data frame with id and row name of spatial polygon, make a data frame that meets the requirements above:
# Choose which dissolve method to use
OAdissolvedLA <- readRDS("GEO/UK/AdminBoundaries/UK_ONSboundaries/OAdissolvedLAO08CD.rds")
# or
# OAdissolvedLA <- readRDS("GEO/UK/AdminBoundaries/UK_ONSboundaries/OAdissolvedLAO11CD.rds")

df <- data.frame(id = getSpPPolygonsIDSlots(OAdissolvedLA))
row.names(df) <- getSpPPolygonsIDSlots(OAdissolvedLA)

# Make spatial polygon data frame
spdf <- SpatialPolygonsDataFrame(OAdissolvedLA, data =df)
# Join table content from OA
spdf@data = data.frame(spdf@data, 
                       OA@data[match(spdf@data[, "id"], 
                                     OA@data[, "LAD08CD"]), ])
# clean up
rownames(spdf@data) <- NULL

# Then don't forget to make sure the projection is correct
UK_adm2 <- spTransform(spdf , "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

saveRDS(UK_adm2, file = "GEO/UK/AdminBoundaries/UK_adm2.rds")
