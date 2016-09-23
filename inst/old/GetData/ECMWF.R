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

UK <- getData("GADM",country="GBR",level=1)

# Read netcdf (Wind u and v component and Temperature)
# fname <- "data/Climate/Grid0p75/climate1981.nc"
# ex.nc = nc_open(fname)
# print(ex.nc) # or the more concise: summary(ex.nc)
# These data consists of a 480(rows)x241(cols)x365(timesteps) array of 3 variables: 'u10', 'v10' and t2m. The coordinate variables are associated with 3 dimensions: longitude, latitude and time.
# To change longitude range from [0,360] to [-180,180]
# For example, plot first time step for temperature brick
# tp <- rotate(brick(x = fname, varname="tp"))
# plot(tp[[1]], ext=extent(-10, +4, 48, 65))
# plot(stationsEngland, add = TRUE)
# library(maps)
# map(add=TRUE, col="brown")

# DATA FOR 2013 ONLY ########################################################

# Temperature ##################################################################
t2m <- rotate(brick(x = "data/Climate/climate2013.nc", varname="t2m"))
# Prepare covariate for RINLA
saveRDS(as.array(t2m), "data/Climate/Temp_GRID.rds")
saveRDS(coordinates(t2m), "data/GEO/UK/Topography/England_grid.rds")

# Total precipitation (read netcdf) ############################################
# ex.nc = open.ncdf("data/Climate/TotalPrecipitation.nc"); print(ex.nc)
# To change longitude range from [0,360] to [-180,180]
tp <- rotate(brick(x = "data/Climate/climate2013.nc", varname="tp"))
# Prepare covariate for RINLA
saveRDS(as.array(tp), "data/Climate/Prec_GRID.rds")

# Boundary layers heights (read netcdf) ########################################
# ex.nc = open.ncdf("data/Climate/BoundaryLayers.nc"); print(ex.nc)
# To change longitude range from [0,360] to [-180,180]
blh <- rotate(brick(x = "data/Climate/climate2013.nc", varname="blh"))

# Wind U & V components ########################################################
# Wind U component
u10 <- rotate(brick(x = "data/Climate/climate2013.nc", varname="u10"))
# Wind V component
v10 <- rotate(brick(x = "data/Climate/climate2013.nc", varname="v10"))
# Calculate wind speed and direction
windSpeed <- function(u, v) {
  sqrt( u^2 + v^2) 
}
windDir <- function(u, v) {
  if(v > 0)         wd <- ((180 / pi) * atan(u/v) + 180)
  if(u < 0 & v < 0) wd <- ((180 / pi) * atan(u/v) + 0)
  if(u > 0 & v < 0) wd <- ((180 / pi) * atan(u/v) + 360)
  return(wd)
}

WS <- as.array(windSpeed(u10,v10))
# Prepare covariate for RINLA
saveRDS(WS, "data/Climate/WindSpeed_GRID.rds")

### ADD CLIMATE VARIABLES TO POLLUTION DATA ####################################

df <- readRDS("~/Dropbox/Projects/kehra/data/Pollution/allPollutants.rds")
# add placeholders to store climate information
df <- cbind(df, "TEMP"=NA,"WS"=NA, "WD"=NA, "HMIX"=NA, "PREC"=NA, "EMI"=NA)

# Loop through station IDs
ids <- as.character(unique((df$id)))

for (id in ids){
  
  myrows <- which(df$id == id)
  
  points <- SpatialPoints(df[myrows[1], c('Longitude', 'Latitude')], 
                          proj4string=CRS('+proj=longlat +datum=WGS84'))
  # Check whether crs(points) == crs(u10) == crs(v10) == crs(t2m)
  # plot(points, add=TRUE, col="red")
  
  df$TEMP[myrows] <- as.vector(extract(t2m, points))
  df$PREC[myrows] <- as.vector(extract(tp, points))
  df$HMIX[myrows] <- as.vector(extract(blh, points))
  u <- as.vector(extract(u10, points))
  v <- as.vector(extract(v10, points))
  df$WS[myrows] <- windSpeed(u,v)
  
  wd <- c()
  for (i in 1:length(myrows)) {
    wd[i] <- windDir(u[i], v[i])
  }
  
  df$WD[myrows] <- wd
  
}

# Could not find EMI values!
all(is.na(df$EMI))
# remove from dataset
df <- df[, 1:18]

saveRDS(df, "~/Dropbox/Projects/kehra/data/Pollution/allPollutantsClima.rds")
