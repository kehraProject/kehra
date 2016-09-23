#' Get data from ECMWF ERA_Interim
#'
#' @description Get data from ECMWF ERA_Interim
#'
#' @param years years to retrieve data for
#' @param points are lat/lon coordinates of points (e.g. stations)
#' @param var variable to retrieve
#' @param prefix string starting netcdf file name
#' @param path folder path where netcdf files are stored
#' @param parallel Bolean, if TRUE parallel jobs are allowed
#'
#' @details Possible variables names are: "t2m" (2m temperature, in K), "u10" (10 metres wind U component, in m/s), "v10" (10 metres wind V component, in m/s), "tp" (total precipitation, in m), "blh" (boundary layer height, in m), "ssr" (surface net solar radiation, in W/m2s).
#'
#' @return time series variable
#'
#' @export
#'
#' @examples
#' # pointInspection(years = 1981:2014, points, var = "t2m")
#'

pointInspection <- function(years, points, var, 
                            prefix = "", path = "~", parallel = FALSE){

  # TEST
  # library(Hmisc)
  # library(sp)
  # library(raster)
  # library(ncdf4)
  # prefix = "UVT"
  # path = "~/Documents/"
  # points <- readRDS("~/Documents/stationsKEHRA.rds")
  # year <- 1991

  pointsSP <- SpatialPoints(points[, c('Longitude', 'Latitude')],
                            proj4string=CRS('+proj=longlat +datum=WGS84'))
  
  if (length(as.list(years)) == 0) {
    
    message("Please, enter valid years.")
    stop
    
  }else{
    
    if (parallel == FALSE){
      
      # multiple years
      tsList <- lapply(X = as.list(years),
                       FUN = pointInspection_internal, 
                       points, var, prefix, path, pointsSP)
      df <- do.call(rbind.data.frame, tsList)
      
    }else{
      
      # this is the case of a single year
      df <- pointInspection_internal(years, points, var, prefix, path, pointsSP)
      
    }
    
  }
  
  return(df)
  
}


pointInspection_internal <- function(year, points, var, prefix, path, pointsSP){
  
  # library(stringr)
  # library(reshape2)
  
  print(year)
  
  # Set name of file containing UVT for given year
  fname <- paste(path, "/", prefix, year, ".nc", sep="")
  
  # Extract variable at the given points and set the length of dates vectors
  myVARgrid <- rotate(brick(x = fname, varname = var))
  tmp <- extract(myVARgrid, pointsSP)
  
  # Dates vector
  nDays <- yearDays(as.Date(paste(year, "-01-01", sep="")))
  timestep <- 24/(dim(tmp)[2]/nDays)
  startH <- str_pad(paste(timestep,":00", sep =""), 5, pad = "0")
  lengthOut <- 24/timestep * nDays
  
  dates <- seq(as.POSIXlt(paste(year,"-01-01 ", startH, sep = ""),
                          format = "%Y-%m-%d %H:%M"),
               as.POSIXlt(paste(year,"-12-31 24:00", sep = ""),
                          format = "%Y-%m-%d %H:%M"),
               length.out = lengthOut)
  
  # Generate TS from variable
  namedTS1 <- as.data.frame(t(tmp))
  row.names(namedTS1) <- NULL
  names(namedTS1) <- points$SiteID
  namedTS1$datetime <- as.character(as.POSIXlt(dates,
                                               format = "%Y-%m-%d %H:%M"))
  myVariable <- melt(data = namedTS1, measure.vars = points$SiteID)
  names(myVariable) <- c("datetime", "SiteID", var)
  myVariable$SiteID <- as.character(myVariable$SiteID)
  
  return(myVariable)
  
}
