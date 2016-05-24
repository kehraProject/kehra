#' Get data from ECMWF ERA_Interim
#'
#' @description Get data from ECMWF ERA_Interim
#'
#' @param db dataframe to populate with info from ERA-Interim
#' @param points are lat/lon coordinates of points (e.g. stations)
#' @param years years to retrieve data for
#' @param var variable to retrieve
#' @param timestep time step in hours (e.g. 1, 3, 6, 12, 24)
#' @param prefix string starting netcdf file name
#' @param path folder path where netcdf files are stored
#'
#' @details Possible variables names are: "t2m" (2m temperature, in K), "u10" (10 metres wind U component, in m/s), "v10" (10 metres wind V component, in m/s), "tp" (total precipitation, in m), "blh" (boundary layer height, in m), "ssr" (surface net solar radiation, in W/m2s).
#'
#' @return updated db
#'
#' @export
#'
#' @examples
#' # GetDataFromECMWF(db, points, years = 1981:2014, var = "t2m",
#' #                  timestep = 6, prefix = "", path = "~")
#'

GetDataFromECMWF <- function(db, points, years = 1981:2014,
                             var = "t2m", timestep = 6,
                             prefix = "", path = "~"){

  # library(Hmisc)
  # points <- stations
  # year <- 1991

  myVARdf <- data.frame(matrix(NA, ncol = 3, nrow = 0))
  names(myVARdf) <- c("datetime", "SiteID", var)

  pointsSP <- SpatialPoints(points[, c('Longitude', 'Latitude')],
                            proj4string=CRS('+proj=longlat +datum=WGS84'))

  for (year in years){

    print(year)

    # Set name of file containing UVT for given year
    fname <- paste(path, "/", prefix, year, ".nc", sep="")

    # Extract variable at the given points and set the length of dates vectors
    myVARgrid <- rotate(brick(x = fname, varname = var))
    tmp <- extract(myVARgrid, pointsSP)

    # Dates vector
    # library(stringr)
    startH <- str_pad(paste(timestep,":00", sep =""), 5, pad = "0")
    nDays <- yearDays(as.Date(paste(year, "-02-01", sep="")))
    lengthOut <- 24/timestep * nDays

    dates <- seq(as.POSIXlt(paste(year,"-01-01 ", startH, sep = ""),
                            format = "%Y-%m-%d %H:%M"),
                 as.POSIXlt(paste(year,"-12-31 24:00", sep = ""),
                            format = "%Y-%m-%d %H:%M"),
                 length.out = lengthOut)

    # Generate TS from variable
    # library(reshape2)
    namedTS1 <- as.data.frame(t(tmp))
    row.names(namedTS1) <- NULL
    names(namedTS1) <- points$SiteID
    namedTS1$datetime <- as.character(as.POSIXlt(dates,
                                                 format = "%Y-%m-%d %H:%M"))
    myVariable <- melt(data = namedTS1, measure.vars = points$SiteID)
    names(myVariable) <- c("datetime", "SiteID", var)
    myVariable$SiteID <- as.character(myVariable$SiteID)

    myVARdf <- rbind(myVARdf, myVariable)

  }

  return(myVARdf)

}
