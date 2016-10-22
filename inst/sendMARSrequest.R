#' Send MARS request
#'
#' @description Send data request to the MARS web service
#'
#' @param years years to retrieve data for
#' @param var variables to retrieve (see details)
#'
#' @details The MARS web service is available through the ecmwfapi (pre-installed). By default the following variables are retrieved: "t2m" (2m temperature, in K), "u10" (10 metres wind U component, in m/s), "v10" (10 metres wind V component, in m/s), "tp" (total precipitation, in m), "blh" (boundary layer height, in m), "ssr" (surface net solar radiation, in W/m2s).
#'
#' @return NetCDF file containing all the variables listed in \code{var}.
#'
#' @export
#'
#' @examples
#' # sendMARSrequest(years = 1981:2014, points, var = "t2m")
#'

sendMARSrequest <- function(){

}
