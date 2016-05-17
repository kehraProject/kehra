#' Wind Direction
#'
#' @description Calculate wind direction in degrees from u & v components
#'
#' @param u first component of wind speed
#' @param v second component of wind speed
#'
#' @return direction in degrees from u & v components
#'
#' @export
#'
#' @examples
#' # windDir(u, v)
#'

windDir <- function(u, v) {

  if (is.na(v) | is.na(u)) {

    wd <- NA

  }else{

    if(v > 0)         wd <- ((180 / pi) * atan(u/v) + 180)
    if(u < 0 & v < 0) wd <- ((180 / pi) * atan(u/v) + 0)
    if(u > 0 & v < 0) wd <- ((180 / pi) * atan(u/v) + 360)

  }

  return(wd)

}
