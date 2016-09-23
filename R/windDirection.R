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
#' # windDirection(u, v)
#'

windDirection <- function(u, v) {

  # Convert u and v to numeric
  # (as.character is necessary to avoid problems when converting from factors)
  uN <- as.numeric(as.character(u))
  vN <- as.numeric(as.character(v))

  if (is.na(vN) | is.na(uN)) {

    wd <- NA

  }else{

    if(vN > 0)         wd <- ((180 / pi) * atan(uN/vN) + 180)
    if(uN < 0 & vN < 0) wd <- ((180 / pi) * atan(uN/vN) + 0)
    if(uN > 0 & vN < 0) wd <- ((180 / pi) * atan(uN/vN) + 360)

  }

  return(wd)

}
