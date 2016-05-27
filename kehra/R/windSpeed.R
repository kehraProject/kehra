#' Wind Speed
#'
#' @description Calculate wind speed in m/s from u & v components
#'
#' @param u first component of wind speed
#' @param v second component of wind speed
#'
#' @return Speed in m/s
#'
#' @export
#'
#' @examples
#' # windSpeed(u, v)
#'

windSpeed <- function(u, v) {

  uN <- as.numeric(as.character(u))
  vN <- as.numeric(as.character(v))

  sqrt(u^2 + v^2)

}
