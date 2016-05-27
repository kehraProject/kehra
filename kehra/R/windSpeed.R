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

  # Convert u and v to numeric
  # (as.character is necessary to avoid problems when converting from factors)
  uN <- as.numeric(as.character(u))
  vN <- as.numeric(as.character(v))

  ws <- sqrt(uN^2 + vN^2)

  return(ws)

}
