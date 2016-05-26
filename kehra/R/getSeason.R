#' Get season a date belongs to
#'
#' @description Get season a date belongs to. This function was taken from the following stackoverflow post: http://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to.
#'
#' @param DATES a date.
#'
#' @return returns the name of the season (e.g. "Fall")
#'
#' @export
#'
#' @examples
#' # my.dates <- as.Date("2011-12-01", format = "%Y-%m-%d") + 0:60
#' # getSeason(my.dates)
#'

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))

  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
