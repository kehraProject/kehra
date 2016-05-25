#' Fill missing values
#'
#' @description Fill missing values
#'
#' @param df dataframe containing the timeseries in columns separated by ID
#' 
#' @return updated df with infilled values
#'
#' @export
#'
#' @examples
#' # FillMissingValues(clima)
#'

FillMissingValues <- function(df){
  
  # Load the training set.
  # df <- readRDS("~/Documents/training.rds")
  # library(dplyr)
  # library(xts)
  # library(zoo)
  
  # For each site create a time series
  for (site in as.character(unique(df$SiteID))[1:3]){
    
    print(site)
    
    siteROWS <- which(df$SiteID == site)
    dfg <- df[siteROWS, ]
    
    datetime <- as.POSIXct(paste(paste(dfg$Year, dfg$Month, dfg$Day, 
                                       sep = "-"), 
                                 paste(dfg$Hour, ":00", sep="")), 
                           format = "%Y-%m-%d %H:%M")
    
    for (column in 13:26) {
      
      siteDF <- xts(dfg[,column], order.by = datetime)
      
      if (column %in% c(13, 14, 15, 19, 20)) {
        x <- na.approx(siteDF, maxgap = 6, na.rm = FALSE)
      }
      
      if (column %in% c(16:18, 21:26)) {
        x <- na.approx(siteDF, maxgap = 12, na.rm = FALSE)
      }
      
      df[siteROWS, column] <- coredata(x)
      
    }
    
  }
  
  return(df)
  
}

