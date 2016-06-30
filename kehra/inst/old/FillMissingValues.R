#' Fill missing values
#'
#' @description Fill missing values
#'
#' @param df dataframe containing the timeseries in columns separated by ID (Header must follow this convention: column 1 = "datetime", column 2 = "SiteID", column 3 = "variable name"). df can be the result of GetDataFromECMWF().
#' @param maxgap maximum gap to interpolate (e.g. 6 hours)
#'
#' @return updated df with infilled values
#'
#' @export
#'
#' @examples
#' # FillMissingValues(clima, maxgap = 6)
#'

FillMissingValues <- function(df, maxgap){
  
  # library(dplyr)
  # library(xts)
  # library(zoo)
  # df <- clima
  # site <- as.character(unique(df$SiteID))[1]
  
  newDF <- data.frame(matrix(NA, nrow = 0, ncol= 3))
  
  # For each site create a time series
  for (site in as.character(unique(df$SiteID))){
    
    print(site)
    
    siteROWS <- which(df$SiteID == site)
    dfg <- df[siteROWS, ]
    
    datetime <- seq.POSIXt(as.POSIXlt(head(dfg$datetime, n = 1)),
                           as.POSIXlt(tail(dfg$datetime, n = 1)),
                           by = "hour")
    emptyTS <- xts(rep(NA, length(datetime)), order.by = datetime)
    
    siteDF <- xts(dfg[, 3], order.by = as.POSIXlt(dfg$datetime))
    siteDFextented <- merge(emptyTS, siteDF)[,2]
    
    x <- na.approx(object = siteDFextented, maxgap = maxgap, na.rm = FALSE)
    
    newDF <- rbind(newDF, data.frame(cbind(index(x), 
                                           rep(site, length(x)),
                                           coredata(x))))
    
  }
  
  names(newDF) <- names(df)
  
  return(newDF)
  
}

