#' Fill missing values
#'
#' @description Fill missing values
#'
#' @param ids site identification codes
#' @param df dataframe containing the timeseries in columns separated by ID (Header must follow this convention: column 1 = "datetime", column 2 = "SiteID", column 3 = "variable name"). df can be the result of GetDataFromECMWF().
#' @param maxgap maximum gap to interpolate (e.g. 6 hours)
#' @param parallel Bolean, if TRUE parallel jobs are allowed.
#'
#' @return updated df with infilled values
#'
#' @export
#'
#' @examples
#' # fillMissingValues(clima)
#'

fillMissingValues <- function(ids, df, maxgap = 12, parallel = FALSE, 
                              formatDT = "%d-%m-%Y %H:%M"){
  
  # library(dplyr)
  # library(xts)
  # library(zoo)
  
  # In case SiteID is a factor, convert it to characters
  ids <- as.character(ids)
  
  if (length(as.list(ids)) == 0) {
    
    message("Please, enter valid id.")
    stop
    
  }else{
    
    if (parallel == FALSE){
      
      # multiple identification numbers
      tsList <- lapply(X = as.list(ids),
                       FUN = fillMissingValues_internal, df, maxgap)
      filledIn <- do.call(rbind.data.frame, tsList)
      
    }else{
      
      # this is the case of a single identification number
      filledIn <- fillMissingValues_internal(ids, df, maxgap)
      # summary(filledIn); summary(siteTS)
      # head(filledIn); head(siteTS)
      
    }
    
  }
  
  names(filledIn) <- names(df)
  
  return(filledIn)
  
}


fillMissingValues_internal <- function(site, df, maxgap){
  
  print(site)
  
  siteROWS <- which(df$SiteID == site)
  siteTS <- df[siteROWS, ]
  
  datetime <- seq.POSIXt(as.POSIXlt(head(siteTS$datetime, n = 1),
                                    format = formatDT),
                         as.POSIXlt(tail(siteTS$datetime, n = 1),
                                    format = formatDT),
                         by = "hour")
  emptyTS <- xts(rep(NA, length(datetime)), order.by = datetime)
  
  siteDF <- xts(siteTS[, 3],
                order.by = as.POSIXlt(siteTS$datetime,
                                      format = formatDT))
  siteDFextented <- merge(emptyTS, siteDF)[,2]
  
  x <- na.approx(object = siteDFextented, maxgap = maxgap, na.rm = FALSE)
  
  if (all(is.na(x))) {
    coreX <- rep(NA, length(x))
  }else{
    coreX <- coredata(x)
  }
  
  siteDF1 <- data.frame(as.character(index(x)),
                        rep(site, length(x)),
                        coreX, stringsAsFactors = FALSE)
  
  return(siteDF1)
  
}
