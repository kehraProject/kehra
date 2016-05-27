#' Fill missing values
#'
#' @description Fill missing values
#'
#' @param id site id code
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

fillMissingValues <- function(id, df, maxgap = 12, parallel = FALSE){

  # library(dplyr)
  # library(xts)
  # library(zoo)
  # df <- t2m; maxgap = 12
  # site <- as.character(unique(df$SiteID))[1]

  # TODO: Implement parallel version
  # library(parallel)
  # Use all the available cores
  # nCores <- detectCores() - 1
  # newDF <- mclapply(...)

  # In case SiteID is a factor, convert it to characters
  # id <- as.character(unique(df$SiteID))

  if (length(as.list(id)) == 0) {

    message("Please, enter valid id.")
    stop

  }else{

    if (parallel == FALSE){

      # multiple identification numbers
      tsList <- lapply(X = as.list(id),
                       FUN = fillMissingValues_internal, df, maxgap)
      filledIn <- do.call(rbind.data.frame, tsList)

    }else{

      # this is the case of a single identification number
      filledIn <- fillMissingValues_internal(id, df, maxgap)

    }

  }

  names(filledIn) <- names(df)

  return(filledIn)

}


fillMissingValues_internal <- function(site, df, maxgap){

  print(site)

  siteROWS <- which(df$SiteID == site)
  siteTS <- df[siteROWS, ]

  datetime <- seq.POSIXt(as.POSIXlt(head(siteTS$datetime, n = 1)),
                         as.POSIXlt(tail(siteTS$datetime, n = 1)),
                         by = "hour")
  emptyTS <- xts(rep(NA, length(datetime)), order.by = datetime)

  siteDF <- xts(siteTS[, 3], order.by = as.POSIXlt(siteTS$datetime))
  siteDFextented <- merge(emptyTS, siteDF)[,2]

  x <- na.approx(object = siteDFextented, maxgap = maxgap, na.rm = FALSE)

  siteDF1 <- data.frame(as.character(index(x)),
                        rep(site, length(x)),
                        coredata(x), stringsAsFactors = FALSE)

  return(siteDF1)

}
