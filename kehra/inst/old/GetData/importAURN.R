importAURN <- function (site = "my1", year = 2009, pollutant = "all", hc = FALSE) 
{
  site <- toupper(site)
  files <- lapply(site, function(x) paste(x, "_", year, sep = ""))
  files <- do.call(c, files)
  loadData <- function(x) {
    tryCatch({
      fileName <- paste("http://uk-air.defra.gov.uk/openair/R_data/", 
                        x, ".RData", sep = "")
      con <- url(fileName, method = "libcurl")
      load(con)
      closeAllConnections(con)
      dat <- get(x)
      return(dat)
    }, error = function(ex) {
      cat(x, "does not exist - ignoring that one.\n")
    })
  }
  thedata <- plyr::ldply(files, loadData)
  if (nrow(thedata) == 0) 
    return()
  if (is.null(thedata)) 
    stop("No data to import - check site codes and year.", 
         call. = FALSE)
  thedata$site <- factor(thedata$site, levels = unique(thedata$site))
  names(thedata) <- tolower(names(thedata))
  id <- which(names(thedata) %in% "noxasno2")
  if (length(id) == 1) 
    names(thedata)[id] <- "nox"
  if (hc) {
    thedata <- thedata
  }
  else {
    theNames <- c("date", "co", "nox", "no2", "no", "o3", 
                  "so2", "pm10", "pm2.5", "v10", "v2.5", "nv10", "nv2.5", 
                  "ws", "wd", "code", "site")
    thedata <- thedata[, which(names(thedata) %in% theNames)]
  }
  if (pollutant != "all") 
    thedata <- thedata[, c("date", pollutant, "site", "code")]
  timeDiff <- difftime(Sys.time(), max(thedata$date), units = "days")
  if (timeDiff < 180) {
    warning("You have selected some data that is less than 6-months old.\n This most recent data is not yet ratified and may be changed\n during the QA/QC process. For complete information about the \nratification status of a data set, please use the online tool at:\n http://www.airquality.co.uk/data_and_statistics.php?action=da_1&go=Go")
  }
  attr(thedata$date, "tzone") <- "GMT"
  class(thedata$date) <- c("POSIXct", "POSIXt")
  thedata
}