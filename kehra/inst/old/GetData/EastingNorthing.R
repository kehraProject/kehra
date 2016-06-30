#' Get Easting and Northing coordinates from DEFRA
#' 
#' @description This function takes as input the UK AIR ID and returns Easting and Northing coordinates (British National Grid, EPSG:27700).
#'
#' @param uka_id An alphanumeric string containing the UK AIR ID defined by DEFRA
#' 
#' @return A named vector containing Easting and Northing coordinates.
#' 
#' @examples
#' EastingNorthing("UKA12536")

EastingNorthing_internal <- function(uka_id){
  
  # For testing:
  # load packages
  # library(RCurl)
  # library(XML)
  # uka_id <- "UKA12536"
  
  rootURL <- "http://uk-air.defra.gov.uk/networks/site-info?uka_id="
  
  myURL <- paste(rootURL, uka_id, sep = "")
  
  # download html
  html <- getURL(myURL, followlocation = TRUE)
  
  # parse html
  doc = htmlParse(html, asText=TRUE)
  plain.text <- xpathSApply(doc, '//*[@id="tab_info"]/p[8]/text()', xmlValue)
  
  # split string into easting and northing and remove heading/trailing spaces
  en <- gsub("^\\s+|\\s+$", "", unlist(strsplit(plain.text, ",")))
  
  return(c("Easting" = as.numeric(en[1]), "Northing" = as.numeric(en[2])))
  
}

EastingNorthing <- function(IDs){
  
  # For testing:
  # load packages
  # library(RCurl)
  # library(XML)
  # IDs <- c("UKA12536", "UKA12536", "UKA12536")
  
  enDF <- do.call(rbind, lapply(X = as.list(IDs), FUN = EastingNorthing_internal))
  
  return(enDF)
  
}
