importMeta <- function (source = "aurn", all = FALSE) {
  site = code = latitude = longitude = site.type = site_name = site_id = NULL
  location_type = SiteCode = SiteName = Classification = Latitude = Longitude = NULL
  meta.source <- c("aurn", "kcl", "saqn")
  source <- tolower(source)
  if (!source %in% meta.source) 
    stop("Meta data sources are 'aurn', 'kcl' and 'saqn.")
  if (source == "aurn") {
    con <- url("http://uk-air.defra.gov.uk/openair/R_data/AURN_metadata.RData")
    meta <- get(load(con))
    closeAllConnections() ########## CV: CHANGED THIS LINE! it was: close(con)
    ids <- which(!duplicated(meta$site_id))
    meta <- meta[ids, ]
    meta <- rename(meta, code = site_id, site = site_name, 
                   site.type = location_type)
  }
  if (source == "saqn") {
    con <- url("http://www.scottishairquality.co.uk/openair/R_data/SCOT_metadata.RData")
    meta <- get(load(con))
    closeAllConnections() ########## CV: CHANGED THIS LINE! it was: close(con)
    ids <- which(!duplicated(meta$site_id))
    meta <- meta[ids, ]
    meta <- rename(meta, code = site_id, site = site_name, 
                   site.type = location_type)
  }
  if (source == "kcl") {
    con <- url("http://www.londonair.org.uk/r_data/sites.RData")
    meta <- get(load(con))
    closeAllConnections() ########## CV: CHANGED THIS LINE! it was: close(con)
    meta <- rename(meta, code = SiteCode, site = SiteName, 
                   site.type = Classification, latitude = Latitude, 
                   longitude = Longitude)
  }
  if (!all) 
    meta <- subset(meta, select = c(site, code, latitude, 
                                    longitude, site.type))
  meta
}