library(RPostgreSQL) 
library(rgdal)
# library(foreign)  

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="kehra", host="localhost", user="kehrauser", password="c!occ0lAta")

# Test: dbListTables(con)

# Read tables in demographics schema (need only RPostgreSQL)
# pop <- dbReadTable(conn = con, name=c("demographics","PopulationEstimatesLA"))
# popR <-  dbReadTable(conn = con, name=c("demographics","PopulationEstimatesGOR"))

# Read tables in gis schema (need RPostgreSQL and rgdal for PostGIS)
dsn="PG:dbname='kehra'"
# ogrListLayers(dsn)
GBR_adm0 <- readOGR(dsn,"gis.GBR_adm0")
KAZ_adm0 <- readOGR(dsn,"gis.KAZ_adm0")

# q='select * from gis."GBR_adm0" union select * from gis."KAZ_adm0";'
# adm0 <- dbGetQuery(con,q)
# library(rgeos)
# readWKT(adm0$geom) ??? 


# adm1 <- readRDS("data/adm1.rds")
# adm2 <- readRDS("data/adm2.rds")

bbox <- data.frame("Name"=adm0@data$Code0,  
                   "latmin"=c(40.417, 48.5), 
                   "latmax"=c(55.428, 64.067), 
                   "longmin"=c(46.583, -13.683), 
                   "longmax"=c(90, 3.858),
                   "latmid"=c(50,55),
                   "longmid"=c(65,-5),
                   "zoomlevel"=c(5,6))

load("data/nuclearSites.rda")
populatedPlaces <- read.dbf("data/ne_10m_populated_places.dbf", as.is = FALSE)

listOfDatasets <- data.frame(Label=c("Vulnerability Index"),
                             Location=c("data/VulnerabilityIndex.rds"),
                             stringsAsFactors = FALSE)

# Close PostgreSQL connection 
# dbDisconnect(con)
