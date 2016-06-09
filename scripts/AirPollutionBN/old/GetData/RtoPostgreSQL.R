# Establish connection to database using RPostgreSQL
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv,
                 dbname="kehra",
                 host="172.31.70.1",
                 port=5432,
                 user="kehrauser",password="c!occ0lAta")

# Read table in R
pop <- readRDS("/home/kz/Dropbox/Projects/kehra/data/SocioEconomic/PopulationEstimatesLA.rds")
names(pop)[1] <- "GORname"
names(pop)[4] <- "LAD11CD"
# Write table in postgresql
dbWriteTable(con, "PopulationEstimatesLA", value=pop, overwrite=TRUE)

popR <- readRDS("/home/kz/Dropbox/Projects/kehra/data/SocioEconomic/PopulationEstimatesRegion.rds")
names(popR)[3] <- "GORname"
dbWriteTable(con, "PopulationEstimatesGOR", value=popR, overwrite=TRUE)

library(rgdal)
table1 <- dbReadTable(conn = con, name=c("gis","GBR_adm0"))
table1 <- ogrInfo("PG:dbname='kehra'", layer="GBR_adm0")

# Close PostgreSQL connection
dbDisconnect(con)
