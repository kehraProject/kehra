########## LOCAL ADMINISTRATIVE UNITS (LAU) - NUTS3 ############################
source("~/Dropbox/Projects/kehra/scripts/NUTStoLAU.R")

# Health data for LA
YearlyHAinUK_NUTS3 <- readRDS("Health/HospitalAdmissionsUK/Yearly/YearlyHAinUK_NUTS3.rds")

# LA file from ONS does not match!
# UK_adm2 <- readOGR(dsn = "GEO/UK/AdminBoundaries/LOA/LSOA_2011_EW_BFE_V2.shp",
#                    layer = "LSOA_2011_EW_BFE_V2") 

# Let's use the NUTS3!
UK_adm2 <- readRDS("GEO/UK/AdminBoundaries/UK_adm2.rds")

# Check whether there is a correspondent NUTS code for each LA code
for (myCODE in unique(YearlyHAinUK_NUTS3$LA_CODE)) {
  tmp <- NUTStoLAU(LAUcode = myCODE)
}
# The LA code "22UK" corresponds to two NUTS3 codes: UKH34 and UKH36
# Also, there is no correspondence in the NUTS codes for the following LAU codes:
missingLAUcodes <- c("20UB", "20UD", "20UE", "20UF", "20UG", "20UH", "20UJ", 
                     "35UB", "35UC", "35UD", "35UE", "35UF", "35UG", "13UB",
                     "13UC", "13UD", "13UE", "13UG", "13UH", "39UB", "39UC",
                     "39UD", "39UE", "39UF", "09UC", "09UD", "09UE", 
                     "15UB", "15UC", "15UD", "15UE", "15UF", "15UG", "15UH")

# LAU to NUTS3
tmpCODE <- as.character(UK_adm2@data$NUTS_ID)
tmpLAold <- tmpLAnew <- c()
for (x in tmpCODE) {
  tmpLAold <- c(tmpLAold,NUTStoLAU(NUTScode = x)$old[1])
  tmpLAnew <- c(tmpLAnew,NUTStoLAU(NUTScode = x)$new)
}

for (x in tmpCODE) {
  print(length(NUTStoLAU(NUTScode = x)$old))
}



rm(x,tmpLAold,tmpLAnew,tmpCODE)

# Source: http://ec.europa.eu/eurostat/web/nuts/local-administrative-units
# see also:
# http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/eurostat/index.html
# Correspondence table LAU 2 – NUTS 2013, EU-28 (2014)
# http://ec.europa.eu/eurostat/documents/345175/501971/EU-28_2014.xlsx
# Save the UK sheet only in csv format:
# /home/kz/Dropbox/Projects/kehra/data/Health/EU-28_2014_UKonly.csv
# Use the function below to find a correspondance between LAU and NUTS3 codes:
source('~/Dropbox/Projects/kehra/scripts/NUTStoLAU.R')
# Example use:
# NUTStoLAU(NUTScode=NULL, LAUcode = "00AC")
# NUTStoLAU(NUTScode=NULL, LAUcode = "E09000003")
# NUTStoLAU(NUTScode="UKI71", LAUcode = NULL)

UK_adm2 <- readRDS("~/Dropbox/Projects/kehra/data/GEO/UK/AdminBoundaries/GADM/GBR_adm2.rds") 
x <- UK_adm2@data
temp <- read.csv("/home/kz/Dropbox/Projects/kehra/data/Health/EU-28_2014_UKonly.csv")
temp$LAU1_NAT_CODE[1]
which(x$LSOA11CD %in% as.character(temp$LAU1_NAT_CODE_NEW))