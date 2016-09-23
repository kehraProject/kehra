################################################################################
######################## HEALTH DATA ###########################################
################################################################################

# Clean the current environment
rm(list = ls())

library(gdata) # handles xls file

################################################################################
######################## MORTALITY #############################################
################################################################################

####################### ALL COUNTRIES (5-year averages) ########################

library(rworldmap)

Deaths <- read.xls("data/GEO/AllCountries/WPP2015_MORT_F03_1_DEATHS_BOTH_SEXES.XLS", 
                   sheet = "ESTIMATES", header = TRUE, skip=10,
                   stringsAsFactors=FALSE)[c(15:34,36:44,46:52,54:58,60:75,
                                             78:84,88:92,94:102,104:114,116:133,
                                             136:145,148:157,159:170,172:178,
                                             181:197,199:206,208:220,222,223,
                                             226,227,229:233,235:237,239:241),
                                           -c(1,2,4)]
rownames(Deaths) <- NULL
names(Deaths)[1] <- "Country"

# Deaths$Region <- NA
# Deaths$Subregion <- NA# 
# Deaths$Region[1:57] <- "Africa"
# Deaths$Region[58:107] <- "Asia"
# Deaths$Region[108:146] <- "Europe"
# Deaths$Region[147:184] <- "LatinAmericaCaribbean"
# Deaths$Region[185:186] <- "NorthAmerica"
# Deaths$Region[187:199] <- "NorthAmerica"
# Deaths$Country[Deaths$Country.code==638] <- "Reunion"
# Deaths$Country[Deaths$Country.code==583] <- "Micronesia (Federated States of)"

sPDF <- joinCountryData2Map(Deaths, joinCode = "UN", 
                            nameCountryColumn = "Country",
                            nameJoinColumn = "Country.code", verbose = TRUE)
# ignore the 6 failed codes

library(googleVis)

plot(gvisGeoChart(as.data.frame(sPDF), locationvar = "NAME",
                  colorvar = "X1950.1955",
                  options = list(gvis.editor = "Edit me!", height = 350)))

########## WEEKLY DEATHS BY REGION OF USUAL RESIDENCE (UK) #####################

# available data from week ending on the 05/08/2011 to the 23/11/2015 (so far)
# Get the week ending dates (Fri) and week numbers
# install.packages("lubridate")

library(lubridate)
x <- seq(from = as.Date("2011-08-05"), to = Sys.Date()-28, by = "1 day")
xx <- data.frame(x = x, weekday = weekdays(x), 
                 next_friday = ceiling_date(x, "week") +
                   ifelse(weekdays(x) %in% c("Saturday", "Sunday"), 5, -2))
weekEndingDate <- unique(xx$next_friday)
weekEndingDatecorrected <- format(weekEndingDate, "%d-%m-%Y")

# Use ISO convention
source('scripts/GetData/ISOweek.R')
weeknum1 <- ISOweek(weekEndingDate)$weeknum
library(stringr)
weeknum0 <- str_pad(weeknum1, 2, pad = "0") # add 0 to single digit

weekYear <- ISOweek(weekEndingDate)$year

require(RCurl)
baseURL <- "http://www.ons.gov.uk/ons/rel/vsob2/weekly-provisional-figures-on-deaths-registered-in-england-and-wales/"

selectedfilePaths <- missingfilePaths <- correctfilenames <- c()
for (i in 1:length(weekEndingDatecorrected)){
  
  correctfilenames <- c(correctfilenames, paste("weekly-deaths---week-", 
                                                weeknum0[i], "-", weekYear[i], 
                                                ".xls", sep=""))
  folderFile <- paste("week-ending-", weekEndingDatecorrected[i], "/",
                      correctfilenames[i], sep="")
  fullURL <- paste(baseURL, folderFile, sep="") 
  
  if ( url.exists(fullURL) ){
    selectedfilePaths <- c(selectedfilePaths,fullURL)
  }else{
    selectedfilePaths <- c(selectedfilePaths,NA)
    missingfilePaths <- c(missingfilePaths,fullURL)
    message(paste(folderFile, "does not exists!"))
  }
  
}

# This returns the following missing paths (13):
# week-ending-17-02-2012/weekly-deaths---week-07-2012.xls
# week-ending-24-02-2012/weekly-deaths---week-08-2012.xls
# week-ending-02-03-2012/weekly-deaths---week-09-2012.xls
# week-ending-07-09-2012/weekly-deaths---week-36-2012.xls
# week-ending-26-10-2012/weekly-deaths---week-43-2012.xls
# week-ending-14-12-2012/weekly-deaths---week-50-2012.xls
# week-ending-04-01-2013/weekly-deaths---week-01-2013.xls
# week-ending-11-01-2013/weekly-deaths---week-02-2013.xls
# week-ending-08-02-2013/weekly-deaths---week-06-2013.xls
# week-ending-15-03-2013/weekly-deaths---week-11-2013.xls
# week-ending-17-05-2013/weekly-deaths---week-20-2013.xls
# week-ending-24-05-2013/weekly-deaths---week-21-2013.xls
# week-ending-18-07-2014/weekly-deaths---week-29-2014.xls

# Fix the above manually
# Attempt n.1 finds 5 of missing datasets (use 1 digit for week 1-9)
missingfilePaths <- c()
for (i in which(is.na(selectedfilePaths))){
  folderFile <- paste("week-ending-", 
                      weekEndingDatecorrected[i], "/weekly-deaths---week-", 
                      weeknum1[i], "-", weekYear[i], ".xls", sep="")
  fullURL <- paste(baseURL, folderFile, sep="") 
  if ( url.exists(fullURL) ){
    selectedfilePaths[i] <- fullURL
    message(paste(folderFile, "was found!"))
  }else{
    missingfilePaths <- c(missingfilePaths,fullURL)
    message(paste(folderFile, "does not exists!"))
  }
}

# Attempt n.2 finds 2 of missing datasets (weeknum different convention, 2 dig.)
weeknum2 <- as.numeric(strftime(as.POSIXlt(weekEndingDate),format="%W"))
weeknum2 <- str_pad(weeknum2, 2, pad = "0")
missingfilePaths <- c()
for (i in which(is.na(selectedfilePaths))){
  folderFile <- paste("week-ending-", 
                      weekEndingDatecorrected[i], "/weekly-deaths---week-", 
                      weeknum2[i], "-", weekYear[i], ".xls", sep="")
  fullURL <- paste(baseURL, folderFile, sep="")
  if ( url.exists(fullURL) ){
    selectedfilePaths[i] <- fullURL
    message(paste(folderFile, "was found!"))
  }else{
    missingfilePaths <- c(missingfilePaths,fullURL)
    message(paste(folderFile, "does not exists!"))
  }
}

# Attempt n.3 finds 3 of missing datasets (2 digits, weeknum - 1)
weeknum3 <- as.numeric(weeknum0) - 1
weeknum3 <- str_pad(weeknum3, 2, pad = "0")
missingfilePaths <- c()
for (i in which(is.na(selectedfilePaths))){
  folderFile <- paste("week-ending-", 
                      weekEndingDatecorrected[i], "/weekly-deaths---week-", 
                      weeknum3[i], "-", weekYear[i], ".xls", sep="")
  fullURL <- paste(baseURL, folderFile, sep="")
  if ( url.exists(fullURL) ){
    selectedfilePaths[i] <- fullURL
    message(paste(folderFile, "was found!"))
  }else{
    missingfilePaths <- c(missingfilePaths,fullURL)
    message(paste(folderFile, "does not exists!"))
  }
}

# Attempt n.4 finds 3 of the missing datasets
weeknum4 <- as.numeric(strftime(as.POSIXlt(weekEndingDate),format="%W")) + 1
weekYear4 <- as.numeric(strftime(as.POSIXlt(weekEndingDate),format="%Y")) - 1
missingfilePaths <- c()
for (i in which(is.na(selectedfilePaths))){
  folderFile <- paste("week-ending-", 
                      weekEndingDatecorrected[i], "/weekly-deaths---week-", 
                      weeknum4[i], "-", weekYear4[i], ".xls", sep="")
  fullURL <- paste(baseURL, folderFile, sep="")
  if ( url.exists(fullURL) ){
    selectedfilePaths[i] <- fullURL
    message(paste(folderFile, "was found!"))
  }else{
    missingfilePaths <- c(missingfilePaths,fullURL)
    message(paste(folderFile, "does not exists!"))
  }
}

setwd("data/Health/DeathsUK")

# download files
mapply(download.file, selectedfilePaths, correctfilenames, method="wget")

# Build dataset
library(zoo)
x <- seq(from = as.Date("2011-06-13"), to = Sys.Date()-28, by = "1 day")
xx <- data.frame(x = x, weekday = weekdays(x), 
                 next_friday = ceiling_date(x, "week") +
                   ifelse(weekdays(x) %in% c("Saturday", "Sunday"), 5, -2))
DateALL <- format(unique(xx$next_friday), "%d-%m-%Y")
DateALL <- as.Date(DateALL, "%d-%m-%Y")

myList <- list("North East" = zoo(NA, order.by = DateALL), 
               "North West" = zoo(NA, order.by = DateALL), 
               "Yorkshire and The Humber" = zoo(NA, order.by = DateALL), 
               "East Midlands" = zoo(NA, order.by = DateALL), 
               "West Midlands" = zoo(NA, order.by = DateALL), 
               "East" = zoo(NA, order.by = DateALL), 
               "London" = zoo(NA, order.by = DateALL), 
               "South East" = zoo(NA, order.by = DateALL),
               "South West" = zoo(NA, order.by = DateALL), 
               "Wales" = zoo(NA, order.by = DateALL))

# Get the first 8 weeks from the first file
for (i in 1:length(myList)){
  
  tmpvalues <- as.numeric(read.xlsx(correctfilenames[1], 
                         sheetName = paste("Figures for week", weeknum0[1]), 
                         startRow=42 + i, colIndex=2:9, endRow=42 + i, 
                         as.data.frame=T, header=F, colClasses="numeric"))
  coredata(myList[[i]])[1:8] <- tmpvalues
  
}

counterDate <- 8
# From the remaining files extract only the last week of data
for (j in 2:length(correctfilenames)){
  
  print(correctfilenames[j])
  
  counterDate <- counterDate + 1
  
  if (weekYear[j] <= 2013 | weekYear[j] >= 2015) {
    startRow <- 43
  }else{
    if (as.numeric(weeknum0[j]) >= 10 & weekYear[j] == 2014) {
      startRow <- 43
    }else{
      startRow <- 45
      }
  }
  
  if (class(try(read.xlsx(correctfilenames[j], 
                          sheetName = paste("Figures for week", weeknum0[j]), 
                          startRow=startRow, colIndex=1:9, endRow=startRow + 9, 
                          as.data.frame=T, header=F, colClasses="character"), 
                silent =T)) != "try-error"){
    
    tmpTAB <- read.xlsx(correctfilenames[j],
                        sheetName = paste("Figures for week", weeknum0[j]), 
                        startRow=startRow, colIndex=1:9, endRow=startRow + 9, 
                        as.data.frame=T, header=F, colClasses="character")
    
  }else{
    
    tmpTAB <- read.xlsx(correctfilenames[j],
                        sheetName = paste("Figures for week", 
                                          as.numeric(weeknum0[j])), 
                        startRow=startRow, colIndex=1:9, endRow=startRow + 9, 
                        as.data.frame=T, header=F, colClasses="character")
    
  }
  
  for (ii in 1:length(myList)){
    
    region <- as.character(tmpTAB[ii,1])
    
    if (region != names(myList)[ii]) print("region != names(myList)[ii]")
    
    coredata(myList[[ii]])[counterDate] <- as.numeric(tmpTAB[ii,9])
    
  }
  
}

library(ggplot2)
df <- cbind("Date" = index(myList[[1]]), as.data.frame(myList))
df <- melt(data = df, id.vars="Date", value.name="Deaths", variable.name="Region")
ggplot(data=df, aes(x=Date, y=Deaths, group = Region, colour = Region)) +
  geom_line() + xlab("") # + geom_point( size=4, shape=21, fill="white")

saveRDS(object = df, file = "data/Health/WeeklyDeaths.rds")

########## DAILY DEATHS BY REGION ##############################################
# SOURCE:
# http://www.ons.gov.uk/ons/about-ons/business-transparency/freedom-of-information/what-can-i-request/published-ad-hoc-data/health/may-2015/deaths-by-region--england-and-wales-1998-to-2013.xls

library(gdata)
xM <- read.xls("data/Health/DeathsUK/Daily/deathsbyregionenglandwales19982013final_tcm77-420655.xls", sheet = "Region - Males")

xF <- read.xls("data/Health/DeathsUK/Daily/deathsbyregionenglandwales19982013final_tcm77-420655.xls", sheet = "Region - Females")

sumM <- apply(xM[,6:24], 1, sum)
sumF <- apply(xF[,6:24], 1, sum)

dimX <- dim(xM)[1]
startDate <- as.Date(paste(xM[1,1], "-", xM[1,2], "-", xM[1,3], sep=""), 
                     format = "%Y-%m-%d")
endDate <- as.Date(paste(xM[dimX,1], "-", xM[dimX,2], "-", xM[dimX,3], sep=""), 
                   format = "%Y-%m-%d")

xDates <- seq.Date(from = startDate, to = endDate, by="1 day")

x <- data.frame(matrix(NA, nrow = length(xDates), ncol = 11))
x[,1] <- xDates

for (i in 1:10){
  
  ii <- seq(i, dimX, 10)
  x[,1+i] <- (sumM + sumF)[ii]
  
}

names(x) <- c("Date", "North East", "North West", "Yorkshire and The Humber",
              "East Midlands", "West Midlands", "East",
              "London", "South East", "South West", "Wales")

library(ggplot2)
library(reshape2)
df <- melt(data = x, id.vars="Date", value.name="Deaths", variable.name="Region")
ggplot(data=df, aes(x=Date, y=Deaths, group = Region, colour = Region)) +
  geom_line() + xlab("") # + geom_point( size=4, shape=21, fill="white")

saveRDS(object = df, file = "data/Health/DailyDeaths.rds")

########### DAILY MORTALITY RATES ##############################################
library(reshape2)

x <- readRDS("data/Health/DailyDeaths.rds")
y <- readRDS("data/SocioEconomic/PopulationEstimatesRegions1971_2014.rds")[,c(1,2,30:45)]

temp <- dcast(x, Date ~ Region, value.var = "Deaths")[,-11]
temp$Y <- format(as.Date(temp$Date), "%Y")

mr <- temp 

for (myRegion in 1:9){
  
  for (myYear in 1998:2013){
    population <- y[myRegion, which(substr(names(y),5,8) %in% myYear)]
    myRows <- which(temp$Y ==myYear)
    mr[myRows, myRegion + 1] <- temp[myRows, 
                                     myRegion + 1]/(population/length(myRows)) 
  }
  
}

mrmelt <- melt(data = mr, id.vars="Date", value.name="Deaths", variable.name="Region")
mrmelt$Y <- format(as.Date(mrmelt$Date), "%Y")

ggplot(data = mrmelt)+
  geom_bar(aes(x = Y, y = Deaths, group=Region), stat="identity", position='dodge')

saveRDS(object = mr, file = "data/Health/MortalityRates.rds")

########## YEARLY DEATHS BY SEX AND AGE, ALL CAUSES ############################
# http://www.ons.gov.uk/ons/about-ons/business-transparency/freedom-of-information/what-can-i-request/published-ad-hoc-data/health/january-2015/deaths-by-lower-super-output-areas/index.html

library(XML)
library(stringr)
url <- "http://www.ons.gov.uk/ons/about-ons/business-transparency/freedom-of-information/what-can-i-request/published-ad-hoc-data/health/january-2015/deaths-by-lower-super-output-areas/index.html"
html <- paste(readLines(url), collapse="\n")
matched <- str_match_all(html, "<a href=\"(.*?)\"")
matched <- unlist(matched)
x <- matched[grep(".zip", matched)]
###

library(openxlsx)
library(dplyr)

folderPATH <- "data/Health/DeathsUK/DeathsbyLowerSuperOutputAreas1981-2013/"
x <- list.files(folderPATH)

df <- data.frame(matrix(NA, ncol = 3, nrow = 0))
for (i in 1:length(x)){
  
  tmp <- readWorkbook(xlsxFile = paste(folderPATH,x[i],sep=""), 
                      sheet = 3, cols = 1:5)
  by_LSOA <- group_by(tmp, Year, LSOA.code) %>%
    summarise(sums = sum(Deaths, na.rm = TRUE)) %>%
    arrange(Year)
  
  df <- rbind(df,by_LSOA)
  
}

names(df) <- c("Year", "LSOA11CD", "Deaths")

library(reshape2)
dflong <- dcast(data = df, LSOA11CD~Year)

write.csv(dflong, "data/GEO/UK/AdminBoundaries/UK_ONSboundaries/Lower_layer_super_output_areas_YearlyDeaths.csv")

library(raster)
# read data    
p <- shapefile("data/GEO/UK/AdminBoundaries/UK_ONSboundaries/Lower_layer_super_output_areas_(E+W)_2011_Boundaries_(Full_Extent)_V2/LSOA_2011_EW_BFE_V2.shp")
d <- read.csv("data/GEO/UK/AdminBoundaries/UK_ONSboundaries/Lower_layer_super_output_areas_YearlyDeaths.csv")

# merge on common variable, here called 'key'
m <- merge(p, d, by='LSOA11CD')

# perhaps save as shapefile again
shapefile(m, "data/GEO/UK/AdminBoundaries/UK_ONSboundaries/LSOAyearlyDeaths.shp")

########## YEARLY DEATHS BY REGION AND CAUSES ##################################

# Source:
# http://www.ons.gov.uk/ons/rel/vsob1/mortality-statistics--deaths-registered-in-england-and-wales--series-dr-/2014/rft-table-5.xls

# http://www.ons.gov.uk/ons/publications/re-reference-tables.html?edition=tcm%3A77-370351

YearlyHAinUK_NUTS1 <- readRDS("data/Health/HospitalAdmissionsUK/Yearly/YearlyHAinUK_NUTS1.rds")

# ADMIN LEVEL 2: ONS Local Unitary Authorities 2011 vs 2008 ####################
# source("scripts/GetData/LA2008shapefile.R")

# Health data for LA
YearlyHAinUK_NUTS3 <- readRDS("Health/HospitalAdmissionsUK/Yearly/YearlyHAinUK_NUTS3.rds")

# In 2008, the boundaries changed and a new naming convention was adopted
for (myCODE in unique(YearlyHAinUK_NUTS3$LA_CODE)) {
  if (myCODE %in% UK_adm2@data$LAD08CD) {
    
  }else{
    print(myCODE)
  }
}

# This are areas that have been aggregated in 2008
# [1] "00EJ"
# [1] "00EM"
# [1] "00EQ"
# [1] "00EW"
# [1] "00GG"
# [1] "00KB"
# [1] "00KC"
# [1] "00HE"
# [1] "00HF"
# [1] "00HY"

# When linking to time series data, refer to the LAO08CD

################################################################################
######################## HOSPITAL ADMISSIONS ###################################
################################################################################

# ONS Hospital Admissions: Summary Statistics, Apr 2002/ Mar 08 ################
# source:
# http://www.neighbourhood.statistics.gov.uk/dissemination/datasetList.do?JSAllowed=true&ph=60&CurrentPageId=60&step=1&datasetFamilyId=1920&Next.x=1&Next.y=1&nsjs=true&nsck=false&nssvg=false&nswid=1855
# We can only look at the GOR(1) and LA(2) sheets because we have correpsondence with NUTS code

setwd("data/Health/HospitalAdmissionsUK/Yearly/")

# GOR = NUTS1 from Apr 2007 to Mar 2008 ########################################
YearlyHAinUK_NUTS1 <- read.xls("F710308_2116_GeoPolicy.xls", 
                               sheet = "GOR", header = TRUE, skip=5,
                               stringsAsFactors=FALSE)[2:10,c(9,10,13:21)]
names(YearlyHAinUK_NUTS1) <- c("GOR_CODE","GOR_NAME", "All", 
                               "CHD", "Cerebrovascular_Disease", "Cancer",
                               "Falls", "CABG_PTCA", "Hip Replacement", 
                               "Knee Replacement", "Cataract")
YearlyHAinUK_NUTS1$LastYearlyUpdate <- 2008
# Apr 2006 to Mar 2007
temp <- read.xls("F710307_1931_GeoPolicy.xls", 
                 sheet = "GOR", header = TRUE, skip=5,
                 stringsAsFactors=FALSE)[2:10,c(9,10,13:21)]
names(temp) <- c("GOR_CODE","GOR_NAME", "All", 
                 "CHD", "Cerebrovascular_Disease", "Cancer",
                 "Falls", "CABG_PTCA", "Hip Replacement", 
                 "Knee Replacement", "Cataract")
temp$LastYearlyUpdate <- 2007
YearlyHAinUK_NUTS1 <- rbind(YearlyHAinUK_NUTS1,temp)
# Apr 2005 to Mar 2006
temp <- read.xls("F710306_1921_GeoPolicy.xls", 
                 sheet = "GOR", header = TRUE, skip=5,
                 stringsAsFactors=FALSE)[2:10,c(9,10,13:21)]
names(temp) <- c("GOR_CODE","GOR_NAME", "All", 
                 "CHD", "Cerebrovascular_Disease", "Cancer",
                 "Falls", "CABG_PTCA", "Hip Replacement", 
                 "Knee Replacement", "Cataract")
temp$LastYearlyUpdate <- 2006
YearlyHAinUK_NUTS1 <- rbind(YearlyHAinUK_NUTS1,temp)
# Apr 2004 to Mar 2005
temp <- read.xls("F710305_1937_GeoPolicy.xls", 
                 sheet = "GOR", header = TRUE, skip=5,
                 stringsAsFactors=FALSE)[2:10,c(9,10,13:21)]
names(temp) <- c("GOR_CODE","GOR_NAME", "All", 
                 "CHD", "Cerebrovascular_Disease", "Cancer",
                 "Falls", "CABG_PTCA", "Hip Replacement", 
                 "Knee Replacement", "Cataract")
temp$LastYearlyUpdate <- 2005
YearlyHAinUK_NUTS1 <- rbind(YearlyHAinUK_NUTS1,temp)
# Apr 2003 to Mar 2004
temp <- read.xls("F710304_1936_GeoPolicy.xls", 
                 sheet = "GOR", header = TRUE, skip=5,
                 stringsAsFactors=FALSE)[2:10,c(9,10,13:21)]
names(temp) <- c("GOR_CODE","GOR_NAME", "All", 
                 "CHD", "Cerebrovascular_Disease", "Cancer",
                 "Falls", "CABG_PTCA", "Hip Replacement", 
                 "Knee Replacement", "Cataract")
temp$LastYearlyUpdate <- 2004
YearlyHAinUK_NUTS1 <- rbind(YearlyHAinUK_NUTS1,temp)
# Apr 2002 to Mar 2003
temp <- read.xls("F710303_1935_GeoPolicy.xls", 
                 sheet = "GOR", header = TRUE, skip=5,
                 stringsAsFactors=FALSE)[2:10,c(9,10,13:21)]
names(temp) <- c("GOR_CODE","GOR_NAME", "All", 
                 "CHD", "Cerebrovascular_Disease", "Cancer",
                 "Falls", "CABG_PTCA", "Hip Replacement", 
                 "Knee Replacement", "Cataract")
temp$LastYearlyUpdate <- 2003
YearlyHAinUK_NUTS1 <- rbind(YearlyHAinUK_NUTS1,temp)

saveRDS(YearlyHAinUK_NUTS1, file="YearlyHAinUK_NUTS1.rds")

# LA = NUTS3 from Apr 2007 to Mar 2008 #########################################
YearlyHAinUK_NUTS3 <- read.xls("F710308_2116_GeoPolicy.xls", 
                               sheet = "LA", header = TRUE, skip=5,
                               stringsAsFactors=FALSE)[2:326,c(1,2,5,6,9:17)]
names(YearlyHAinUK_NUTS3) <- c("GOR_CODE","GOR_NAME","LA_CODE","LA_NAME", "All",
                               "CHD", "Cerebrovascular_Disease", "Cancer",
                               "Falls", "CABG_PTCA", "Hip Replacement", 
                               "Knee Replacement", "Cataract")
YearlyHAinUK_NUTS3$LastYearlyUpdate <- 2008
# Apr 2006 to Mar 2007
temp <- read.xls("F710307_1931_GeoPolicy.xls", 
                 sheet = "LA", header = TRUE, skip=5,
                 stringsAsFactors=FALSE)[2:326,c(1,2,5,6,9:17)]
names(temp) <- c("GOR_CODE","GOR_NAME","LA_CODE","LA_NAME", "All",
                 "CHD", "Cerebrovascular_Disease", "Cancer",
                 "Falls", "CABG_PTCA", "Hip Replacement", 
                 "Knee Replacement", "Cataract")
temp$LastYearlyUpdate <- 2007
YearlyHAinUK_NUTS3 <- rbind(YearlyHAinUK_NUTS3,temp)
# Apr 2005 to Mar 2006
temp <- read.xls("F710306_1921_GeoPolicy.xls", 
                 sheet = "LA", header = TRUE, skip=5,
                 stringsAsFactors=FALSE)[2:326,c(1,2,5,6,9:17)]
names(temp) <- c("GOR_CODE","GOR_NAME","LA_CODE","LA_NAME", "All",
                 "CHD", "Cerebrovascular_Disease", "Cancer",
                 "Falls", "CABG_PTCA", "Hip Replacement", 
                 "Knee Replacement", "Cataract")
temp$LastYearlyUpdate <- 2006
YearlyHAinUK_NUTS3 <- rbind(YearlyHAinUK_NUTS3,temp)
# Apr 2004 to Mar 2005
temp <- read.xls("F710305_1937_GeoPolicy.xls", 
                 sheet = "LA", header = TRUE, skip=5,
                 stringsAsFactors=FALSE)[2:326,c(1,2,5,6,9:17)]
names(temp) <- c("GOR_CODE","GOR_NAME","LA_CODE","LA_NAME", "All",
                 "CHD", "Cerebrovascular_Disease", "Cancer",
                 "Falls", "CABG_PTCA", "Hip Replacement", 
                 "Knee Replacement", "Cataract")
temp$LastYearlyUpdate <- 2005
YearlyHAinUK_NUTS3 <- rbind(YearlyHAinUK_NUTS3,temp)
# Apr 2003 to Mar 2004
temp <- read.xls("F710304_1936_GeoPolicy.xls", 
                 sheet = "LA", header = TRUE, skip=5,
                 stringsAsFactors=FALSE)[2:326,c(1,2,5,6,9:17)]
names(temp) <- c("GOR_CODE","GOR_NAME","LA_CODE","LA_NAME", "All",
                 "CHD", "Cerebrovascular_Disease", "Cancer",
                 "Falls", "CABG_PTCA", "Hip Replacement", 
                 "Knee Replacement", "Cataract")
temp$LastYearlyUpdate <- 2004
YearlyHAinUK_NUTS3 <- rbind(YearlyHAinUK_NUTS3,temp)
# Apr 2002 to Mar 2003
temp <- read.xls("F710303_1935_GeoPolicy.xls", 
                 sheet = "LA", header = TRUE, skip=5,
                 stringsAsFactors=FALSE)[2:326,c(1,2,5,6,9:17)]
names(temp) <- c("GOR_CODE","GOR_NAME","LA_CODE","LA_NAME", "All",
                 "CHD", "Cerebrovascular_Disease", "Cancer",
                 "Falls", "CABG_PTCA", "Hip Replacement", 
                 "Knee Replacement", "Cataract")
temp$LastYearlyUpdate <- 2003
YearlyHAinUK_NUTS3 <- rbind(YearlyHAinUK_NUTS3,temp)
match 

saveRDS(YearlyHAinUK_NUTS3, file="YearlyHAinUK_NUTS3.rds")

# ALSO AVAILABLE ON THE SAME SITE: Hospital Admissions by Age and Sex

# Now bind HA to SpatialPolygons of admin areas ################################

setwd("data/Health/HospitalAdmissionsUK/Yearly/")
library(maptools)

NUTS1YearlyHA <- readRDS("../../../../data/GEO/UK/NUTS_2013/UK_nuts1.rds")
LUtable <- readRDS("../../../../data/GEO/LookUpTables/NUTS1GOR.rds")
LUtable <- LUtable[1:9,]

filenames <- list.files(pattern = "\\.xls$")

for (myFILE in filenames){ # from 2003 to 2008
  temp   <- NUTS1YearlyHA@data
  myYEAR <- paste("20",substr(x = myFILE, start = 6, stop = 7),sep="")
  GOR    <- read.xls(myFILE, sheet = "GOR", header = TRUE, skip=5,
                     stringsAsFactors=FALSE)[2:10,9]
  
  HA_all <- data.frame("All" = read.xls(myFILE, sheet = "GOR", 
                                        header = TRUE, skip=5,
                                        stringsAsFactors=FALSE)[2:10,13])
  HA_all$NUTS_ID <- as.character(LUtable$NUTS1_code[LUtable$GOR_code==GOR])
  
  HA_CHD <- read.xls(myFILE, sheet = "GOR", header = TRUE, skip=5,
                     stringsAsFactors=FALSE)[2:10,14]
  HA_CD <- read.xls(myFILE, sheet = "GOR", header = TRUE, skip=5,
                    stringsAsFactors=FALSE)[2:10,15]
  HA_CANCER <- read.xls(myFILE, sheet = "GOR", header = TRUE, skip=5,
                        stringsAsFactors=FALSE)[2:10,16]
}

HA$NUTS1 <- LUtable$NUTS1_code[LUtable$GOR_code == HA$GOR_CODE]


spCbind(gisData,1:nrow(gisData))



################################################################################
# Hospital Episode Statistics (HES): 
# Outpatient Activity - Provider-level analysis
# Provisional Monthly Hospital Episode Statistics for Admitted Patient Care, Outpatients and Accident and Emergency Data - from Apr 2008 to June 2015 #######

# Source: http://www.hscic.gov.uk/hes
# and specifically: http://www.hscic.gov.uk/searchcatalogue?q=title%3a%22Provisional+Monthly+Hospital+Episode+Statistics%22&sort=Most+recent&size=100&page=1#top

# This data is useless! It should be monthly but it is just a yearly average of 
# monthly data

# setwd("data/Health/HospitalAdmissionsUK/Monthly/")

# CHECK OTHER HSCIC INDICATORS:
# /home/kz/Dropbox/Projects/kehra/data/Health/Indicator Portal indicators.xls

################################################################################
# ONS Neighbourhood statistics
# http://www.neighbourhood.statistics.gov.uk/dissemination/instanceSelection.do?JSAllowed=true&Function=&%24ph=60_61&CurrentPageId=61&step=2&datasetFamilyId=824&instanceSelection=015182&Back.x=18&Back.y=17
