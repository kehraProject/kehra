################################################################################
######################## POPULATION ESTIMATES ##################################
################################################################################

# DOWNLOAD DATASET FROM THE SOURCE BELOW:
# http://www.ons.gov.uk/ons/data/web/explorer/dataset-finder/-/q/dcDetails/Social/MYEDE?p_p_lifecycle=1&_FOFlow1_WAR_FOFlow1portlet_dataset_navigation=datasetCollectionDetails

# Clean the current environment
rm(list = ls())

library(openxlsx) # handles xlsx file

######################## BY COUNTRY ############################################

x <- read.xlsx(xlsxFile = "data/SocioEconomic/MYEDE  Population Estimates for High Level Areas/XLS_MYEDE_2011STATH_2_EN.xlsx", sheet = "CTRY", 
               rows = c(10, 11,12,13,14), cols = c(1,2,3,4,5,6,282,558,834))
names(x) <- c("ParentName", "ParentCode", "AreaName", "AreaCode", "AreaType",
              "X2014", "X2013", "X2012", "X2011")

saveRDS(object = x, file = "~/Dropbox/Projects/kehra/data/SocioEconomic/PopulationEstimatesCountry.rds")

######################## BY LOCAL AUTHORITY ####################################

x <- read.xlsx(xlsxFile = "data/SocioEconomic/MYEDE  Population Estimates for High Level Areas/XLS_MYEDE_2011STATH_2_EN.xlsx", sheet = "LONB_MD_NMD_UA", 
               rows = c(10:358), cols = c(1,2,3,4,5,6,282,558,834))
names(x) <- c("ParentName", "ParentCode", "AreaName", "AreaCode", "AreaType",
              "X2014", "X2013", "X2012", "X2011")

saveRDS(object = x, file = "~/Dropbox/Projects/kehra/data/SocioEconomic/PopulationEstimatesLA.rds")

######################## BY REGION #############################################
library(gdata)

x <- read.csv("data/SocioEconomic/CSV_MYEDE2011STATH_EN_277237.csv", skip = 8)[1:9,]
names(x)[1] <- "Code"
y <- read.xls(xls = "data/SocioEconomic/MYE6TS3B_mid-1971-mid-2012-unformatted-country-data-file.xls", 
              sheet = "Mid-1971 to Mid-2012", skip=1, header=TRUE)[c(8:16),]
y[,3:44] <- y[,3:44] * 1000
df <- join(x,y[,c(1,3:33)],by="Code",type="inner")
df <- df[,c("Code", "Geographic.Area", 
            "Mid.1971", "Mid.1972", "Mid.1973", "Mid.1974", "Mid.1975", 
            "Mid.1976", "Mid.1977", "Mid.1978", "Mid.1979", "Mid.1980", 
            "Mid.1981", "Mid.1982", "Mid.1983", "Mid.1984", "Mid.1985", 
            "Mid.1986", "Mid.1987", "Mid.1988", "Mid.1989", "Mid.1990", 
            "Mid.1991", "Mid.1992", "Mid.1993", "Mid.1994", "Mid.1995", 
            "Mid.1996", "Mid.1997", "Mid.1998", "Mid.1999", "Mid.2000",
            "Mid.2001",
            "X2002.Total..All.Ages.Total..All.Persons",
            "X2003.Total..All.Ages.Total..All.Persons",
            "X2004.Total..All.Ages.Total..All.Persons",
            "X2005.Total..All.Ages.Total..All.Persons", 
            "X2006.Total..All.Ages.Total..All.Persons",
            "X2007.Total..All.Ages.Total..All.Persons",
            "X2008.Total..All.Ages.Total..All.Persons",
            "X2009.Total..All.Ages.Total..All.Persons",
            "X2010.Total..All.Ages.Total..All.Persons",
            "X2011.Total..All.Ages.Total..All.Persons",
            "X2012.Total..All.Ages.Total..All.Persons",
            "X2013.Total..All.Ages.Total..All.Persons",
            "X2014.Total..All.Ages.Total..All.Persons")]
names(df)[34:46] <- c("Mid.2002","Mid.2003","Mid.2004","Mid.2005","Mid.2006","Mid.2007","Mid.2008","Mid.2009","Mid.2010","Mid.2011","Mid.2012","Mid.2013","Mid.2014")

saveRDS(object = df, file = "data/SocioEconomic/PopulationEstimatesRegions1971_2014.rds")
