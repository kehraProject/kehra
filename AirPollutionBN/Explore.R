# Load the full dataset for the UK.
df <- readRDS("~/kehra/data/EnglandDB.rds")

################################################################################
# MAKE A MAP OF THE ACTIVE STATIONS ############################################
################################################################################
library(devtools)
library(ggmap)

# for theme_map
source_gist("33baa3a79c5cfef0f6df")

# Load data
stations <- readRDS("~/kehra/data/Pollution/stations.rds")

myMAP <- get_map("birmingham, uk",zoom=6)
ggmap(myMAP)+
  geom_point(data = data.frame(stations), aes(x = Longitude, y = Latitude),
             alpha=0.5 , color = "red") +
  xlab("Longitude") + ylab("Latitude")

################################################################################
# FIND OUT TEMPORAL COVERAGE ###################################################
################################################################################

# How many stations?
length(unique(df$SiteID)) # 162

# Range of temporal coverage
# remove all the row that contain only NA
ind <- apply(df[, c("PM10", "PM2.5", "NO2", "O3", "SO2", "CO")], 1, function(y) all(is.na(y)))
# Group by site
grouped <- group_by(df[!ind, ], SiteID)
x <- summarise(grouped, uniqueYears = n_distinct(Year))
summary(x$uniqueYears)

# How many stations measured O3? For how many years?
length(unique(df$SiteID[!is.na(df$O3)])) # 95 stations
x <- group_by(df[!is.na(df$O3),], SiteID) %>% summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 15 years

# How many stations measured CO? For how many years?
length(unique(df$SiteID[!is.na(df$CO)])) # 80 stations
x <- group_by(df[!is.na(df$CO),], SiteID) %>% summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 11 years

# How many stations measured NO2? For how many years?
length(unique(df$SiteID[!is.na(df$NO2)])) # 146 stations
x <- group_by(df[!is.na(df$NO2),], SiteID) %>% summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 12 years

# How many stations measured PM10? For how many years?
length(unique(df$SiteID[!is.na(df$PM10)])) # 54 stations
x <- group_by(df[!is.na(df$PM10),], SiteID) %>% summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 6 years

# How many stations measured PM2.5? For how many years?
length(unique(df$SiteID[!is.na(df$PM2.5)])) # 63 stations
x <- group_by(df[!is.na(df$PM2.5),], SiteID) %>% summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 6 years

# How many stations measured SO2? For how many years?
length(unique(df$site[!is.na(df$so2)])) # 56
sort(unique(df$Year[!is.na(df$so2)])) # 17 years
length(unique(df$SiteID[!is.na(df$SO2)])) # 54 stations
x <- group_by(df[!is.na(df$SO2),], SiteID) %>% summarise(uniqueYears = n_distinct(Year))
summary(x$uniqueYears) # (mean) 6 years

rm(grouped, x, ind)

################################################################################
# GENERATE LAGGED VARIABLES ####################################################
################################################################################

library(dplyr)
data <- 
  df %>%
  group_by(SiteID) %>%
  mutate(pm10_l1 = lag(pm10, 24)) %>%
  mutate(pm10_l2 = lag(pm10, 48)) %>%
  mutate(pm10_l3 = lag(pm10, 72)) %>%
  mutate(pm10_l4 = lag(pm10, 96)) %>%
  mutate(pm10_l5 = lag(pm10, 120)) %>%
  mutate(pm2.5_l1 = lag(pm2.5, 24)) %>%
  mutate(pm2.5_l2 = lag(pm2.5, 48)) %>%
  mutate(pm2.5_l3 = lag(pm2.5, 72)) %>%
  mutate(pm2.5_l4 = lag(pm2.5, 96)) %>%
  mutate(pm2.5_l5 = lag(pm2.5, 120)) %>%
  mutate(no2_l1 = lag(no2, 24)) %>%
  mutate(no2_l2 = lag(no2, 48)) %>%
  mutate(no2_l3 = lag(no2, 72)) %>%
  mutate(no2_l4 = lag(no2, 96)) %>%
  mutate(no2_l5 = lag(no2, 120)) %>%
  mutate(o3_l1 = lag(o3, 24)) %>%
  mutate(o3_l2 = lag(o3, 48)) %>%
  mutate(o3_l3 = lag(o3, 72)) %>%
  mutate(o3_l4 = lag(o3, 96)) %>%
  mutate(o3_l5 = lag(o3, 120)) %>%
  mutate(so2_l1 = lag(so2, 24)) %>%
  mutate(so2_l2 = lag(so2, 48)) %>%
  mutate(so2_l3 = lag(so2, 72)) %>%
  mutate(so2_l4 = lag(so2, 96)) %>%
  mutate(so2_l5 = lag(so2, 120)) %>%
  mutate(co_l1 = lag(co, 24)) %>%
  mutate(co_l2 = lag(co, 48)) %>%
  mutate(co_l3 = lag(co, 72)) %>%
  mutate(co_l4 = lag(co, 96)) %>%
  mutate(co_l5 = lag(co, 120))

################################################################################
# GENERATE CUMULATED VARIABLES #################################################
################################################################################

library(RcppRoll)
library(dplyr)
data <- 
  data %>%
  group_by(SiteID) %>%
  mutate(pm10_c1 = roll_sum(pm10, 24, align = "right", fill = NA)) %>%
  mutate(pm10_c2 = roll_sum(pm10, 48, align = "right", fill = NA)) %>%
  mutate(pm10_c3 = roll_sum(pm10, 72, align = "right", fill = NA)) %>%
  mutate(pm10_c4 = roll_sum(pm10, 96, align = "right", fill = NA)) %>%
  mutate(pm10_c5 = roll_sum(pm10, 120, align = "right", fill = NA)) %>%
  mutate(pm2.5_c1 = roll_sum(pm2.5, 24, align = "right", fill = NA)) %>%
  mutate(pm2.5_c2 = roll_sum(pm2.5, 48, align = "right", fill = NA)) %>%
  mutate(pm2.5_c3 = roll_sum(pm2.5, 72, align = "right", fill = NA)) %>%
  mutate(pm2.5_c4 = roll_sum(pm2.5, 96, align = "right", fill = NA)) %>%
  mutate(pm2.5_c5 = roll_sum(pm2.5, 120, align = "right", fill = NA)) %>%
  mutate(no2_c1 = roll_sum(no2, 24, align = "right", fill = NA)) %>%
  mutate(no2_c2 = roll_sum(no2, 48, align = "right", fill = NA)) %>%
  mutate(no2_c3 = roll_sum(no2, 72, align = "right", fill = NA)) %>%
  mutate(no2_c4 = roll_sum(no2, 96, align = "right", fill = NA)) %>%
  mutate(no2_c5 = roll_sum(no2, 120, align = "right", fill = NA)) %>%
  mutate(o3_c1 = roll_sum(o3, 24, align = "right", fill = NA)) %>%
  mutate(o3_c2 = roll_sum(o3, 48, align = "right", fill = NA)) %>%
  mutate(o3_c3 = roll_sum(o3, 72, align = "right", fill = NA)) %>%
  mutate(o3_c4 = roll_sum(o3, 96, align = "right", fill = NA)) %>%
  mutate(o3_c5 = roll_sum(o3, 120, align = "right", fill = NA)) %>%
  mutate(so2_c1 = roll_sum(so2, 24, align = "right", fill = NA)) %>%
  mutate(so2_c2 = roll_sum(so2, 48, align = "right", fill = NA)) %>%
  mutate(so2_c3 = roll_sum(so2, 72, align = "right", fill = NA)) %>%
  mutate(so2_c4 = roll_sum(so2, 96, align = "right", fill = NA)) %>%
  mutate(so2_c5 = roll_sum(so2, 120, align = "right", fill = NA)) %>%
  mutate(co_c1 = roll_sum(co, 24, align = "right", fill = NA)) %>%
  mutate(co_c2 = roll_sum(co, 48, align = "right", fill = NA)) %>%
  mutate(co_c3 = roll_sum(co, 72, align = "right", fill = NA)) %>%
  mutate(co_c4 = roll_sum(co, 96, align = "right", fill = NA)) %>%
  mutate(co_c5 = roll_sum(co, 120, align = "right", fill = NA))

################################################################################
# DRAW CORRELATIONMATRIX FOR OBSERVED AND GENERATED VARIABLES ##################
################################################################################

names(data)

data <- data[,c("Lat","Lon","Alt","Year","Sea","Mon","Day","Hour",
                "t2m","ws","wd","tp","blh","ssr",
                "pm10","pm10_l1","pm10_l2","pm10_l3","pm10_l4","pm10_l5", 
                "pm10_c1","pm10_c2","pm10_c3","pm10_c4","pm10_c5",
                "pm2.5", "pm2.5_l1","pm2.5_l2","pm2.5_l3","pm2.5_l4","pm2.5_l5",
                "pm2.5_c1","pm2.5_c2","pm2.5_c3","pm2.5_c4","pm2.5_c5",
                "no2","no2_l1","no2_l2","no2_l3","no2_l4","no2_l5", 
                "no2_c1","no2_c2","no2_c3","no2_c4","no2_c5",
                "o3", "o3_l1","o3_l2","o3_l3","o3_l4","o3_l5",
                "o3_c1","o3_c2","o3_c3","o3_c4","o3_c5",
                "so2", "so2_l1","so2_l2","so2_l3","so2_l4","so2_l5",
                "so2_c1","so2_c2","so2_c3","so2_c4","so2_c5",
                "co","co_l1","co_l2","co_l3","co_l4","co_l5", 
                "co_c1","co_c2","co_c3","co_c4","co_c5")]

library(corrplot)
corrplot(cor(data, use = "complete.obs"), order = "original")

################################################################################
# SPLIT THE DATASET INTO TRAINING AND TESTING SETS #############################
################################################################################

# Training set contains all the data up to 2007, testing only data for 2008-14
ind <- which(as.numeric(as.character(data$Year)) <= 2007)

training <- data[ind, ] # ~77% round(dim(training)[1]/dim(data)[1],2)
testing <- data[-ind, ] # ~23 round(dim(testing)[1]/dim(data)[1],2)

saveRDS(training, "~/kehra/data/training.rds")
saveRDS(testing, "~/kehra/data/testing.rds")

################################################################################
# CLEAN UP TRAINING SET ########################################################
################################################################################

df <- readRDS("~/kehra/data/training.rds")

any(is.na(df$t2m))
dfclean <- df[-which(is.na(df$t2m)),]; rm(df)
any(is.na(dfclean$ws))
any(is.na(dfclean$wd))
any(is.na(dfclean$tp))
dfclean <- dfclean[-which(is.na(dfclean$tp)),]
any(is.na(dfclean$blh))
any(is.na(dfclean$ssr))

any(is.na(dfclean$CVD00))
dfclean <- dfclean[-which(is.na(dfclean$CVD00)),]
any(is.na(dfclean$CVD20))
any(is.na(dfclean$CVD40))
any(is.na(dfclean$CVD60))

df <- dfclean[complete.cases(dfclean[, c("PM10","PM2.5","NO2","O3","SO2","CO")]),]

summary(dfclean)

require(devtools)
install_github('davidcarslaw/openair')
library(openair)
source('~/r_kehra/AirPollutionBN/importAURN.R')

x <- importMeta(source='aurn', all = TRUE)
y <- importAURN(site = x$code, year = 1981:2014, pollutant = c("pm10","pm2.5","no2","o3","so2","co"), hc = FALSE)
yy <- y[complete.cases(y),]

w <- importAURN(site = stations$SiteID, year = 1981:2014, pollutant = c("pm10","pm2.5","no2","o3","so2","co"), hc = FALSE)
ww <- w[complete.cases(w),] 
names(ww) 

clima <- readRDS("~/kehra/data/Climate/clima_England_1981_2014.rds")
names(ww) <- c("datetime", "pm10", "pm2.5", "no2", "o3", "so2", "co", "site", "SiteID")
library(dplyr)
ww <- ww[,c(1:7,9)]
str(ww)
ww$datetime <- as.character(ww$datetime)
ww$SiteID <- as.character(ww$SiteID)
str(clima)

df1 <- left_join(ww, clima, by = c("datetime", "SiteID"))
df1 <- left_join(df1, stations, by = "SiteID")
df1$Year <- format(as.POSIXlt(df1$datetime), "%Y")
df1$Month <- format(as.POSIXlt(df1$datetime), "%m")
df1$Day <- format(as.POSIXlt(df1$datetime), "%d")
df1$Hour <- format(as.POSIXlt(df1$datetime), "%H")
library(kehra)
df1$Season <- getSeason(DATES = as.POSIXlt(df1$datetime))

df1 <- df1[,c("SiteID", "Region", "Zone", "Environment.Type", "Latitude", "Longitude", "Altitude",
              "Year", "Season", "Month", "Day", "Hour",
              "t2m", "ws", "wd", "tp", "blh", "ssr",
              "pm10", "pm2.5", "no2", "o3", "so2", "co")]
str(df1)
df1$Year <- as.numeric(df1$Year)
season <- as.factor(df1$Season)
levels(season) <- c(1,2,3,4) # 1 = "Fall", 2 = "Spring", 3 = "Summer", 4 = "Winter"
df1$Season <- as.numeric(levels(season))[season]
df1$Month <- as.numeric(df1$Month)
df1$Day <- as.numeric(df1$Day)
df1$Hour <- as.numeric(df1$Hour)

names(df1) <- c("ID", "Reg", "Zone", "Type", "Lat", "Lon", "Alt",
                "Year", "Sea", "Mon", "Day", "Hour",
                "t2m", "ws", "wd", "tp", "blh", "ssr",
                "pm10", "pm2.5", "no2", "o3", "so2", "co")

saveRDS(df1, "~/kehra/data/trainingComplete.rds")
