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

rm(df)

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

saveRDS(data, "~/kehra/data/dataset.rds")

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
# DRAW CORRELATIONMATRIX FOR OBSERVED AND GENERATED VARIABLES ##################
################################################################################

as.data.frame(names(training))
str(training)

library(corrplot)
corrplot(cor(training[,10:92], use = "complete.obs"), order = "original")
