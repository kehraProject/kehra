# Load the full dataset for the UK.
df <- readRDS("~/kehra/data/EnglandDB.rds")

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
# SPLIT THE DATASET INTO TRAINING AND TESTING SETS #############################
################################################################################

# Training set contains all the data up to 2007, testing only data for 2008-14
ind <- which(as.numeric(as.character(df$Year)) <= 2007)

training <- df[ind, ] # ~77% round(dim(training)[1]/dim(df)[1],2)
testing <- df[-ind, ] # ~23 round(dim(testing)[1]/dim(df)[1],2)

saveRDS(training, "~/kehra/data/training.rds")
saveRDS(testing, "~/kehra/data/testing.rds")
