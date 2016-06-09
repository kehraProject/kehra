# Load the full dataset for the UK.
df <- readRDS("~/r_kehra/data/EnglandDB.rds")

################################################################################
# MAKE A MAP OF THE ACTIVE STATIONS ############################################
################################################################################
library(devtools)
library(ggmap)

# for theme_map
source_gist("33baa3a79c5cfef0f6df")

# Load data
stations <- readRDS("~/r_kehra/data/Pollution/stations.rds")

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

# saveRDS(data, "~/r_kehra/data/dataset.rds")

################################################################################
# SPLIT THE DATASET INTO TRAINING AND TESTING SETS #############################
################################################################################

data <- readRDS("~/r_kehra/data/dataset.rds")

# Training set contains all the data up to 2007, testing only data for 2008-14
ind <- which(as.numeric(as.character(data$Year)) <= 2007)

training <- data[ind, ] # ~77% round(dim(training)[1]/dim(data)[1],2)
testing <- data[-ind, ] # ~23 round(dim(testing)[1]/dim(data)[1],2)

saveRDS(training, "~/r_kehra/data/training.rds")
saveRDS(testing, "~/r_kehra/data/testing.rds")

################################################################################
# DRAW CORRELATIONMATRIX FOR OBSERVED AND GENERATED VARIABLES ##################
################################################################################

rm(data, testing, ind); gc()

as.data.frame(names(training))
str(training)

# Re-order continuous features in the training columns

trainingConOrd <- training[, c("Latitude","Longitude","Altitude", 
                               "t2m","ws","wd","tp","blh","ssr",      
                               "pm10",
                               "pm10_l1","pm10_l2","pm10_l3","pm10_l4","pm10_l5", 
                               "pm10_c1","pm10_c2","pm10_c3","pm10_c4","pm10_c5",
                               "pm2.5",
                               "pm2.5_l1","pm2.5_l2","pm2.5_l3","pm2.5_l4","pm2.5_l5",
                               "pm2.5_c1","pm2.5_c2","pm2.5_c3","pm2.5_c4","pm2.5_c5", 
                               "no2",
                               "no2_l1","no2_l2","no2_l3","no2_l4","no2_l5",
                               "no2_c1","no2_c2","no2_c3","no2_c4","no2_c5",
                               "o3",
                               "o3_l1","o3_l2","o3_l3","o3_l4","o3_l5",
                               "o3_c1","o3_c2","o3_c3","o3_c4","o3_c5",
                               "so2",
                               "so2_l1","so2_l2","so2_l3","so2_l4","so2_l5",
                               "so2_c1","so2_c2","so2_c3","so2_c4","so2_c5",
                               "co",
                               "co_l1","co_l2","co_l3","co_l4","co_l5",
                               "co_c1","co_c2","co_c3","co_c4","co_c5",
                               "CVD00","CVD20","CVD40","CVD60",
                               "LIV00","LIV20","LIV40","LIV60")]

completeTraining <- trainingConOrd[complete.cases(trainingConOrd),]
saveRDS(completeTraining, "~/r_kehra/data/completeTraining.rds")

library(corrplot)
corrplot(corr = cor(completeTraining), order = "original")

## Correlation matrix for large datasets #######################################
library(ff)

bigcor <- function(x, nblocks = 10, verbose = TRUE, ...){
  
  NCOL <- ncol(x)
  
  ## test if ncol(x) %% nblocks gives remainder 0
  if (NCOL %% nblocks != 0) stop("Choose different 'nblocks' so that ncol(x) %% nblocks = 0!")
  
  ## preallocate square matrix of dimension
  ## ncol(x) in 'ff' single format
  corMAT <- ff(vmode = "single", dim = c(NCOL, NCOL))
  
  ## split column numbers into 'nblocks' groups
  SPLIT <- split(1:NCOL, rep(1:nblocks, each = NCOL/nblocks))
  
  ## create all unique combinations of blocks
  COMBS <- expand.grid(1:length(SPLIT), 1:length(SPLIT))
  COMBS <- t(apply(COMBS, 1, sort))
  COMBS <- unique(COMBS)
  
  ## iterate through each block combination, calculate correlation matrix
  ## between blocks and store them in the preallocated matrix on both
  ## symmetric sides of the diagonal
  for (i in 1:nrow(COMBS)) {
    COMB <- COMBS[i, ]
    G1 <- SPLIT[[COMB[1]]]
    G2 <- SPLIT[[COMB[2]]]
    if (verbose) cat("Block", COMB[1], "with Block", COMB[2], "\n")
    flush.console()
    COR <- cor(x[, G1], x[, G2], ...)
    corMAT[G1, G2] <- COR
    corMAT[G2, G1] <- t(COR)
    COR <- NULL
  }
  
  gc()
  return(corMAT)
}

# CorrelationMatrix <- bigcor(training[,10:92], nblocks = 41)
# instead of cor(training[,10:92], use = "complete.obs")


