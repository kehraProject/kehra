# sudo apt-get install libgmp3-dev
# install.packages("gmp")
# install.packages("pcalg")
library(pcalg)
require(Rgraphviz)

# Load data
df <- readRDS("data/UKdataset.rds")

# option 0: Remove only categorical features
df2 <- df[, -c(1:17)]
## estimate CPDAG
pc.fit0 <- pc(suffStat = list(C = cor(df2), n = nrow(df2)),
              indepTest = gaussCItest, ## indep.test: partial correlations
              alpha=0.01, verbose = TRUE, 
              labels = c("LAT", "LON", "A",
                         "ws", "wd", "o3", "so2", "co", "pm10", "pm2.5",
                         "nv2.5", "v2.5", "nv10", "v10", "no", "no2", "nox",
                         "TEMP", "WS", "WD", "BLH", "PREC", 
                         "C00", "C20", "C40", "C60", 
                         "L00", "L20", "L40", "L60"))
pc.fit0
## show estimated CPDAG
plot(pc.fit0, main = "Estimated CPDAG")

# option 1: keep only most important pollutant and all the climate variables features
df3 <- df[, -c(1:17,21:22, 28:32, 34, 40:42, 44:46)]
# which column contans NAs?
# for(col in 1:dim(df3)[2]) if (any(is.na(df3[,col]))) print(paste(col, names(df3)[col]))
## estimate CPDAG
pc.fit1 <- pc(suffStat = list(C = cor(df3), n = nrow(df3)),
              indepTest = gaussCItest, ## indep.test: partial correlations
              alpha=0.01, verbose = TRUE, 
              labels = c("LAT", "LON", "A",
                         "o3", "so2", "co", "pm10", "pm2.5", "no2", 
                         "TEMP", "WS", "WD", "BLH", "PREC", 
                         "C60", "L60"))
pc.fit1
## show estimated CPDAG
plot(pc.fit1, main = "Estimated CPDAG")

# option 2: keep only most important pollutant and all the climate variables features, only complete cases
df4 <- df[, -c(1:17,21:22, 28:32, 34, 40:42, 44:46)]
# which column contans NAs?
# for(col in 1:dim(df3)[2]) if (any(is.na(df3[,col]))) print(paste(col, names(df4)[col]))
df4 <- df4[complete.cases(df4),]
## estimate CPDAG
pc.fit2 <- pc(suffStat = list(C = cor(df4), n = nrow(df4)),
             indepTest = gaussCItest, ## indep.test: partial correlations
             alpha=0.01, verbose = TRUE, 
             labels = c("LAT", "LON", "A",
                        "o3", "so2", "co", "pm10", "pm2.5", "no2", 
                        "TEMP", "WS", "WD", "BLH", "PREC", 
                        "C60", "L60"))
pc.fit2
## show estimated CPDAG
plot(pc.fit2, main = "Estimated CPDAG")
