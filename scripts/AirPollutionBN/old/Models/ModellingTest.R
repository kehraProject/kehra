# TEST THE PC ALGORITHM FOR CAUSAL RELATIONSHIPS
```{r}
# Start SIMPLE!
df00 <- df[, c("LAT", "LON", "ALT", 
               "TEMP", "BLH", "PREC", "WS", "WD",
               "so2", "pm10", "pm2.5", "no2", "co", "o3",
               "CVD60")]
pc.fit0 <- pc(suffStat = list(C = cor(df00, use="complete.obs"), 
                              n = nrow(df00)),
              indepTest = gaussCItest, ## indep.test: partial correlations
              alpha=0.01, labels = names(df00))
## show estimated CPDAG
plot(pc.fit0, main = "Estimated CPDAG")

# option 0: Remove only categorical features
df2 <- df[, -c(1:17)]
# Generate sufficient statistics for the conditional independence test
## estimate CPDAG
pc.fit0 <- pc(suffStat = list(C = cor(df2, use="complete.obs"), n = nrow(df2)),
              indepTest = gaussCItest, ## indep.test: partial correlations
              alpha=0.005, verbose = TRUE, labels = names(df2))
pc.fit0
## show estimated CPDAG
plot(pc.fit0, main = "Estimated CPDAG")

# option 1: keep only most important pollutant and all the climate variables features
df3 <- df[, -c(1:17,21:22, 28:32, 34, 40:42, 44:47)]
# which column contans NAs?
# for(col in 1:dim(df3)[2]) if (any(is.na(df3[,col]))) print(paste(col, names(df3)[col]))
## estimate CPDAG
pc.fit1 <- pc(suffStat = list(C = cor(df3), n = nrow(df3)),
              indepTest = gaussCItest, ## indep.test: partial correlations
              alpha=0.01, verbose = TRUE, 
              labels = c("LAT", "LON", "A",
                         "o3", "so2", "co", "pm10", "pm2.5", "no2", 
                         "TEMP", "WS", "WD", "BLH", "PREC", 
                         "C60"))
pc.fit1
## show estimated CPDAG
plot(pc.fit1, main = "Estimated CPDAG")

# option 2: keep only most important pollutant and all the climate variables features, only complete cases
df4 <- df[, -c(1:17,21:22, 28:32, 34, 40:42, 44:47)]
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
                         "C60"))
pc.fit2
## show estimated CPDAG
plot(pc.fit2, main = "Estimated CPDAG")

# export pc.fit2 to bnlearn
res <- as.bn(pc.fit2@graph)
# check and modify node ordering
node.ordering(res)
ordering2blacklist(res)
res <- drop.arc(res, "LAT", "LON")
res <- drop.arc(res, "LAT", "A")
res <- drop.arc(res, "L60", "C60")
ord = node.ordering(res)
names(df4) <- c("LAT", "LON", "A",
                "o3", "so2", "co", "pm10", "pm2.5", "no2",
                "TEMP", "WS", "WD", "BLH", "PREC", 
                "C60", "L60")
bn.gs <- gs(df4[1:10000,], blacklist = ordering2blacklist(ord))
graphviz.plot(bn.gs)
score(x = bn.gs, data = df4[1:100,], type = "bic-g")

node.ordering(res) <- c("LAT", "LON", "A", "so2", "o3", "co", "pm10", "pm2.5",
                        "no2", "TEMP", "WS", "WD", "BLH", "PREC", "C60", "L60")
```

# TEST BNLEARN, regardless of PC results
```{r}
library(bnlearn)

# Start SIMPLE!
# Use ws/wd modelled at the station (hourly)
df0 <- df[, -c(20, 23:27, 29, 39:42)]

df0 <- df[, c("Year", "Month", "Day", "Time", "code", "Region", "site.type",
              "latitude", "longitude", "altitude", "ws", "wd", "o3", "so2",
              "co", "pm10", "pm2.5", "no2", "TEMP", "BLH", "PREC", 
              "StCVDcancer60")]
df0 <- df0[complete.cases(df0),]
# or Use WS/WD modelled from ECMWF ERA INTERIM (daily)
df1 <- df[, c("Year", "Month", "Day", "Time", "code", "Region", "site.type",
              "latitude", "longitude", "altitude", 
              "o3", "so2", "co", "pm10", "pm2.5", "no2", 
              "WS", "WD", "TEMP", "BLH", "PREC", 
              "StCVDcancer60")]
df1 <- df1[complete.cases(df1),]

str(df1)
# saveRDS(df0, "UKdataset2Learn.rds")
```

Using bnlearn, test one-to-one dependecy with Pearson's correlation test (X2):
```{r}
ci.test("pm10", "TEMP", test="cor", data=df0)
ci.test("pm10", "PREC", test="cor", data=df0)
ci.test("pm10", "WS", test="cor", data=df0)
ci.test("pm10", "WD", test="cor", data=df0)
ci.test("pm10", "BLH", test="cor", data=df0)
```

Let's test the one-to-two hypothesis with Pearson's correlation test (X2):
```{r}
ci.test("pm10", "TEMP", "PREC", test="cor", data=df0)
```

# 
Use NO expert knowledge to define a Direct Acyclic Graph (DAG) using bnlearn.
Note that bnlearn's algorithms only work with complete cases!
  ```{r}
bn.mmhc <- mmhc(df0)
graphviz.plot(bn.mmhc)
# Network score (Bayesian Information Criterion)
score(x = bn.mmhc, data = df0)

saveRDS(bn.mmhc, "~/kehra/data/bn00.rds")
```

Topography cannot be influenced by climate/pollution factors, therefore we generate a black list (topography only) and update the BIC score:
  ```{r}
bl1 <- data.frame(from=c(rep(c("PM10","PM2p5","CO","O3","NO2","SO2","TEMP",
                               "WS", "WD","HMIX","PREC"), times=3),
                         c("Latitude", "Longitude"),
                         c("Latitude", "Z"),
                         c("Z", "Longitude")),
                  to=c(rep(c("Latitude", "Longitude", "Z"),each=11),
                       rep(c("Z", "Longitude", "Latitude"), each=2)))
bn.mmhc <- mmhc(df1, blacklist = bl1)
graphviz.plot(bn.mmhc)
score(x = bn.mmhc, data = df1, type = "bic-g")
```

Bayesian Networks do not allow for cyclic dependencies, therefore we assume that Climate affects Pollution and not viceversa. Therefore we generate black/white lists and update the BIC score:
  ```{r}
# black list
bl1 <- data.frame(from=c(rep("Latitude",2), rep("Longitude",2), rep("Z", 2),
                         rep(c("TEMP", "PREC", "WS", "WD", "HMIX",
                               "PM10", "PM2p5","CO", "O3", "NO2", "SO2"),3),
                         "WD", "WS", "PREC", "PM10", "PM10", "PM2p5", "PM2p5"),
                  to=c(c("Longitude", "Z"), c("Latitude", "Z"),
                       c("Longitude", "Latitude"),
                       rep("Latitude",11), rep("Longitude",11), rep("Z",11),
                       "WS", "WD", "WD","WS", "PM2p5", "PM10", "WD"))
# white list
wl1 <- data.frame(from=c("Z", "WS", "WS", "WD"),
                  to=c("TEMP", "PREC", "HMIX", "PM10"))

bn.mmhc <- mmhc(df1, whitelist = wl1, blacklist = bl1)
graphviz.plot(bn.mmhc)
score(x = bn.mmhc, data = df1, type = "bic-g")
```

## Study the conditional indenpendency
```{r}
# dseparation
nano <- nodes(bn.mmhc)
indNodes <- c()
for (n1 in nano){
  for (n2 in nano){
    if (dsep(bn.mmhc, n1, n2)){
      temp <- paste(n1, "and", n2, "are independent")
      indNodes <- c(indNodes, temp)
    }
  }
}
# What happens is I fix Temp?
nano <- nodes(bn.mmhc)
indNodes2 <- c()
fixedVar <- "WD" # "PREC" # "TEMP"
for (n1 in nano[nano != fixedVar]){
  for (n2 in nano[nano != fixedVar]){
    if (dsep(bn.mmhc, n1, n2)){
      temp <- paste(n1, "and", n2, "are independent")
      indNodes2 <- c(indNodes2, temp)
    }
  }
}

setdiff(indNodes, indNodes2)
setdiff(indNodes2, indNodes)
```

### TRAINING
Find local distributions and their parameters.
```{r}
fittedbn <- bn.fit(bn.mmhc, data = df0)
print(fittedbn)
print(fittedbn$pm10)
```

Note difference between fitted parameters for complete cases and those calculated using all observations!
  
  ```{r}
fittedbn$pm10
lm(pm10 ~ Time + latitude + wd + pm2.5, data=df0)               
confint(lm(pm10 ~ Time + latitude + wd + pm2.5, data=df0))

lm(pm10 ~ Time + latitude + wd + pm2.5, data=df)         # lm can handle NAs!  
confint(lm(pm10 ~ Time + latitude + wd + pm2.5, data=df)) 

quantile(df0$pm10, na.rm = TRUE) # using observed CO and WD
quantile(predict(lm(pm10 ~ Time + latitude + wd + pm2.5, data=df0)))     # using observed vaues
```

# INFERENCE

The first column is the vector of marginal expectations, the second is the marginal standard deviation and the remaining is the correlation matrix.

E.g. What values of SO2 should I expect if PM10 raises above EU limits?
```{r}
fittedbn$pm10
fittedbn$so2
fittedbn$CVD60

# observed PM10 and others
quantile(df0$pm10) # quantile(df$PM10, na.rm = TRUE)
quantile(df0$so2)
quantile(df0$wd)

# how is the model doing trying to predict PM10 knowing the appropriate range for SO2?
newPM10 <- cpdist(fittedbn, nodes = "pm10", evidence = (so2 < 300))
quantile(newPM10[,1])

# how is the model doing trying to predict PM10 knowing the appropriate range for LAT?
newPM10 <- cpdist(fittedbn, nodes = "pm10", evidence = (latitude < 54))
quantile(newPM10[,1])

# What is the probability of SO2 > 300, if PM10 > 50?
cpquery(fittedbn, event = (so2 > 300), evidence = (pm10 > 50))
cpquery(fittedbn, event = (so2 > 300), evidence = (pm10 < 50))

cpquery(fittedbn, event = (so2 > 300), evidence = (pm2.5 < 40))
cpquery(fittedbn, event = (so2 > 300), evidence = (pm2.5 > 40))

quantile(df0$CVD60)
cpquery(fittedbn, event = (CVD60 > 10), evidence = (pm2.5 > 40))


# load parallel and bnlearn and rsprng.
library(parallel)
library(bnlearn)
cl = makeCluster(detectCores() - 2)
# check it works.
clusterEvalQ(cl, runif(10))

# load the data.
df <- readRDS("~/kehra/data/EnglandDB_1979_2014.rds")
df.bn <- df[complete.cases(df), ]

# call a learning function passing the cluster object (the
# return value of the previous makeCluster() call) as a
# parameter.
res = gs(df.bn, cluster = cl)
# note that the number of test is evenly divided between
# the two nodes of the cluster.
clusterEvalQ(cl, test.counter())

# a few tests are still executed by this process.
(test.counter())

# stop the cluster.
stopCluster(cl)
