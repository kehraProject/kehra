################################################################################
# LEARN THE BAYESIAN NETWORK USING PCALG (CAUSAL RELATIONSHIPS) ################
################################################################################

# Install dependencies from the Bioconductor and CRAN repos
# For PC algorithm
# sudo apt-get install libgmp3-dev
# install.packages("gmp")
# install.packages("pcalg")
library(pcalg)
require(Rgraphviz)

# suffStat <- list(C = cor(df, use="complete.obs"), n = nrow(df))
suffStat <- list(C = cor(df00), n = nrow(df00))
pc.fit0 <- fci(suffStat,
               indepTest = gaussCItest, ## indep.test: partial correlations
               alpha=0.005, labels = names(df00))
plot(pc.fit0, main = "Estimated CPDAG")

# export pc.fit to bnlearn
res <- as.bn(pc.fit0@graph)

library(igraph)
library(networkD3)

x <- igraph.from.graphNEL(as.graphNEL(res))

# Convert to object suitable for networkD3
net <- igraph_to_networkD3(x)
net$nodes$group <- c(1,1,1,1,2,2,2,2,3,3,4,4,4,4,4,4,3,3,3,5,5)
net$nodes$col <- c("yellow","yellow","yellow","yellow",
                   "brown","brown","brown","brown",
                   "blue","blue","pink","pink","pink","pink",
                   "pink","pink","blue","blue","blue","red","red")

# Create force directed network plot
forceNetwork(Links = net$links, Nodes = net$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group')

src <- as.character(net$nodes$name[net$links$source + 1])
tgt <- as.character(net$nodes$name[net$links$target + 1])
simpleNetwork(Data = data.frame(src, tgt), fontSize = 16)

pp <- graphviz.plot(x = res, highlight = list(nodes = nodes(res),
                                              arcs = arcs(res),
                                              col = "grey",
                                              textCol = "black"))
nodeRenderInfo(pp) <- list(col = c(),
                           textCol = c(),
                           fill = c("Y" = "grey", "M" = "grey", 
                                    "D" = "grey", "T" = "grey",
                                    "LAT" = "yellow", "LON" = "yellow",
                                    "ALT" = "yellow",
                                    "WS" = "pink", "WD" = "pink", 
                                    "TP" = "pink", "BLH" = "pink",
                                    "R" = "pink",
                                    "RID" = "orange",
                                    "O3" = "blue", "SO2" = "blue",
                                    "CO" = "blue", "PM10" = "blue",
                                    "PM25" = "blue", "NO2" = "blue",
                                    "C60" = "red", "L60" = "red"),
                           fontsize = 20)

# CLEAN UP EDGES
# TIME
pp@edgeL$Y$edges <- c(9, 13, 20)
pp@edgeL$D$edges <- c(9, 10, 11, 15, 17, 19)
pp@edgeL$M$edges <- c(11, 14, 17, 19)
pp@edgeL$T$edges
# GEO
pp@edgeL$LAT$edges <- c(13, 21)
pp@edgeL$LON$edges <- c(11, 16, 17, 18, 19, 20, 21)
pp@edgeL$ALT$edges
pp@edgeL$RID$edges <- c(9, 10, 16, 17, 21)
# HEALTH
pp@edgeL$C60$edges <- integer(0)
pp@edgeL$L60$edges <- integer(0)

renderGraph(pp)

edgeRenderInfo(pp) <- list(lty = c())

## show estimated CPDAG
library(igraph)
graphviz.plot(res)
plot(pc.fit0, main = "Estimated CPDAG", frame=TRUE, vertex.size=50)


# check and modify node ordering
undirected.arcs(res)
node.ordering(res)
ordering2blacklist(res)
res <- drop.arc(res, from = "latitude", to = "longitude")
res <- drop.arc(res, "latitude", "altitude")
res <- drop.arc(res, "LIV60", "CVD60")
ord = node.ordering(res)

bn.gs <- gs(df00[complete.cases(df00),], blacklist = ordering2blacklist(ord))
graphviz.plot(bn.gs)
score(x = bn.gs, data = df00, type = "bic-g")

node.ordering(res) <- c("LAT", "LON", "A", "so2", "o3", "co", "pm10", "pm2.5",
                        "no2", "TEMP", "WS", "WD", "BLH", "PREC", "C60", "L60")

# TEST BNLEARN, regardless of PC results
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

# Using bnlearn, test one-to-one dependecy with Pearson's correlation test (X2):
ci.test("pm10", "TEMP", test="cor", data=df0)
ci.test("pm10", "PREC", test="cor", data=df0)
ci.test("pm10", "WS", test="cor", data=df0)
ci.test("pm10", "WD", test="cor", data=df0)
ci.test("pm10", "BLH", test="cor", data=df0)

# Let's test the one-to-two hypothesis with Pearson's correlation test (X2):
ci.test("pm10", "TEMP", "PREC", test="cor", data=df0)

# Use NO expert knowledge to define a Direct Acyclic Graph (DAG) using bnlearn.
# Note that bnlearn's algorithms only work with complete cases!
bn.mmhc <- mmhc(df0)
graphviz.plot(bn.mmhc)
# Network score (Bayesian Information Criterion)
score(x = bn.mmhc, data = df0)

saveRDS(bn.mmhc, "~/kehra/data/bn00.rds")

# Topography cannot be influenced by climate/pollution factors, therefore we generate a black list (topography only) and update the BIC score:
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

# Bayesian Networks do not allow for cyclic dependencies, therefore we assume that Climate affects Pollution and not viceversa. Therefore we generate black/white lists and update the BIC score:
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

## Study the conditional indenpendency
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

### TRAINING
# Find local distributions and their parameters.
fittedbn <- bn.fit(bn.mmhc, data = df0)
print(fittedbn)
print(fittedbn$pm10)

# Note difference between fitted parameters for complete cases and those calculated using all observations!
fittedbn$pm10
lm(pm10 ~ Time + latitude + wd + pm2.5, data=df0)               
confint(lm(pm10 ~ Time + latitude + wd + pm2.5, data=df0))

lm(pm10 ~ Time + latitude + wd + pm2.5, data=df)         # lm can handle NAs!  
confint(lm(pm10 ~ Time + latitude + wd + pm2.5, data=df)) 

quantile(df0$pm10, na.rm = TRUE) # using observed CO and WD
quantile(predict(lm(pm10 ~ Time + latitude + wd + pm2.5, data=df0)))     # using observed vaues

# INFERENCE
# The first column is the vector of marginal expectations, the second is the marginal standard deviation and the remaining is the correlation matrix.
# E.g. What values of SO2 should I expect if PM10 raises above EU limits?
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
