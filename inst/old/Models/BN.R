## Install dependencies from the Bioconductor repo
source("https://bioconductor.org/biocLite.R")
biocLite("graph")
biocLite("RBGL")
biocLite("Rgraphviz")
n
## Install dependencies from the CRAN repo
install.packages(c("Matrix", "igraph", "Rcpp"))
install.packages("gRbase")
install.packages("gRain")

install.packages("bnlearn")
library(bnlearn)

df <- readRDS("~/Dropbox/Projects/kehra/data/Pollution/allPollutantsClima.rds")
names(df)
# "Date", "id", "Longitude", "Latitude", "Site", "Region", "Z",
# "PM10", "PM2p5", "CO", "O3", "NO2", "SO2", "TEMP", "WS", "WD", "HMIX", "PREC"
head(df)

df1 <- na.omit(df[,c("Longitude", "Latitude", "Z",
                     "PM10", "PM2p5", "CO", "O3", "NO2", "SO2", 
                     "TEMP", "WS", "WD", "HMIX", "PREC")])

# There are some NAs in columns 9:13, remove rows with all NAs
temp <- df[,c("PM10", "PM2p5", "CO", "O3", "NO2", "SO2")]
df2 <- df[rowSums(is.na(temp)) != ncol(temp),]
# Keep only complete cases (rows with no NAs)
df3 <- df[complete.cases(temp),]

################################################################################
# Check whether PM10 depends on TEMP and PREC
df0 <- df[complete.cases(df[,c("PM10", "TEMP", "PREC")]), 
          c("PM10", "TEMP", "WS", "WD", "HMIX", "PREC")]

# Partial correlation can be estimated numerically
# First, compute the correlation matrix
cormat <- cor(df0)

library(corpcor)
# compute the inverse of the correlation matrix
invcor <- cor2pcor(cormat)
dimnames(invcor) <- dimnames(cormat)

# Let's test one-to-one dependecy with Pearson's correlation test (X2):
ci.test("PM10", "TEMP", test="cor", data=df0)
ci.test("PM10", "PREC", test="cor", data=df0)
ci.test("PM10", "WS", test="cor", data=df0)
ci.test("PM10", "WD", test="cor", data=df0)
ci.test("PM10", "HMIX", test="cor", data=df0)

# Let's test the one-to-two hypothesis with Pearson's correlation test (X2):
ci.test("PM10", "TEMP", "PREC", test="cor", data=df0)

################################################################################
library(openair)
corPlot(df, dendrogram = TRUE)
################################################################################
# Use expert knowledge to define a Direct Acyclic Graph (DAG) using bnlearn
# Note that bnlearn's algorithms only work with complete cases!
bn.mmhc <- mmhc(df1)
graphviz.plot(bn.mmhc)

# Bayesian Networks do not allow for cyclic dependencies, therefore we assume that Climate affects Pollution and non viceversa.

# black list (topography only)
bl1 <- data.frame(from=c(rep(c("PM10","PM2p5","CO","O3","NO2","SO2","TEMP","WS",
                             "WD","HMIX","PREC"),times=3),
                         c("Latitude", "Longitude"),
                         c("Latitude", "Z"),
                         c("Z", "Longitude")),
                  to=c(rep(c("Latitude", "Longitude", "Z"),each=11),
                       rep(c("Z", "Longitude", "Latitude"), each=2)))
bn.mmhc <- mmhc(df1, blacklist = bl1)
graphviz.plot(bn.mmhc)

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

## Study the conditional indenpendency (dseparation)
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

### TRAINING (find local distributions and their parameters)
fittedbn <- bn.fit(bn.mmhc, data = df1)
print(fittedbn$PM10)
print(fittedbn$PM10)

## RBMN package
library(rbmn)
gbn.rbmn <- bnfit2nbn(fittedbn)
gema.rbmn <- nbn2gema(gbn.rbmn)
mn.rbmn <- gema2mn(gema.rbmn)
print8mn(mn.rbmn)

est.para <- bn.fit(bn.mmhc, data = df1)
est.para$PM10
lm(PM10 ~ CO + WD, data=df1)

################################################################################
### SPATIAL MODELLING (YEARLY DEATHS BY LSOA)
library(maptools)
# read the shapefile with LSOA and yearly deaths
ewlsoa <- readShapePoly("~/Dropbox/Projects/kehra/data/GEO/UK/AdminBoundaries/UK_ONSboundaries/LSOAyearlyDeaths.shp")

# transform the shapefile into an adjacency matrix
library(spdep)
# specify the neighborhood structure
temp <- poly2nb(ewlsoa)
EW.adj <- "~/Dropbox/Projects/kehra/data/GEO/UK/AdminBoundaries/UK_ONSboundaries/EW.graph"
nb2INLA(EW.adj, temp)

# import the graph in INLA
# H <- inla.read.graph(filename=EW.adj)
# Too big! Do not plot! image(inla.graph2matrix(H), xlab="", ylab="")

# specify the formula
formula <- y ~ 1 + f(LSOA11CD, model="bym", graph=EW.adj)

# Run model
d <- read.csv("~/Dropbox/Projects/kehra/data/GEO/UK/AdminBoundaries/UK_ONSboundaries/Lower_layer_super_output_areas_YearlyDeaths.csv")
library(INLA)
d2 <- d[,c(2,35)]
mod.lsoadeaths <- inla(formula, family="poisson", data = d2, E="X2013",
                       control.compute = list(dic=TRUE))

### SPATIO-TEMPORAL BAYESIAN MODEL
# order the data.frame as in the example in chapter 7
df <- df[with(df, order(Date, id)), ]

n_stations <- length(unique(df$id))    # 104 stations
n_data <- dim(df)[1]                   # 37960 space-time data
n_days <- n_data/n_stations            # 365 time points

df$time <- rep(1:n_days, each=n_stations)
coordinates.allyear <- as.matrix(coordinates[Piemonte_data$Station.ID,
                                             c("UTMX","UTMY")])

################################################################################

library(bnlearn)
# bnlearn only works with complete cases!

#aurnDataAll <- readRDS("data/Pollution/UKAIR/EnglandAURNdata.rds")  # 18Millions
aurnDataAll <- readRDS("data/Pollution/UKAIR/EnglandAURNdata1998_2013.rds")  # 13Millions
aurnData <- aurnDataAll[complete.cases(aurnDataAll[,c("site", "co","pm10","pm2.5","no2","so2","o3","ws","wd")]),c("site","co","pm10","pm2.5","no2","so2","o3","ws","wd")]       # 237K

aurnData$Latitude <- NA
aurnData$Longitude <- NA
aurnData$Altitude <- NA

# add coordinates
for (i in 1:length(unique(aurnData$site))){
  
  Name <- as.character(unique(aurnData$site))[i]
  myRow <- try(which(allStations$Site.Name %in% Name), silent=TRUE)
  
  # if(inherits(myRow,"try-error")) { next } # only true if an error occurs
  if(length(myRow)==0) { next }
  
  aurnData$Latitude[aurnData$site == Name] <- allStations$Latitude[myRow]
  aurnData$Longitude[aurnData$site == Name] <- allStations$Longitude[myRow]
  aurnData$Altitude[aurnData$site == Name] <- allStations$Altitude..m.[myRow]
  
}

corPlot(aurnData[,-1], dendrogram = TRUE)

bn.mmhc <- mmhc(aurnData) #no prior knowledge applied
graphviz.plot(bn.mmhc)
