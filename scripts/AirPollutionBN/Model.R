################################################################################
# LEARN THE BAYESIAN NETWORK USING BNLEARN #####################################
################################################################################

# For bnlearn
# Bioconductor
# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")
# biocLite("RBGL")
# biocLite("Rgraphviz")

# CRAN
# install.packages(c("Matrix", "igraph", "Rcpp"))
# install.packages("gRbase")
# install.packages("gRain")
# install.packages("bnlearn")
library(devtools)
install_github("cvitolo/bnlearn")
library(bnlearn)

# Load the training set
df <- readRDS("~/data/training.rds")
as.data.frame(names(df))
df <- df[,c(2:24,28)]
# Check data structure
str(df)
# which colum does not contain NA?
colSums(is.na(df)) == 0
# remove NA from weather variables
df <- df[-which(is.na(df$ws)),]
# remove NA from health variables
df <- df[-which(is.na(df$CVD60)),]
# Re-order so that CVD is first and columns are ordered based on descending 
# number of missing values
df <- droplevels(df[,c("Region", "Zone", "Type", "Year", "Season", "Month", 
                       "Day", "Hour", "Latitude", "Longitude", "Altitude", 
                       "CVD60", "t2m", "ws", "wd", "tp", "blh", "ssr", 
                       "no2", "o3", "so2", "co", "pm10", "pm2.5")])

# Imputing Missing Data With Expectation – Maximization
# initialise using the empty graph and complete observations.
library(data.table)
dfc <- data.table(df[complete.cases(df),])

dag <- empty.graph(names(dfc)); graphviz.plot(dag)
bn <- bn.fit(dag, dfc)

current <- df
for (i in 19:24) {
  
  colName <- names(current)[i]
  colValues <- eval(parse(text = paste("current$", colName, sep = ""))) 
  
  # E: replacing missing values with their (E)xpectation.
  current[is.na(colValues), colName] <- predict(object = bn, 
                                                node = colName, 
                                                data = current[is.na(colValues),
                                                               names(current)[1:(i-1)]],
                                                method = "bayes-lw")
  
  # M: learning the model that (M)aximises the score with the current data.
  dag <- mmhc(current[complete.cases(current),])
  bn <- bn.fit(dag, current[complete.cases(current),])
  
  # graphviz.plot(dag)
  readline(prompt="Press [enter] to continue")
  
}


# Define blacklist
bl <- data.frame("from" = c(rep("Region",10),
                            rep("Zone",10),
                            rep("Type",10),
                            rep("Year",10),
                            rep("Season",10),
                            rep("Month",10),
                            rep("Day",10),
                            rep("Hour",10),
                            rep("Latitude",10),
                            rep("Longitude",10),
                            rep("Altitude",10)), 
                 "to" = c("Zone", "Type", "Year", "Season", "Month", "Day", "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Type", "Year", "Season", "Month", "Day", "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Year", "Season", "Month", "Day", "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Season", "Month", "Day", "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Month", "Day", "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Day", "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Month", "Hour", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Month", "Day", "Latitude", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Month", "Day", "Hour", "Longitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Month", "Day", "Hour", "Latitude", "Altitude",
                          "Region", "Zone", "Type", "Year", "Season", "Month", "Day", "Hour", "Latitude", "Longitude"))

dag <- mmhc(df00, blacklist = bl, alpha = 0.05)
any(is.null(df00))
unique(df00$Longitude)

graphviz.plot(dag, highlight = NULL, layout = "dot",
              shape = "circle", main = NULL, sub = NULL)


