# Load the training set.
df <- readRDS("~/kehra/data/training.rds")

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
library(bnlearn)

# Load the training set
df <- readRDS("~/kehra/data/training.rds")
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
# Re-order so that CVD is first and I only have one column with missing values
df00 <- df[,c(1:11, 24, 12:18)]

# Imputing Missing Data With Expectation – Maximization
# initialise using the empty graph and complete observations.
df00c <- droplevels(df00[complete.cases(df00),])

x <- df00c[,c(1:5)]

check.data(x)

dag <- empty.graph(names(x))
graphviz.plot(dag)

bn <- bn.fit(dag, x)

for (i in 1:3) {
  
  # E: replacing missing values with their (E)xpectation.
  current = df00
  current[is.na(current$A), "A"] =
    predict(bn, node = "A", current[is.na(current$A), LETTERS[2:6]],
            method = "bayes-lw")
  
  # M: learning the model that (M)aximises the score with the current data.
  dag = hc(current)
  bn = bn.fit(dag, current)
  
  graphviz.plot(dag)
  
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


