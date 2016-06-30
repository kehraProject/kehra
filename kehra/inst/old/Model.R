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
install.packages("bnlearn")
# temporary patches:
# library(devtools)
# install_github("cvitolo/bnlearn")

# load parallel and bnlearn and rsprng.
# library(parallel)
# cl <- makeCluster(detectCores()-1)
# check it works.
# clusterEvalQ(cl, runif(10))

library(bnlearn)
# Load the training set
df <- readRDS("~/data/training.rds") # as.data.frame(names(df))
df <- df[,c(2:24,28)] # Check data structure with str(df)
# which colum does not contain NA? colSums(is.na(df)) == 0
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

# Define blacklist to apply to future changes in bn
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

# Imputing Missing Data With Expectation – Maximization
# initialise using the empty graph and complete observations.
firstCompleteObservations <- which(complete.cases(df))
dfc <- df[firstCompleteObservations, ]

dag <- empty.graph(names(dfc))
bn <- bn.fit(dag, dfc)                                       # node.ordering(bn)

# Iterate and check for convergence tracking the likelihood of the dag 
# with regards to the original complete observations
current <- df
# keep track of the log-Likelihood
logLikelihood <- logLik(object = dag, 
                        data = current[firstCompleteObservations, ])
for (j in 1:10){ # check convergence at each loop # j <- 1
  
  for (i in 24:19) { # i <- 24
    
    # what is the name of the column to fill in?
    colName <- names(current)[i]
    print(colName)
    # extract the set of complete observations (ignoring only column i)
    completeRows <- which(complete.cases(current[,-i]))
    dfc <- current[completeRows, ]
    # within the above table, what are the values of column i?
    colValues <- eval(parse(text = paste("current$", colName, 
                                         "[completeRows]", sep = "")))
    # in what rows we have NA?
    naRows <- which(is.na(colValues))
    
    # If there are columns in which the non-zero-probability levels do not
    # contain
    col2remove <- c()
    for (nCol in names(which(sapply(dfc, class) == 'factor'))){
      stringCol <- paste("bn$", nCol, "$prob", sep = "")
      levelsCol <- names(which( eval(parse(text = stringCol)) != 0 ))
      if (!all(unique(current[,nCol]) %in% levelsCol)) {
        # if (length(levelsCol) <= 1) {
        col2remove <- c(col2remove, nCol) 
      }
    }
    col2remove <- which(names(current) %in% col2remove)
    
    # E: replacing missing values with their (E)xpectation
    E <- predict(object = bn, 
                 node = colName, 
                 data = dfc[,-c(col2remove,i)],
                 method = "bayes-lw")
    # measured <- current[completeRows[-naRows], colName]
    # modelled <- E[-naRows]
    # rmse <- c(rmse, sqrt( mean( (modelled-measured)^2 , na.rm = TRUE ) ))
    current[completeRows[naRows], colName] <- E[naRows]
    
    # M: learning the model that (M)aximises the score with the current data.
    dfc <- current[complete.cases(current),]
    # call a learning function passing the cluster object (the return value of the 
    # previous makeCluster() call) as a parameter.
    dag <- hc(dfc, blacklist = bl, debug = TRUE)
    bn <- bn.fit(dag, dfc, debug = TRUE)
    
    graphviz.plot(dag)
    readline(prompt="Press [enter] to continue") 
    
  }
  
  # keep track of the log-Likelihood at each complete round of iteration
  logLikelihood <- c(logLikelihood, 
                     logLik(object = dag, 
                            data = current[firstCompleteObservations, ]))
  
}

# stop the cluster.
# stopCluster(cl)

graphviz.plot(dag, highlight = NULL, layout = "dot",
              shape = "circle", main = NULL, sub = NULL)
