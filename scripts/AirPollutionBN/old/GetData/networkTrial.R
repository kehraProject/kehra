library(networkD3)

names(df00)[pp@edgeL$LON$edges]

# Create fake data
src <- c("Y", "Y", "Y",
         "M", "M", "M", "M",
         "D", "D", "D", "D", "D", "D", 
         "T", "T", "T", 
         "RID", "RID", "RID", "RID", "RID", 
         "LAT", "LAT",
         "LON", "LON", "LON", "LON", "LON", "LON", "LON",
         "ALT", "ALT", "ALT",
         "WS", "WS", "WS", "WS", "WS",
         "WD", "WD", "WD", "WD", "WD", "WD", "WD", "WD", 
         "O3", "O3", "O3", "O3", "O3", "O3",
         "SO2", "SO2", "SO2", "SO2",
         "CO", "CO", "CO",
         "PM10", "PM10", "PM10",
         "PM25", "PM25",
         "NO2", 
         "TP", "TP", "TP",
         "BLH", 
         "R")
target <- c("WS", "CO", "C60",
            "O3", "PM10", "TP", "R",
            "WS", "WD", "O3", "PM25", "TP", "R",
            "WS", "CO", "NO2",
            "WS", "WD", "NO2", "TP", "L60",
            "CO", "L60",
            "O3", "NO2", "TP", "BLH", "R", "C60", "L60",
            "O3", "SO2", "L60",
            "O3", "PM10", "NO2", "BLH", "R",
            "CO", "PM10", "PM25", "NO2", "TP", "BLH", "R", "C60",
            "SO2", "PM25", "NO2", "TP", "BLH", "L60",
            "CO", "PM10", "PM25", "NO2",
            "PM10", "PM25", "NO2",
            "PM25", "BLH", "R",
            "NO2", "BLH",
            "PM10",
            "BLH", "R", "C60",
            "R",
            "L60")
networkData <- data.frame(src, target)

# Plot
simpleNetwork(networkData, fontSize = 18, nodeColour = list("Y" = "yellow", "R" = "red", "C60" = "red", "L60" = "red"))
