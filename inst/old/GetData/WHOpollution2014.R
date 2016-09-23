library(googleVis)
library(maps)

whoPollution2014 <- world.cities

whoPollution2014$PM10 <- whoPollution2014$PM2p5 <- NA

x <- read.csv("~/Dropbox/Projects/kehra/data/GEO/AllCountries/AAP_PM_database_May2014_cities.csv")

for (j in 1:dim(x)[1]){
  
  i <- unlist(strsplit( as.character(x$City.station[j]), split=" "))[1]
  if ( i %in% whoPollution2014$name ){
    whoPollution2014$PM10[j] <- as.numeric(as.character(x$PM10..Annual.mean..ug.m3.[j]))
    whoPollution2014$PM2p5[j] <- as.numeric(as.character(x$PM.2.5..Annual.mean..ug.m3.[j]))
  }else{
    print(paste(j, i, "not found!"))
  }
  
}

write.csv(whoPollution2014, "data/GEO/AllCountries/whoPollutedCities2014.csv")

# Different colors and add regression lines
ggplot(x, aes(x=PM10..Annual.mean..ug.m3., y=PM.2.5..Annual.mean..ug.m3., color=Region)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE) +   # Don't add shaded confidence region
  geom_vline(xintercept = 40, col="red") +
  geom_hline(yintercept = 25, col="red")

x2 <- x[x$Country=="Kazakhstan" | x$Country=="United Kingdom",]
ggplot(x2, aes(x=PM10..Annual.mean..ug.m3., y=PM.2.5..Annual.mean..ug.m3., 
               color=Country)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE) +   # Don't add shaded confidence region
  geom_vline(xintercept = 40, col="red") +
  geom_hline(yintercept = 25, col="red")
