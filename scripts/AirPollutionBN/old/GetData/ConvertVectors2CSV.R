# Convert vector to csv 
library(ggplot2)

x <- readRDS("adm0.rds")
polygon_dataframe <- fortify( x )
write.csv(polygon_dataframe,"adm0.csv")

x <- readRDS("adm1.rds")
polygon_dataframe <- fortify( x )
write.csv(polygon_dataframe,"adm1.csv")

x <- readRDS("adm2.rds")
polygon_dataframe <- fortify( x )
write.csv(polygon_dataframe,"adm2.csv")


