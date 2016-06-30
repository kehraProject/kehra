library(raster)

Altitude_GRID <- as.matrix(raster("data/GEO/UK/Topography/GBR_alt.gri"))

saveRDS(Altitude_GRID, "data/GEO/UK/Topography/GBR_alt.rds")
