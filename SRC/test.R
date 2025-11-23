#Loading required packages
library(sf)
library(dplyr)
library(SpatialRDD)

#Reading cutoff boundary shapefile for 2018.
cutoff_boundary_2018 <- st_read("DATA/Shapefiles/S2.shp")

borderpoints.sf <- discretise_border(cutoff = cutoff_boundary_2018, n=50)
borderpoints.sf$id <- 1:nrow(borderpoints.sf)

#Reading final dataset.
df <- st_read("DATA/Shapefiles/S5.shp")

results <- spatialrd(y = "rt_2018", data = df, cutoff.points = borderpoints.sf, treated = "LWE2018", minobs = 10, spatial.object = F)
knitr::kable(results)
