#Loading required packages
library(sf)
library(dplyr)

#Reading master subdistricts shapefile.
subdistricts <- st_read("DATA/Shapefiles/S1.shp")

#Reading cutoff boundary shapefile
cutoff_boundary <- st_read("DATA/Shapefiles/S2.shp")

#Filtering master shapefile for relevant 26 states.
subdistricts <- subdistricts[subdistricts$STATE %in% c("HIMACHAL PRADESH", "KARNATAKA", "KERALA", "ANDHRA PRADESH", "TELANGANA", "UTTARPRADESH", "UTTARAKHAND", "RAJASTHAN", "JHARKHAND", "CHHATISGARH", "MADHYA PRADESH", "MEGHALAYA", "NAGALAND", "TRIPURA", "MANIPUR", "MIZORAM", "ARUNACHAL PRADESH", "MAHARASHTRA", "BIHAR", "GUJARAT", "PUNJAB", "HARYANA", "ODISHA", "TAMIL NADU", "JAMMU AND KASHMIR", "ASSAM"), ]

#Extracting centroids of subdistrict polygons.
subdistricts_centroid <- st_centroid(subdistricts)

#Calculating distance to cutoff from centroids.
distance_to_cutoff <- st_distance(subdistricts_centroid, cutoff_boundary)

#Adding distance to cutoff to the master shapefile.
subdistricts$d2c <- distance_to_cutoff

#Making distances positive for treated subdistricts and negative for control.
subdistricts <- subdistricts %>% mutate(d2c = ifelse(ADS == 0, -1*d2c, d2c))

#Writing the shapefile with distances.
st_write(subdistricts, "DATA/Shapefiles/S4.shp")
