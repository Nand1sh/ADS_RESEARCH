#Loading required packages
library(sf)
library(dplyr)

#Reading master subdistricts shapefile.
subdistricts <- st_read("DATA/Shapefiles/S1.shp")

#Reading cutoff boundary shapefile for 2018.
cutoff_boundary_2018 <- st_read("DATA/Shapefiles/S2.shp")
cutoff_boundary_2021 <- st_read("DATA/Shapefiles/S3.shp")

#Filtering master shapefile for relevant 11 states.
subdistricts <- subdistricts[subdistricts$STATE %in% c("ANDHRA PRADESH", "BIHAR",
                                                       "CHHATISGARH", "JHARKHAND",
                                                       "MADHYA PRADESH","MAHARASHTRA",
                                                       "ODISHA", "TELANGANA",
                                                       "UTTARPRADESH", "WEST BENGAL",
	                                               "KERALA"), ]

#Extracting centroids of subdistrict polygons.
subdistricts_centroid <- st_centroid(subdistricts)

#Calculating distance to cutoff from centroids.
distance_to_cutoff_2018 <- st_distance(subdistricts_centroid, cutoff_boundary_2018)
distance_to_cutoff_2021 <- st_distance(subdistricts_centroid, cutoff_boundary_2021)

#Adding distance to cutoff to the master shapefile.
subdistricts$d2c_18 <- distance_to_cutoff_2018
subdistricts$d2c_21 <- distance_to_cutoff_2021

#Making distances positive for treated subdistricts and negative for control.
subdistricts <- subdistricts %>% mutate(d2c_18 = ifelse(LWE2018 == 0, -1*d2c_18, d2c_18))
subdistricts <- subdistricts %>% mutate(d2c_21 = ifelse(LWE2021 == 0, -1*d2c_21, d2c_21))

#Writing the shapefile with distances.
st_write(subdistricts, "DATA/Shapefiles/S4.shp")
