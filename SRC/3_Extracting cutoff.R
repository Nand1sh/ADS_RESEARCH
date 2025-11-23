#Loading required packages.
library(sf)

#Reading subdisticts shapefile with treatment status.
subdistricts <- st_read("DATA/Shapefiles/S1.shp")  

#Filtering for treated subdistricts.
subdistricts_2018 <- subdistricts[subdistricts$LWE2018 == 1, ]
subdistricts_2021 <- subdistricts[subdistricts$LWE2021 == 1, ]

#Unionizing treated subdistricts to get treatment polygon.
lwe_affected_area_2018 <- st_union(subdistricts_2018$geometry)
lwe_affected_area_2021 <- st_union(subdistricts_2021$geometry)

#Extracting boundary of the unionized treatment polygon.
lwe_affected_boundary_2018 <- st_boundary(lwe_affected_area_2018)
lwe_affected_boundary_2021 <- st_boundary(lwe_affected_area_2021)

#The following chunk is commented out and should not be run.
#This is because some manual cleaning is required to this shapefile
#in QGIS. I have done it already and it can be read in the next
#R script without issues.

#Writing the boundary cutoff as a shapefile.
# st_write(lwe_affected_boundary_2018, "DATA/Shapefiles/S2.shp")
# st_write(lwe_affected_boundary_2021, "DATA/Shapefiles/S3.shp")
