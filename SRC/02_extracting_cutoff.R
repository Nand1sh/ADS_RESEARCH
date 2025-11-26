#Loading required packages.
library(sf)

#Reading subdisticts shapefile with treatment status.
subdistricts <- st_read("DATA/Shapefiles/S1.shp")  

#Filtering for treated subdistricts.
subdistricts_ads <- subdistricts[subdistricts$ADS == 1, ]

#Unionizing treated subdistricts to get treatment polygon.
ads_treated_area <- st_union(subdistricts_ads$geometry)

#Extracting boundary of the unionized treatment polygon.
ads_treated_boundary <- st_boundary(ads_treated_area)

#The following chunk is commented out and should not be run.
#This is because some manual cleaning is required to this shapefile
#in QGIS. I have done it already and it can be read in the next
#R script without issues.

#Writing the boundary cutoff as a shapefile.
#st_write(ads_treated_boundary, "DATA/Shapefiles/S2.shp")
