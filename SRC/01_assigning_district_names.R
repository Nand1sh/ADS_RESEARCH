#Loading required packages
library(sf)
library(dplyr)

#Reading master subdistricts and districts shapefiles from SHRUG
df_subdistrict <- st_read("DATA/SHRUG/subdistrict.shp")
df_district <- st_read("DATA/SHRUG/district.shp")

# Assigning district names
df_subdistrict <- df_subdistrict %>%
  left_join(
    df_district %>% st_drop_geometry() %>% select(pc11_d_id, d_name),
    by = "pc11_d_id"
  )

#Writing subdistrict level shapefile with district names
st_write(df_subdistrict, "DATA/Shapefiles/S0.shp")
