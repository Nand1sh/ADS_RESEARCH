#Loading required packages.
library(sf)
library(dplyr)
library(DescTools)
options(scipen=999)

#Reading master shapefile.
subdistricts <- st_read("DATA/Shapefiles/S5.shp")

#Reading controls shapefile.
controls_merged <- st_read("DATA/Shapefiles/S6.shp")

#Creating unique ID.
subdistricts <- subdistricts %>% 
  mutate(ID = row_number())

#Delcaring the proj4 string for merging.
proj4 <- "+proj=lcc +lat_0=24 +lon_0=80 +lat_1=12.472944 +lat_2=35.172806 +x_0=4000000 +y_0=4000000 +datum=WGS84 +units=m +no_defs"

#Extracting centroids of the controls polygons.
controls_merged_centroids <- st_centroid(controls_merged) 

#Transforming the crs of the centroids for merging using proj4.
controls_merged_centroids <- st_transform(controls_merged_centroids, crs = proj4)

#Merging controls to master shapefile but storing separately.
df <- st_join(subdistricts, controls_merged_centroids, join = st_intersects)

#Accounting for geographic discrepancies across shapefiles.
df <- aggregate(cbind(pc11_tot_p, pc11_sc, pc11_st, pc11_lit,
                      pc11_work, pc11_v_p, pc11_fores, pc11_v_a,
                      pc01_tot_p, pc01_sc, pc01_st, pc01_lit,
                      pc01_work, pc01_v_p, pc01_fores, pc01_v_a) ~ ID, data = df, FUN = sum)


#Assigning corrected controls to master shapefile.
df_controls <- merge(subdistricts, df, by = 'ID')

#Calculating % shares for controls.
df_controls <- df_controls %>%
  mutate(pc11_sc_share = (pc11_sc/pc11_tot_p)*100)
df_controls <- df_controls %>%
  mutate(pc11_st_share = (pc11_st/pc11_tot_p)*100)
df_controls <- df_controls %>%
  mutate(pc11_lit_share = (pc11_lit/pc11_tot_p)*100)
df_controls <- df_controls %>%
  mutate(pc11_rural_share = (pc11_v_p/pc11_tot_p)*100)
df_controls <- df_controls %>%
  mutate(pc11_work_share = (pc11_work/pc11_tot_p)*100)
df_controls <- df_controls %>%
  mutate(pc11_forest_share = (pc11_fores/pc11_v_a)*100)
df_controls <- df_controls %>%
  mutate(pc01_sc_share = (pc01_sc/pc01_tot_p)*100)
df_controls <- df_controls %>%
  mutate(pc01_st_share = (pc01_st/pc01_tot_p)*100)
df_controls <- df_controls %>%
  mutate(pc01_lit_share = (pc01_lit/pc01_tot_p)*100)
df_controls <- df_controls %>%
  mutate(pc01_rural_share = (pc01_v_p/pc01_tot_p)*100)
df_controls <- df_controls %>%
  mutate(pc01_work_share = (pc01_work/pc01_tot_p)*100)
df_controls <- df_controls %>%
  mutate(pc01_forest_share = (pc01_fores/pc01_v_a)*100)

#Replacing NAs with 0s when applicable.
df_controls$pc11_forest_share[is.na(df_controls$pc11_forest_share)] <- 0
df_controls$pc01_forest_share[is.na(df_controls$pc01_forest_share)] <- 0

#Interpolating growth rate between 01 and 11 and then extrapolating for 18.
for (control in c("sc_share", "st_share", "lit_share",
                  "rural_share", "work_share", "forest_share")) {
  df_controls <- df_controls %>%
    mutate(!!paste0("pc18_", control) := !!sym(paste0("pc11_", control)) * ((1 + (((!!sym(paste0("pc11_", control)) - !!sym(paste0("pc01_", control))) / (!!sym(paste0("pc01_", control)))) * 10) / 100) ^ 7))
}

#Replacing NAs with 0s for 2018.
df_controls$pc18_forest_share[is.na(df_controls$pc18_forest_share)] <- 0

#Forcibly dropping any observation with NAs.
df_controls <- na.omit(df_controls)

#Removing Infs.
df_controls <- df_controls[is.finite(df_controls$pc18_forest_share), drop = FALSE]
df_controls <- df_controls[is.finite(df_controls$pc18_st_share), drop = FALSE]
df_controls <- df_controls[is.finite(df_controls$pc18_rural_share), drop = FALSE]

#Specifying columns to be winsorized.
cols_to_winsorize <- c("pc18_rural_share", "pc18_work_share", "pc18_forest_share",
                       "pc18_sc_share", "pc18_st_share", "pc18_lit_share",
                       "pc01_rural_share", "pc01_work_share", "pc01_forest_share",
                       "pc01_sc_share", "pc01_st_share", "pc01_lit_share",
                       "pc11_rural_share", "pc11_work_share", "pc11_forest_share",
                       "pc11_sc_share", "pc11_st_share", "pc11_lit_share")

#Winsorizing controls using p = 0.05.
df_controls <- df_controls %>%
  mutate(across(all_of(cols_to_winsorize), ~ Winsorize(., probs = c(0.05, 0.95), na.rm = TRUE)))

#Converting distances to kilometers.
df_controls$d2c_18 <- df_controls$d2c_18/1000
df_controls$d2c_21 <- df_controls$d2c_21/1000

#Converting STATE column to factor.
df_controls$STATE <- as.factor(df_controls$STATE)

#Checking the structure and summary statistics of the data.
str(df_controls)
summary(df_controls)

#Saving final dataset.
saveRDS(df_controls, file = "DATA/df_controls.rds")
