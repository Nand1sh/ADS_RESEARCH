#Loading required packages.
library(sf)
library(dplyr)

#Reading shapefile for controls from SHRUG.
controls <- st_read("DATA/SHRUG/Controls.shp")

#Removing leading 0s from string names.
controls$pc11_sd_id <- sub("^0", "", controls$pc11_sd_id)
controls$pc11_sd_id <- sub("^0", "", controls$pc11_sd_id)
controls$pc11_s_id <- sub("^0", "", controls$pc11_s_id)

#Renaming columns names for later merging.
names(controls)[names(controls) %in% c("pc11_s_id", "pc11_d_id", "pc11_sd_id")] <- c("pc11_state_id", "pc11_district_id", "pc11_subdistrict_id")

#Filtering for relevant states.
controls <- controls[controls$pc11_state_id %in% c("9","10","19","20","21","22","23","27","28","32"), ]

#Creating new string to get a unique identifier.
controls <- controls %>% 
  mutate(sd_d_s = paste0(controls$pc11_subdistrict_id,"_",controls$pc11_district_id,"_",controls$pc11_state_id))

#Reading controls data for 2011 at shrid level.
pc11_pca_shrid <- read.csv("DATA/SHRUG/pc11_pca_clean_shrid.csv")
pc11_vd_shrid <- read.csv("DATA/SHRUG/pc11_vd_clean_shrid.csv")

#Calling garbage collection to free memory.
gc()

#Keeping only relevant variables.
pc11_pca_shrid <- pc11_pca_shrid %>%
  select(c("shrid2", "pc11_pca_tot_p", "pc11_pca_p_sc", "pc11_pca_p_st", "pc11_pca_p_lit",
           "pc11_pca_tot_work_p"))
pc11_vd_shrid <- pc11_vd_shrid %>%
  select(c("shrid2", "pc11_vd_t_p", "pc11_vd_land_fores", "pc11_vd_area"))

#Removing NAs.
pc11_pca_shrid <- na.omit(pc11_pca_shrid)
pc11_vd_shrid <- na.omit(pc11_vd_shrid)

#Removing duplicates.
pc11_pca_shrid <- pc11_pca_shrid[!duplicated(pc11_pca_shrid$shrid2), ]
pc11_vd_shrid <- pc11_vd_shrid[!duplicated(pc11_vd_shrid$shrid2), ]

#Garbage collection.
gc()

#Reading keys to merge files by shrid2.
pc11_keys <- read.csv("DATA/SHRUG/shrid_pc11subdist_key.csv")

#Removing NAs.
pc11_keys <- na.omit(pc11_keys)

#Removing duplicates.
pc11_keys <- pc11_keys[!duplicated(pc11_keys$shrid2), ]

#Assigning keys.
pc11_pca_shrid <- pc11_pca_shrid %>% 
  merge(.,pc11_keys, by = 'shrid2')
pc11_vd_shrid <- pc11_vd_shrid %>% 
  merge(.,pc11_keys, by = 'shrid2')

#Removing keys file post assignment.
rm(pc11_keys)

#Garbage collection.
gc()

#Filtering for relevant states.
pc11_pca_shrid <- pc11_pca_shrid[pc11_pca_shrid$pc11_state_id %in% c("9","10","19","20","21","22","23","27","28","32"), ]
pc11_vd_shrid <- pc11_vd_shrid[pc11_vd_shrid$pc11_state_id %in% c("9","10","19","20","21","22","23","27","28","32"), ]

#Adding unique identifier.
pc11_pca_shrid <- pc11_pca_shrid %>%
  mutate(sd_d_s = paste0(pc11_pca_shrid$pc11_subdistrict_id,"_",pc11_pca_shrid$pc11_district_id,"_",pc11_pca_shrid$pc11_state_id))

#Keeping only required columns.
pc11_pca_shrid <- pc11_pca_shrid %>%
  select(c("shrid2","pc11_state_id", "pc11_district_id", "pc11_subdistrict_id", "sd_d_s", "pc11_pca_tot_p", "pc11_pca_p_sc", "pc11_pca_p_st", "pc11_pca_p_lit",
           "pc11_pca_tot_work_p"))
pc11_vd_shrid <- pc11_vd_shrid %>%
  select(c("shrid2", "pc11_vd_t_p", "pc11_vd_land_fores", "pc11_vd_area"))

#Merging all 2011 controls.
merged_2011 <- merge(pc11_pca_shrid, pc11_vd_shrid, by = 'shrid2', all.x = TRUE)

#Removing NAs.
merged_2011[is.na(merged_2011)] <- 0

#Removing large datasets no longer required to free up space.
rm(pc11_pca_shrid, pc11_vd_shrid)

#Garbage collection.
gc()

#Reading controls data for 2001 at shrid level.
pc01_pca_shrid <- read.csv("DATA/SHRUG/pc01_pca_clean_shrid.csv")
pc01_vd_shrid <- read.csv("DATA/SHRUG/pc01_vd_clean_shrid.csv")

#Garbage collection.
gc()

#Keeping only relevant variables.
pc01_pca_shrid <- pc01_pca_shrid %>%
  select(c("shrid2", "pc01_pca_tot_p", "pc01_pca_p_sc", "pc01_pca_p_st", "pc01_pca_p_lit",
           "pc01_pca_tot_work_p"))
pc01_vd_shrid <- pc01_vd_shrid %>%
  select(c("shrid2", "pc01_vd_t_p", "pc01_vd_land_fores", "pc01_vd_area"))

#Removing NAs.
pc01_pca_shrid <- na.omit(pc01_pca_shrid)
pc01_vd_shrid <- na.omit(pc01_vd_shrid)

#Removing duplicates.
pc01_pca_shrid <- pc01_pca_shrid[!duplicated(pc01_pca_shrid$shrid2), ]
pc01_vd_shrid <- pc01_vd_shrid[!duplicated(pc01_vd_shrid$shrid2), ]

#Garbage collection.
gc()

#Reading keys to merge files by shrid2.
pc01_keys <- read.csv("DATA/SHRUG/shrid_pc01subdist_key.csv")

#Removing NAs.
pc01_keys <- na.omit(pc01_keys)

#Removing duplicates.
pc01_keys <- pc01_keys[!duplicated(pc01_keys$shrid2), ]

#Assigning keys.
pc01_pca_shrid <- pc01_pca_shrid %>% 
  merge(.,pc01_keys, by = 'shrid2')
pc01_vd_shrid <- pc01_vd_shrid %>% 
  merge(.,pc01_keys, by = 'shrid2')

#Removing keys file post assignment.
rm(pc01_keys)

#Garbage collection.
gc()

#Filtering for relevant states.
pc01_pca_shrid <- pc01_pca_shrid[pc01_pca_shrid$pc01_state_id %in% c("9","10","19","20","21","22","23","27","28","32"), ]
pc01_vd_shrid <- pc01_vd_shrid[pc01_vd_shrid$pc01_state_id %in% c("9","10","19","20","21","22","23","27","28","32"), ]

#Adding unique identifier.
pc01_pca_shrid <- pc01_pca_shrid %>%
  mutate(sd_d_s = paste0(pc01_pca_shrid$pc01_subdistrict_id,"_",pc01_pca_shrid$pc01_district_id,"_",pc01_pca_shrid$pc01_state_id))

#Keeping only required columns.
pc01_pca_shrid <- pc01_pca_shrid %>%
  select(c("shrid2","pc01_state_id", "pc01_district_id", "pc01_subdistrict_id", "sd_d_s", "pc01_pca_tot_p", "pc01_pca_p_sc", "pc01_pca_p_st", "pc01_pca_p_lit",
           "pc01_pca_tot_work_p"))
pc01_vd_shrid <- pc01_vd_shrid %>%
  select(c("shrid2", "pc01_vd_t_p", "pc01_vd_land_fores", "pc01_vd_area"))

#Merging all 2001 controls.
merged_2001 <- merge(pc01_pca_shrid, pc01_vd_shrid, by = 'shrid2', all.x = TRUE)

#Removing NAs.
merged_2001[is.na(merged_2001)] <- 0

#Removing large datasets no longer required to free up space.
rm(pc01_pca_shrid, pc01_vd_shrid)

#Garbage collection.
gc()

#Keeping relevant variables to merge across years.
merged_2001 <- merged_2001 %>%
  select(c("shrid2", "pc01_pca_tot_p", "pc01_pca_p_sc", "pc01_pca_p_st", "pc01_pca_p_lit",
           "pc01_pca_tot_work_p", "pc01_vd_t_p", "pc01_vd_land_fores", "pc01_vd_area"))

#Merging across years.
master_merge <- merge(merged_2011, merged_2001, by = 'shrid2')

#Aggregating back to the subdistrict level.
master_merge  <- aggregate(cbind(pc11_pca_tot_p, pc11_pca_p_sc, pc11_pca_p_st,
                                 pc11_pca_p_lit, pc11_pca_tot_work_p,
                                 pc11_vd_t_p, pc11_vd_land_fores, pc11_vd_area,
                                 pc01_pca_tot_p, pc01_pca_p_sc, pc01_pca_p_st,
                                 pc01_pca_p_lit, pc01_pca_tot_work_p,
                                 pc01_vd_t_p, pc01_vd_land_fores,
                                 pc01_vd_area) ~ sd_d_s, data = master_merge, FUN = sum)

#Removing large datasets no longer required.
rm(merged_2001, merged_2011)

#Garbage collection.
gc()

#Merging aggregated controls to the controls shapefile.
controls_merged <- merge(controls, master_merge, by = 'sd_d_s')

#Renaming columns names to prevent truncation when writing shapefiles.
names(controls_merged) <- c("sd_d_s", "pc11_state", "pc11_distr",
                            "pc11_subdi", "sd_name", "pc11_tot_p",
                            "pc11_sc", "pc11_st", "pc11_lit",
                            "pc11_work", "pc11_v_p", "pc11_fores",
                            "pc11_v_a", "pc01_tot_p", "pc01_sc",
	                    "pc01_st", "pc01_lit", "pc01_work",
	                    "pc01_v_p", "pc01_fores", "pc01_v_a", "geometry")

#Writing shapefile for controls.
st_write(controls_merged, "DATA/Shapefiles/S6.shp")
