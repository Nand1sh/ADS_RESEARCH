#Loading required packages
library(sf)
library(dplyr)
library(tidyr)

#Reading AQI data
aqi <- read.csv("DATA/SHRUG/pm25_pc11subdist.csv")

#Reading SHRUG control data
controls <- st_read("DATA/Shapefiles/S6.shp")

#Renaming columns to fit shapefile's 10 character limit
names(aqi)[names(aqi) == "pm25_num_cells"] <- "pm25_nc"
names(aqi)[names(aqi) == "pm25_mean"] <- "pm25_avg"

#Reducing year length to fit shapefile's 10 character limit
aqi <- aqi %>%
  mutate(year = substr(year, 3, 4))

#Reshaping to wide format
aqi <- aqi %>%
	mutate(sd_d_s = paste(pc11_subdistrict_id,pc11_district_id,pc11_state_id, sep = "_")) %>%
	select(sd_d_s, year, pm25_min, pm25_max, pm25_avg, pm25_nc) %>%
	pivot_wider(id_cols = sd_d_s, names_from = year,
		values_from = c(pm25_min, pm25_max, pm25_avg, pm25_nc), names_sep = ""
	)

#Merging AQI data to SHRUG controls
controls_merged <- merge(controls, aqi, by = 'sd_d_s')

#Writing shapefile for SHRUG.
st_write(controls_merged, "DATA/Shapefiles/S7.shp")
