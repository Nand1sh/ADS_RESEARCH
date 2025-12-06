#Loading required packages.
library(sf)
library(dplyr)
library(tidyr)

#Reading VCF data
vcf <- read.csv("DATA/SHRUG/vcf_pc11subdist.csv")

#Reading SHRUG control data
controls <- st_read("DATA/Shapefiles/S6.shp")

#Renaming columns to fit shapefile's 10 character limit
names(vcf)[names(vcf) == "vcf_num_cells"] <- "vcf_nc"
names(vcf)[names(vcf) == "vcf_mean"] <- "vcf_avg"

#Reducing year length to fit shapefile's 10 character limit
vcf <- vcf %>%
  mutate(year = substr(year, 3, 4))

#Reshaping to wide format
vcf <- vcf %>%
	mutate(sd_d_s = paste(pc11_subdistrict_id,pc11_district_id,pc11_state_id, sep = "_")) %>%
	select(sd_d_s, year, vcf_min, vcf_max, vcf_avg, vcf_nc) %>%
	pivot_wider(id_cols = sd_d_s, names_from = year,
		values_from = c(vcf_min, vcf_max, vcf_avg, vcf_nc), names_sep = ""
	)

#Merging VCF data to SHRUG controls
controls_merged <- merge(controls, vcf, by = 'sd_d_s')

#Writing shapefile for SHRUG.
st_write(controls_merged, "DATA/Shapefiles/S7.5.shp")
