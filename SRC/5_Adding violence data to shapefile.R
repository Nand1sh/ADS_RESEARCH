#Loading required packages.
library(sf)
library(dplyr)

#Reading master shapefile.
subdistricts <- st_read("DATA/Shapefiles/S4.shp")

#Declaring the proj4 string of the master shapefile. This was extracted using QGIS.
proj4 <- "+proj=lcc +lat_0=24 +lon_0=80 +lat_1=12.472944 +lat_2=35.172806 +x_0=4000000 +y_0=4000000 +datum=WGS84 +units=m +no_defs"

#Reading separated violence data.
riots <- read.csv("DATA/ACLED/Riots_ACLED.csv")
erv <- read.csv("DATA/ACLED/Explosions_Remoteviolence_ACLED.csv")
vac <- read.csv("DATA/ACLED/Violence_against_civilians_ACLED.csv")
battles <- read.csv("DATA/ACLED/Battles_ACLED.csv")
protests <- read.csv("DATA/ACLED/Protests_ACLED.csv")
stratdev <- read.csv("DATA/ACLED/Strategic_developments_ACLED.csv")

#Adding all categories of violence for each year to the shapefile by first matching the projection.
for (event in c("riots", "erv", "vac", "battles", "protests", "stratdev")) {
  for (year in c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")) {
    event_year <- get(event)[get(event)$year == year, c("longitude", "latitude")]
    event_year <- st_as_sf(event_year, coords = c("longitude", "latitude"), crs = 4326)
    event_year <- st_transform(event_year, crs = proj4)
    subdistricts <- subdistricts %>% 
      mutate(!!paste0(event, "_", year) := lengths(st_intersects(., event_year)))
  }
}

# Summing violence of different years for each category.
subdistricts <- subdistricts %>% 
  mutate(Riots = riots_2016 + riots_2017 + riots_2018 + riots_2019 + riots_2020 + riots_2021 + riots_2022 + riots_2023)
subdistricts <- subdistricts %>% 
  mutate(Vac = vac_2016 + vac_2017 + vac_2018 + vac_2019 + vac_2020 + vac_2021 + vac_2022 + vac_2023)
subdistricts <- subdistricts %>% 
  mutate(Erv = erv_2016 + erv_2017 + erv_2018 + erv_2019 + erv_2020 + erv_2021 + erv_2022 + erv_2023)
subdistricts <- subdistricts %>% 
  mutate(Battles = battles_2016 + battles_2017 + battles_2018 + battles_2019 + battles_2020 + battles_2021 + battles_2022 + battles_2023)
subdistricts <- subdistricts %>% 
  mutate(Protests = protests_2016 + protests_2017 + protests_2018 + protests_2019 + protests_2020 + protests_2021 + protests_2022 + protests_2023)
subdistricts <- subdistricts %>% 
  mutate(Stratdev = stratdev_2016 + stratdev_2017 + stratdev_2018 + stratdev_2019 + stratdev_2020 + stratdev_2021 + stratdev_2022 + stratdev_2023)

#Writing updated shapefile.
st_write(subdistricts, "DATA/Shapefiles/S5.shp")
