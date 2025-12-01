#Loading required packages
library(sf)
library(dplyr)
library(stringdist)

#Reading master subdistricts shapefile from SoI
shapefile <- st_read("DATA/SHRUG/SUBDISTRICT_BOUNDARY.shp")

#Reading the list of ADS treated districts
ads <- data.frame(District = c(
    "Vizianagaram", "Visakhapatnam", "Cuddapah", "Namsai",
    "Dhubri", "Goalpara", "Barpeta", "Hailakandi", "Baksa",
    "Darrang", "Udalguri", "Sitamarhi", "Araria", "Purnia",
    "Katihar", "Muzaffarpur", "Begusarai", "Khagaria",
    "Banka", "Sheikhpura", "Aurangabad", "Gaya", "Nawada",
    "Jamui", "Korba", "Rajnandgaon", "Mahasamund", "Uttar Bastar Kanker", 
    "Bastar", "Narayanpur", "Dakshin Bastar Dantewada", "Bijapur",
    "Sukma", "Kondagaon", "Dahod", "Narmada", "Mewat", "Chamba",
    "Kupwara", "Baramulla", "Garhwa", "Chatra", "Giridih", "Godda",
    "Sahebganj", "Pakur", "Bokaro", "Lohardaga", "Purbi Singhbhum",
    "Palamu", "Latehar", "Hazaribagh", "Ramgarh", "Dumka", "Ranchi",
    "Khunti", "Gumla", "Simdega", "Pashchimi Singhbhum", "Raichur",
    "Yadgir", "Wayanad", "Chhatarpur", "Damoh", "Barwani", "Rajgarh",
    "Vidisha", "Guna", "Singrauli", "Khandwa (East Nimar)", "Nandurbar",
    "Washim", "Gadchiroli", "Osmanabad", "Chandel", "Ri Bhoi", "Mamit",
    "Kiphire", "Dhenkanal", "Gajapati", "Kandhamal", "Balangir", "Nuapada",
    "Kalahandi", "Rayagada", "Nabarangpur", "Koraput", "Malkangiri", "Moga",
    "Firozpur", "Dholpur", "Karauli", "Jaisalmer", "Sirohi", "Baran", "West Sikkim District",
    "Virudhunagar", "Ramanathapuram", "Khammam", "Jayashankar Bhoopalapally", "Kumuram Bheem (Asifabad)",
    "Dhalai", "Chitrakoot", "Fatehpur", "Bahraich", "Shravasti", "Balrampur", "Siddharthnagar",
    "Chandauli", "Sonbhadra", "Udham Singh Nagar", "Haridwar", "Dakshin Dinajpur", "Malda",
    "Murshidabad", "Birbhum", "Nadia"
  )
)

#Converting district names to same case across dataframes for better merging.
ads$District <- tolower(ads$District)
shapefile$District <- tolower(shapefile$District)

#Find district matches for the ADS districts from the master shapefile.
ads <- ads %>%
  rowwise() %>% 
  mutate(D = shapefile$District[which.max(stringsim(District, shapefile$District, method = 'jw'))])

#Fixing mismatches manually
ads$D[ads$District == "cuddapah"] = "y s r kadapa"
ads$D[ads$District == "banka"] = "b>nka"
ads$D[ads$District == "jamui"] = "jam@i"
ads$D[ads$District == "sitamarhi"] = "s|t>marhi"
ads$D[ads$District == "bijapur"] = "b|j>pur"
ads$D[ads$District == "narayanpur"] = "n>r>inpur"
ads$D[ads$District == "baramulla"] = "b>ram@la"
ads$D[ads$District == "purbi singhbhum"] = "east singhbhum"
ads$D[ads$District == "pashchimi singhbhum"] = "west singhbhum"
ads$D[ads$District == "raichur"] = "r>ich@r"
ads$D[ads$District == "khandwa (east nimar)"] = "east nimar"
ads$D[ads$District == "osmanabad"] = "usm>n>b>d"
ads$D[ads$District == "washim"] = "w>sh|m"
ads$D[ads$District == "balangir"] = "bolangir (balangir)"
ads$D[ads$District == "nuapada"] = "nu>parha"
ads$D[ads$District == "rayagada"] = "r>yagarha"
ads$D[ads$District == "baran"] = "b>r>n"
ads$D[ads$District == "dholpur"] = "dhaulpur"
ads$D[ads$District == "malda"] = "m>ldah"
ads$D[ads$District == "west sikkim district"] = "west"

#Adding treatment status to master shapefile.
shapefile <- shapefile %>% mutate(ADS = ifelse(District %in% ads$D,1,0))

#Checking and correcting for duplicates.
shapefile <- shapefile %>% mutate(Test = paste(District,"_",STATE))
unique(shapefile$Test[shapefile$ADS == 1])
shapefile$ADS[shapefile$Test =="west _ DELHI"] = 0
shapefile$ADS[shapefile$Test =="aurang>b>d _ MAHARASHTRA"] = 0
shapefile <- subset(shapefile, select = -Test)

#Writing shapefile with treatment status.
st_write(shapefile, "DATA/Shapefiles/S1.shp")
