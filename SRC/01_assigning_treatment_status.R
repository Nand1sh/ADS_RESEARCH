#Loading required packages
library(sf)
library(dplyr)
library(stringdist)

#Reading master subdistricts shapefile from SHRUG
shapefile <- st_read("DATA/SHRUG/SUBDISTRICT_BOUNDARY.shp")

#Reading the list of ADS treated districts
ads <- data.frame(District = c("Alluri Sitharamaraju", "Parvathipuram Manyam",
                                    "Y.S.R. Kadapa", "Namsai", "Baksa", "Barpeta",
                                    "Darrang", "Dhubri", "Goalpara", "Hailakandi",
                                    "Udalguri", "Araria", "Aurangabad", "Banka",
                                    "Begusarai", "Gaya", "Jamui", "Katihar",
                                    "Khagaria", "Muzaffarpur", "Nawada", "Purnea",
                                    "Sheikhpura", "Sitamarhi", "Bastar",
                                    "Bijapur", "Dantewada", "Kanker", "Kondagaon",
                                    "Korba", "Mahasamund", "Narayanpur",
                                    "Rajnandgaon", "Sukma", "Dahod", "Narmada",
                                    "Mewat", "Chamba", "Baramula", "Kupwara",
                                    "Bokaro", "Chatra", "Dumka", "Garhwa",
                                    "Giridih", "Godda", "Gumla", "Hazaribag",
                                    "Khunti", "Latehar", "Lohardaga", "Pakur",
                                    "Palamu", "Pashchimi Singhbhum",
                                    "Purbi Singhbhum", "Ramgarh", "Ranchi",
                                    "Sahibganj", "Simdega", "Raichur", "Yadgir",
                                    "Wayanad", "Barwani", "Chhatarpur", "Damoh",
                                    "Guna", "Khandwa", "Rajgarh", "Singrauli",
                                    "Vidisha", "Gadchiroli", "Nandurbar",
                                    "Osmanabad", "Washim", "Chandel", "Ribhoi",
                                    "Mamit", "Kiphire", "Balangir", "Dhenkanal",
                                    "Gajapati", "Kalahandi", "Kandhamal",
                                    "Koraput", "Malkangiri", "Nabarangapur",
                                    "Nuapada", "Rayagada", "Ferozepur", "Moga",
                                    "Baran", "Dholpur", "Jaisalmer", "Karauli",
                                    "Sirohi", "Soreng", "Ramanathapuram",
                                    "Virudhunagar", "Asifabad",
                                    "Bhadradri-Kothagudem", "Bhupalpally",
                                    "Dhalai", "Bahraich", "Balrampur",
                                    "Chandauli", "Chitrakoot", "Fatehpur",
                                    "Shravasti", "Siddharthnagar", "Sonbhadra",
                                    "Haridwar", "Udham Singh Nagar"))

#Converting district names to same case across dataframes for better merging.
ads$District <- tolower(ads$District)
shapefile$District <- tolower(shapefile$District)

#Find district matches for the ADS districts from the master shapefile.
ads <- ads %>%
  rowwise() %>% 
  mutate(D = shapefile$District[which.max(stringsim(District, shapefile$District, method = 'jw'))])

#Fixing mismatches manually
ads$D[ads$District == "banka"] = "b>nka"
ads$D[ads$District == "jamui"] = "jam@i"
ads$D[ads$District == "purnea"] = "p@rnia"
ads$D[ads$District == "sitamarhi"] = "s|t>marhi"
ads$D[ads$District == "sitamarhi"] = "s|t>marhi"
ads$D[ads$District == "bijapur"] = "b|j>pur"
ads$D[ads$District == "dantewada"] = "dakshin bastar dantew>da"
ads$D[ads$District == "kanker"] = "uttar bastar k>nker"
ads$D[ads$District == "narayanpur"] = "n>r>inpur"
ads$D[ads$District == "baramula"] = "b>ram@la"
ads$D[ads$District == "pashchimi singhbhum"] = "west singhbhum"
ads$D[ads$District == "purbi singhbhum"] = "east singhbhum"
ads$D[ads$District == "raichur"] = "r>ich@r"
ads$D[ads$District == "khandwa"] = "east nimar"
ads$D[ads$District == "osmanabad"] = "usm>n>b>d"
ads$D[ads$District == "washim"] = "w>sh|m"
ads$D[ads$District == "balangir"] = "bolangir (balangir)"
ads$D[ads$District == "nuapada"] = "nu>parha"
ads$D[ads$District == "rayagada"] = "r>yagarha"
ads$D[ads$District == "baran"] = "b>r>n"
ads$D[ads$District == "dholpur"] = "dhaulpur"
ads$D[ads$District == "asifabad"] = "kumuram bheem"
ads$D[ads$District == "bhupalpally"] = "jayashankar bhupalapally"

#Removing districts for which no match was found
ads <- ads %>%
	filter(!District %in% c('alluri sitharamaraju', 'parvathipuram manyam', 'soreng'))

#Adding treatment status to master shapefile.
shapefile <- shapefile %>% mutate(ADS = ifelse(District %in% ads$D,1,0))

#Checking and correcting for duplicates.
shapefile <- shapefile %>% mutate(Test = paste(District,"_",STATE))
unique(shapefile$Test[shapefile$ADS == 1])
shapefile$ADS[shapefile$Test =="aurang>b>d _ MAHARASHTRA"] = 0
shapefile <- subset(shapefile, select = -Test)

#Writing shapefile with treatment status.
st_write(shapefile, "DATA/Shapefiles/S1.shp")
