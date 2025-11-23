#Loading required packages
library(sf)
library(dplyr)
library(stringdist)

#Reading master subdistricts shapefile from SoI.
shapefile <- st_read("DATA/SoI/SUBDISTRICT_BOUNDARY.shp")

#Reading the list of LWE districts for 2018 and 2021.
lwe_2018 <- data.frame(District = c("East Godavari","Guntur", "Srikakulam", "Visakhapatnam",
	                           "Vizianagram", "West Godavari", "Arwal", "Aurangabad",
                                   "Banka","East Champaran", "Gaya", "Jamui", "Jehanabad",
	                           "Kaimur", "Lakhisarai", "Munger", "Muzaffarpur", "Nalanda",
	                           "Nawada", "Rohtas", "Vaishali", "West Champaran", "Balod",
	                           "Balrampur", "Bastar", "Bijapur", "Dantewada", "Dhamtari",
	                           "Gariyaband", "Kanker", "Kondagaon", "Mahasamund", "Narayanpur",
	                           "Rajnandgaon", "Sukma", "Kabirdham", "Bokaro", "Chatra", 
	                           "Dhanbad", "Dumka", "East Singhbhum", "Garhwa", "Giridih",
	                           "Gumla", "Hazaribagh", "Khunti", "Koderma", "Latehar", "Lohardaga",
	                           "Palamu", "Ramgarh", "Ranchi", "Simdega", "Saraikela-Kharaswan",
	                           "West Singhbhum", "Balaghat", "Mandla", "Chandrapur",  "Gadchiroli",
	                           "Gondia", "Angul", "Bargarh", "Bolangir", "Boudh", "Deogarh",
	                           "Kalahandi", "Kandhamal", "Koraput", "Malkangiri", "Nabrangpur",
	                           "Nayagarh", "Nuapada", "Rayagada", "Sambhalpur", "Sundergarh",
	                           "Adilabad", "Bhadradri-Kothagudem", "Jayashankar-Bhupalpally",
	                           "Khammam", "Komaram-Bheem", "Mancherial", "Peddapalle",
	                           "Warangal Rural", "Chandauli", "Mirzapur", "Sonebhadra", "Jhargram",
	                           "Malappuram", "Palakkad", "Wayanad"))

lwe_2021 <- data.frame(District = c("East Godavari", "Srikakulam", "Visakhapatnam", "Vizianagram",
	                            "West Godavari", "Aurangabad", "Banka", "Gaya", "Jamui", "Kaimur",
	                            "Lakhisarai", "Munger", "Nawada", "Rohtas", "West Champaran",
                                    "Balrampur", "Bastar", "Bijapur", "Dantewada", "Dhamtari", 
	                            "Gariyaband", "Kanker", "Kondagaon", "Mahasamund",  "Narayanpur", 
	                            "Rajnandgaon", "Sukma", "Kabirdham", "Mungeli", "Bokaro", "Chatra",
                                    "Dhanbad", "Dumka", "East Singhbhum", "Garhwa", "Giridih", "Gumla",
	                            "Hazaribagh", "Khunti", "Latehar", "Lohardaga", "Palamu", "Ranchi",
	                            "Saraikela-Kharaswan", "West Singhbhum", "Balaghat", "Mandla",
                                    "Dindori", "Gadchiroli", "Gondia", "Bargarh", "Bolangir", "Kalahandi",
	                            "Kandhamal", "Koraput", "Malkangiri", "Nabrangpur", "Nuapada", 
	                            "Rayagada", "Sundergarh", "Adilabad", "Bhadradri-Kothagudem",
                                    "Jayashankar-Bhupalpally", "Komaram-Bheem", "Mancherial", "Mulugu", 
	                            "Jhargram", "Malappuram", "Palakkad", "Wayanad"))

#Converting district names to same case across dataframes for better merging.
lwe_2018$District <- tolower(lwe_2018$District)
lwe_2021$District <- tolower(lwe_2021$District)
shapefile$District <- tolower(shapefile$District)

#Find district matches for the LWE districts from the master shapefile.
lwe_2018 <- lwe_2018 %>%
  rowwise() %>% 
  mutate(D = shapefile$District[which.max(stringsim(District, shapefile$District, method = 'jw'))])

lwe_2021 <- lwe_2021 %>%
  rowwise() %>% 
  mutate(D = shapefile$District[which.max(stringsim(District, shapefile$District, method = 'jw'))])

#Fixing mismatches manually
lwe_2018$D[lwe_2018$District == "banka"] = "b>nka"
lwe_2018$D[lwe_2018$District == "jamui"] = "jam@i"
lwe_2018$D[lwe_2018$District == "west champaran"] = "pashchimi champ>ran"
lwe_2018$D[lwe_2018$District == "balrampur"] = "balr>mpur"
lwe_2018$D[lwe_2018$District == "bijapur"] = "b|j>pur"
lwe_2018$D[lwe_2018$District == "dantewada"] = "dakshin bastar dantew>da"
lwe_2018$D[lwe_2018$District == "kanker"] = "uttar bastar k>nker"
lwe_2018$D[lwe_2018$District == "narayanpur"] = "n>r>inpur"
lwe_2018$D[lwe_2018$District == "nuapada"] = "nu>parha"
lwe_2018$D[lwe_2018$District == "rayagada"] = "r>yagarha"
lwe_2018$D[lwe_2018$District == "palakkad"] = "p>lakk>d"
lwe_2018$D[lwe_2018$District == "east champaran"] = "p@rbi champ>ran"
lwe_2018$D[lwe_2018$District == "nalanda"] = "n>landa"
lwe_2018$D[lwe_2018$District == "boudh"] = "baudh (bauda)"

lwe_2021$D[lwe_2021$District == "banka"] = "b>nka"
lwe_2021$D[lwe_2021$District == "jamui"] = "jam@i"
lwe_2021$D[lwe_2021$District == "west champaran"] = "pashchimi champ>ran"
lwe_2021$D[lwe_2021$District == "balrampur"] = "balr>mpur"
lwe_2021$D[lwe_2021$District == "bijapur"] = "b|j>pur"
lwe_2021$D[lwe_2021$District == "dantewada"] = "dakshin bastar dantew>da"
lwe_2021$D[lwe_2021$District == "kanker"] = "uttar bastar k>nker"
lwe_2021$D[lwe_2021$District == "narayanpur"] = "n>r>inpur"
lwe_2021$D[lwe_2021$District == "nuapada"] = "nu>parha"
lwe_2021$D[lwe_2021$District == "rayagada"] = "r>yagarha"
lwe_2021$D[lwe_2021$District == "palakkad"] = "p>lakk>d"

#Adding treatment status to master shapefile.
shapefile <- shapefile %>% mutate(LWE2018 = ifelse(District %in% lwe_2018$D,1,0))
shapefile <- shapefile %>% mutate(LWE2021 = ifelse(District %in% lwe_2021$D,1,0))

#Checking and correcting for duplicates.
shapefile <- shapefile %>% mutate(Test = paste(District,"_",STATE))
unique(shapefile$Test[shapefile$LWE2018 == 1])
unique(shapefile$Test[shapefile$LWE2021 == 1])
shapefile$LWE2018[shapefile$Test =="aurang>b>d _ MAHARASHTRA"] = 0
shapefile$LWE2021[shapefile$Test =="aurang>b>d _ MAHARASHTRA"] = 0
shapefile <- subset(shapefile, select = -Test)

#Writing shapefile with treatment status.
st_write(shapefile, "DATA/Shapefiles/S1.shp")
