#Loading required packages,
library(dplyr)
library(sf)

#Reading final dataset.
df_controls <- readRDS("DATA/df_controls.rds")

#Identifying rows with non alphabet characters in string names.
non_alpha_distrcts <- df_controls[grep("[^a-zA-Z]", df_controls$Distrct), ]
non_alpha_tehsils <- df_controls[grep("[^a-zA-Z]", df_controls$TEHSIL), ]

#Extracting non alphabet characters from the filtered rows.
non_alpha_distrcts <- unlist(regmatches(non_alpha_distrcts$Distrct, gregexpr("[^a-zA-Z]",non_alpha_distrcts$Distrct)))
non_alpha_tehsils <- unlist(regmatches(non_alpha_tehsils$TEHSIL, gregexpr("[^a-zA-Z]",non_alpha_tehsils$TEHSIL)))

#Keeping only unique instances of the non alphabet characters.
non_alpha_distrcts <- unique(non_alpha_distrcts)
non_alpha_tehsils <- unique(non_alpha_tehsils)

#Printing values of the unique characters potentially requiring cleaning.
print(non_alpha_distrcts)
print(non_alpha_tehsils)

#Cleaning the names.
df_controls$Distrct <- gsub(">", "a", df_controls$Distrct)
df_controls$Distrct <- gsub("@", "u", df_controls$Distrct)
df_controls$Distrct <- gsub("\\|", "i", df_controls$Distrct)  
df_controls$TEHSIL <- gsub(">", "a", df_controls$TEHSIL)
df_controls$TEHSIL <- gsub("@", "u", df_controls$TEHSIL)
df_controls$TEHSIL <- gsub("\\|", "i", df_controls$TEHSIL)  
df_controls$TEHSIL <- gsub("1", "i", df_controls$TEHSIL)
df_controls$TEHSIL <- gsub("<", "a", df_controls$TEHSIL)
df_controls$TEHSIL <- gsub("\\\\", "i", df_controls$TEHSIL)
df_controls$TEHSIL <- gsub("#", "u", df_controls$TEHSIL)

#Matching case of all string variables.
df_controls$Distrct <- tolower(df_controls$Distrct)
df_controls$STATE <- tolower(df_controls$STATE)
df_controls$TEHSIL <- tolower(df_controls$TEHSIL)

#Removing geometry to de-shapefile the dataframe.
df_controls <- st_drop_geometry(df_controls)

#Removing ID column.
df_controls <- df_controls %>% select(-ID)

#Saving cleaned dataframe.
write.csv(df_controls, "DATA/df_controls.csv", row.names = FALSE)
