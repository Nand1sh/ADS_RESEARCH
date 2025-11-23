#Loading required packages.
library(dplyr)

#Reading final dataset.
df_controls <- read.csv("DATA/df_controls.csv")

#Creating list to store state wise effective observations
df <- list()

#Storing statewise data in the list
for (i in c("bihar","kerala", "andhra pradesh", "telangana",
            "uttarpradesh", "jharkhand", "chhatisgarh",
            "madhya pradesh", "maharashtra","odisha")) {
  df[[i]] <- df_controls[df_controls$STATE == i, ]
}

#Filtering for effective observations using MSE optimized bandwidth bounds
df[["andhra pradesh"]] <- df[["andhra pradesh"]] %>% filter(d2c_18 >= -34.799 & d2c_18 <= 34.799)
df[["bihar"]] <- df[["bihar"]] %>% filter(d2c_18 >= -19.468 & d2c_18 <= 19.468)
df[["chhatisgarh"]] <- df[["chhatisgarh"]] %>% filter(d2c_18 >= -16.075 & d2c_18 <= 16.075)
df[["jharkhand"]] <- df[["jharkhand"]] %>% filter(d2c_18 >= -14.457 & d2c_18 <= 14.457)
df[["kerala"]] <- df[["kerala"]] %>% filter(d2c_18 >= -11.379 & d2c_18 <= 11.379)
df[["madhya pradesh"]] <- df[["madhya pradesh"]] %>% filter(d2c_18 >= -32.865 & d2c_18 <= 32.865)
df[["maharashtra"]] <- df[["maharashtra"]] %>% filter(d2c_18 >= -25.233 & d2c_18 <= 25.233)
df[["odisha"]] <- df[["odisha"]] %>% filter(d2c_18 >= -23.332 & d2c_18 <= 23.332)
df[["telangana"]] <- df[["telangana"]] %>% filter(d2c_18 >= -27.715 & d2c_18 <= 27.715)
df[["uttarpradesh"]] <- df[["uttarpradesh"]] %>% filter(d2c_18 >= -60.551 & d2c_18 <= 60.551)

#Combining the filtered data
df_effective <- bind_rows(df)

#Saving effective observations dataframe.
write.csv(df_effective, "DATA/df_effective.csv", row.names = FALSE)
