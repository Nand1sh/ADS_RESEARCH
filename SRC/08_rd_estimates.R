#Loading required packages.
library(dplyr)
library(rdrobust)
library(ggplot2)
library(gridExtra)
library(stargazer)
options(scipen=999)

#Reading final dataset.
df_controls <- read.csv("DATA/df_controls.csv")

#Main RDD statewise.

#Creating list variables to store regression results.
srdd <- list()
srdd2 <- list()

#Running statewise RDD with and without controls using riots post 2018 as outcome.
for (i in c("karnataka", "andhra pradesh", "telangana", "uttarpradesh",
	"rajasthan", "jharkhand", "chhatisgarh", "madhya pradesh", "mizoram",
	"maharashtra", "bihar", "gujarat", "odisha", "assam")) {
  df <- df_controls[df_controls$STATE == i, ]
  z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                      df$pc18_work_share, df$pc18_rural_share,
                      df$pc18_lit_share, df$pc18_forest_share)
  
  srdd[[i]] <- rdrobust(y = df$pm25_avg18, x = df$d2c,  all = TRUE, covs = z_controls)
  srdd2[[i]] <- rdrobust(y = df$pm25_avg18, x = df$d2c,  all = TRUE)
}


#Running statewise RDD with and without controls using riots post 2018 as outcome.
for (i in c("andhra pradesh", "telangana", "uttarpradesh",
	"rajasthan", "jharkhand", "chhatisgarh", "madhya pradesh", "mizoram",
	"maharashtra", "bihar", "gujarat", "odisha", "assam")) {
  df <- df_controls[df_controls$STATE == i, ]
  z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                      df$pc18_work_share, df$pc18_rural_share,
                      df$pc18_lit_share, df$pc18_forest_share)
  
  srdd[[i]] <- rdrobust(y = df$pm25_max18, x = df$d2c,  all = TRUE, covs = z_controls)
  srdd2[[i]] <- rdrobust(y = df$pm25_max18, x = df$d2c,  all = TRUE)
}

####### CHATGPT

srdd <- list()
srdd2 <- list()

# Running statewise RDD with and without controls using pm25_max18 as outcome
state_list <- c(
  "karnataka",
  "kerala",
  "andhra pradesh",
  "telangana",
  "uttarpradesh",
  "rajasthan",
  "jharkhand",
  "chhatisgarh",
  "madhya pradesh",
  "meghalaya",
  "tripura",
  "manipur",
  "mizoram",
  "maharashtra",
  "bihar",
  "gujarat",
  "odisha",
  "tamil nadu",
  "assam"
)

for (i in state_list) {
  
  cat("\nRunning state:", i, "\n")
  
  df <- df_controls[df_controls$STATE == i, ]
  
  z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                      df$pc18_work_share, df$pc18_rural_share,
                      df$pc18_lit_share, df$pc18_forest_share)
  
  # ---- Attempt RDD with controls ----
  srdd[[i]] <- tryCatch({
    rdrobust(y = df$pm25_avg16, x = df$d2c, all = TRUE, covs = z_controls)
  }, error = function(e) {
    message("  RDD with controls failed: ", conditionMessage(e))
    return(NULL)   # skip this state for srdd
  })
  
  # ---- Attempt RDD without controls ----
  srdd2[[i]] <- tryCatch({
    rdrobust(y = df$pm25_avg16, x = df$d2c, all = TRUE)
  }, error = function(e) {
    message("  RDD without controls failed: ", conditionMessage(e))
    return(NULL)   # skip this state for srdd2
  })
}




#Reporting main results state-wise.
# ANDHRA PRADESH
summary(srdd[["andhra pradesh"]])
summary(srdd2[["andhra pradesh"]])

# ASSAM
summary(srdd[["assam"]])
summary(srdd2[["assam"]])

# BIHAR
summary(srdd[["bihar"]])
summary(srdd2[["bihar"]])

# CHHATISGARH
summary(srdd[["chhatisgarh"]])
summary(srdd2[["chhatisgarh"]])

# GUJARAT
summary(srdd[["gujarat"]])
summary(srdd2[["gujarat"]])

# JHARKHAND
summary(srdd[["jharkhand"]])
summary(srdd2[["jharkhand"]])

# KARNATAKA
summary(srdd[["karnataka"]])
summary(srdd2[["karnataka"]])

# KERALA
summary(srdd[["kerala"]])
summary(srdd2[["kerala"]])

# MADHYA PRADESH
summary(srdd[["madhya pradesh"]])
summary(srdd2[["madhya pradesh"]])

# MAHARASHTRA
summary(srdd[["maharashtra"]])
summary(srdd2[["maharashtra"]])

# MANIPUR
summary(srdd[["manipur"]])
summary(srdd2[["manipur"]])

# MEGHALAYA
summary(srdd[["meghalaya"]])
summary(srdd2[["meghalaya"]])

# MIZORAM
summary(srdd[["mizoram"]])
summary(srdd2[["mizoram"]])

# ODISHA
summary(srdd[["odisha"]])
summary(srdd2[["odisha"]])

# RAJASTHAN
summary(srdd[["rajasthan"]])
summary(srdd2[["rajasthan"]])

# TAMIL NADU
summary(srdd[["tamil nadu"]])
summary(srdd2[["tamil nadu"]])

# TELANGANA
summary(srdd[["telangana"]])
summary(srdd2[["telangana"]])

# TRIPURA
summary(srdd[["tripura"]])
summary(srdd2[["tripura"]])

# UTTARPRADESH
summary(srdd[["uttarpradesh"]])
summary(srdd2[["uttarpradesh"]])

#Running aggregate RDD with and without controls using riots post 2018 as outcome.

#Copying final data into a temporary df.
df <- df_controls

df <- df %>% filter(!STATE == "uttarpradesh" )
df <- df %>% filter(!STATE == "mizoram" )

#Creating matrix for state fixed effects.
states_fe <- model.matrix(~ df$STATE - 1)
states_fe <- states_fe[, -c(17)]

#Binding all controls together.
z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                    df$pc18_work_share, df$pc18_rural_share,
                    df$pc18_lit_share, df$pc18_forest_share, states_fe)

#With controls and standard errors clustered by state.
srdd3 <- rdrobust(y = df$pm25_avg18,
                 x = df$d2c,  all = TRUE, cluster = df$STATE, covs = z_controls)
summary(srdd3)


#Without controls but standard errors clustered by state.
srdd4 <- rdrobust(y = df$pm25_avg18,
                 x = df$d2c,  all = TRUE, cluster = df$STATE)
summary(srdd4)

#Placebo test.
#Using pre 2018 riots as outcome. Only checking for CHHATISGARH and ODISHA.

#CHHATISGARH
df <- df_controls[df_controls$STATE == "chhatisgarh", ]

z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                    df$pc18_work_share, df$pc18_rural_share,
                    df$pc18_lit_share, df$pc18_forest_share)

srdd5 <- rdrobust(y = df$rt_2016 + df$rt_2017,
                 x = df$d2c_18,  all = TRUE, covs = z_controls)
summary(srdd5)

#ODISHA
df <- df_controls[df_controls$STATE == "odisha", ]

z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                    df$pc18_work_share, df$pc18_rural_share,
                    df$pc18_lit_share, df$pc18_forest_share)

srdd6 <- rdrobust(y = df$rt_2016 + df$rt_2017,
                 x = df$d2c_18,  all = TRUE, covs = z_controls)
summary(srdd6)
