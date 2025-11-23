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
for (i in c("bihar","kerala", "andhra pradesh", "telangana",
            "uttarpradesh", "jharkhand", "chhatisgarh",
            "madhya pradesh", "maharashtra","odisha")) {
  df <- df_controls[df_controls$STATE == i, ]
  z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                      df$pc18_work_share, df$pc18_rural_share,
                      df$pc18_lit_share, df$pc18_forest_share)
  
  srdd[[i]] <- rdrobust(y = df$rt_2019 + df$rt_2020 + df$rt_2021 + df$rt_2022 + df$rt_2023, x = df$d2c_18,  all = TRUE, covs = z_controls)
  srdd2[[i]] <- rdrobust(y = df$rt_2019 + df$rt_2020 + df$rt_2021 + df$rt_2022 + df$rt_2023, x = df$d2c_18,  all = TRUE)
}

#Reporting main results state-wise.
summary(srdd[["andhra pradesh"]])
summary(srdd2[["andhra pradesh"]])
summary(srdd[["bihar"]])
summary(srdd2[["bihar"]])
summary(srdd[["chhatisgarh"]])
summary(srdd2[["chhatisgarh"]])
summary(srdd[["jharkhand"]])
summary(srdd2[["jharkhand"]])
summary(srdd[["kerala"]])
summary(srdd2[["kerala"]])
summary(srdd[["madhya pradesh"]])
summary(srdd2[["madhya pradesh"]])
summary(srdd[["maharashtra"]])
summary(srdd2[["maharashtra"]])
summary(srdd[["odisha"]])
summary(srdd2[["odisha"]])
summary(srdd[["telangana"]])
summary(srdd2[["telangana"]])
summary(srdd[["uttarpradesh"]])
summary(srdd2[["uttarpradesh"]])

#Running aggregate RDD with and without controls using riots post 2018 as outcome.

#Copying final data into a temporary df.
df <- df_controls

#Creating matrix for state fixed effects.
states_fe <- model.matrix(~ df$STATE - 1)
states_fe <- states_fe[, -c(11)]

#Binding all controls together.
z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                    df$pc18_work_share, df$pc18_rural_share,
                    df$pc18_lit_share, df$pc18_forest_share, states_fe)

#With controls and standard errors clustered by state.
srdd3 <- rdrobust(y = df$rt_2019 + df$rt_2020 + df$rt_2021 + df$rt_2022 + df$rt_2023,
                 x = df$d2c_18,  all = TRUE, cluster = df$STATE, covs = z_controls)
summary(srdd3)


#Without controls but standard errors clustered by state.
srdd4 <- rdrobust(y = df$rt_2019 + df$rt_2020 + df$rt_2021 + df$rt_2022 + df$rt_2023,
                 x = df$d2c_18,  all = TRUE, cluster = df$STATE)
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
