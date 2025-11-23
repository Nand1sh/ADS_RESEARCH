#Reading event level violence data from 2016 till 14-05-2023.
acled <- read.csv("DATA/ACLED/Violence_master.csv")

#Filtering for required 11 states.
acled <- acled[acled$admin1 %in% c("Andhra Pradesh","Uttar Pradesh", "Bihar", "Chhattisgarh",
                                   "Jharkhand", "Madhya Pradesh", "Maharashtra",
                                   "Odisha","Telangana","West Bengal", "Kerala"), ]

#Filtering for events with violence.
#Do not run this chunk for main results.
# acled <- acled[acled$fatalities != 0, ]

#Separating violence data by event type.
erv <- acled[acled$event_type == "Explosions/Remote violence", ]
riots <- acled[acled$event_type == "Riots", ] 
vac <- acled[acled$event_type == "Violence against civilians", ] 
battles <- acled[acled$event_type == "Battles", ] 
protests <- acled[acled$event_type == "Protests", ]
stratdev <- acled[acled$event_type == "Strategic developments", ]

#Writing above filtered datasets.
write.csv(battles,"DATA/ACLED/Battles_ACLED.csv")
write.csv(erv,"DATA/ACLED/Explosions_Remoteviolence_ACLED.csv")
write.csv(riots,"DATA/ACLED/Riots_ACLED.csv")
write.csv(vac,"DATA/ACLED/Violence_against_civilians_ACLED.csv")
write.csv(protests,"DATA/ACLED/Protests_ACLED.csv")
write.csv(stratdev,"DATA/ACLED/Strategic_developments_ACLED.csv")
