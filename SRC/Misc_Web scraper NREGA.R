library(rvest)
library(dplyr)

nrega_table <- data.frame()

get_data = function(District_links) {
  repeat {
    try(page_district <- read_html(District_links))
    if(class(try(page_district <- read_html(District_links), silent = FALSE)) != "try-error"){
      break
    }
  }
  district_table <- page_district %>% html_nodes("table") %>% .[5] %>% html_table() %>% .[[1]]
  district_table <- district_table[-c(1)]
  district_table <- district_table[-c(1,2,3,4), ]
  district_name <- page_district %>% html_elements(xpath = '//*[@id="ContentPlaceHolder1_lbl_head"]/text()[2]') %>% html_text(., trim = TRUE)
  district_table <- cbind(as.character(district_name), district_table)
  district_table <- cbind(as.character(state), district_table)
  district_table <- cbind(as.character(year), district_table)
  colnames(district_table) <- 1:17
  print(paste("State:", state, "District:", district_name, "Year:", year))
  return(district_table)
}

for (state in c("ANDHRA%20PRADESH&state_code=02", "BIHAR&state_code=05", "CHHATTISGARH&state_code=33",
                "JHARKHAND&state_code=34", "MADHYA%20PRADESH&state_code=17", "MAHARASHTRA&state_code=18",
                "ODISHA&state_code=24","TELANGANA&state_code=36","WEST%20BENGAL&state_code=32", "KERALA&state_code=16")) {
  
  for (year in c("2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019",
                 "2019-2020", "2020-2021", "2021-2022", "2022-2023")) {
    
    State_links <- paste0("https://nreganarep.nic.in/netnrega/app_issue.aspx?page=s&lflag=eng&state_name=",state,"&fin_year=",year,"&source=national&Digest=m")
    
    repeat {
      
      try(page <- read_html(State_links))
      print(paste("Trying"))
      if(class(try(page <- read_html(State_links), silent = FALSE)) != "try-error"){
        print(paste("Success"))
        break
      }
    }
    
    District_links <- page %>% html_nodes("span a" ) %>%
      html_attr("href") %>% paste0("https://nreganarep.nic.in/netnrega/", .)
    
    District = sapply(District_links, FUN= get_data, USE.NAMES = FALSE)
    District2 = as.data.frame(District)
    
    for (i in 1:ncol(District2)) {
      nrega_table= rbind(nrega_table,as.data.frame(District2[,i]))
    }
    
  }
}

colnames(nrega_table) <- c("Serial Number", "Year", "State", "District", "Subdistrict", 
                           "Number of Jobcards Applied for", "Number of Jobcards Issued",
                           "Registered Workers SC", "Registered Workers ST", "Registered Workers Others",
                           "Registered Workers Total", "Registered Workers Women",
                           "Number of Active Jobcards", "Active Workers SC", "Active Workers ST", 
                           "Active Workers Others", "Active Workers Total", "Active Workers Women")

nrega_table$State[nrega_table$State =="ANDHRA%20PRADESH&state_code=02"] = "Andhra Pradesh"
nrega_table$State[nrega_table$State =="BIHAR&state_code=05"] = "Bihar"
nrega_table$State[nrega_table$State =="CHHATTISGARH&state_code=33"] = "Chhattisgarh"
nrega_table$State[nrega_table$State =="JHARKHAND&state_code=34"] = "Jharkhand"
nrega_table$State[nrega_table$State =="MADHYA%20PRADESH&state_code=17"] = "Madhya Pradesh"
nrega_table$State[nrega_table$State =="MAHARASHTRA&state_code=18"] = "Maharashtra"
nrega_table$State[nrega_table$State =="ODISHA&state_code=24"] = "Odisha"
nrega_table$State[nrega_table$State =="TELANGANA&state_code=36"] = "Telangana"
nrega_table$State[nrega_table$State =="WEST%20BENGAL&state_code=32"] = "West Bengal"
nrega_table$State[nrega_table$State =="KERALA&state_code=16"] = "Kerala"

write.csv(nrega_table,"DATA/RAW_DATA/NREGA/NREGA_SUBDISTRICT.csv")
