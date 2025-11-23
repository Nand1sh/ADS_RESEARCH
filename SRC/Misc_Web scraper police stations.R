# load packages
library(dplyr)
library(rvest)
library(RSelenium)
library(netstat)

# create dataframe
policestations_table <- data.frame()

# start the server 
binman::list_versions("chromedriver")

rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "116.0.5845.96",
                             verbose = FALSE,
                             port = free_port())

# create a client object
remDr <- rs_driver_object$client

# open a client sided browser
remDr$open()

# navigate the webpage
remDr$navigate("https://passportindia.gov.in/AppOnlineProject/online/LocatePoliceStation")


# webscraper
for (state in c("3","6","8","17","22","23","28","34","38","19")) {
  
  ELEMstates <- remDr$findElement(using = "css selector",
                                  value = paste0("#state > option:nth-child(",state,")"))
  ELEMstates$clickElement()
  
  source_state <- remDr$getPageSource()[[1]]
  
  # get list of districts
  districts <- read_html(source_state) %>% 
    html_nodes("#district") %>% 
    html_nodes("option") %>%
    html_attr("value") %>% 
    .[-1]

  
  for (district in 2:(length(districts)+1)) {
    ELEMdistricts <- remDr$findElement(using = "css selector",
                                       value = paste0("#district > option:nth-child(",district,")"))
    ELEMdistricts$clickElement()
    
    
    GObutton <- remDr$findElement(using = "css selector",
                                  value = "#LocatePSAction_search")
    
    GObutton$clickElement()
    
    
    ps_t <- remDr$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_table() %>% 
      .[[17]]
    
    
    Nextbuttonstatus <- remDr$findElements(using = "link text",
                                     value = "Next")
    
    if (length(Nextbuttonstatus) != 0) {
      
      while (length(Nextbuttonstatus) != 0) {
        
        Nextbutton <- remDr$findElement(using = "link text", value = "Next")
  
        Nextbutton$clickElement()
        
        
        ps_t_n <- remDr$getPageSource()[[1]] %>% 
          read_html() %>% 
          html_table() %>% 
          .[[17]]
        
        ps_t <- rbind(ps_t,ps_t_n)
        
        
        Nextbuttonstatus <- remDr$findElements(using = "link text",
                                               value = "Next")
        
      }
      
    }
    
    ps_t <- ps_t[-c(1)]
    ps_t <- cbind(as.character(state), ps_t)
    
    policestations_table <- rbind(policestations_table, ps_t)
    
  }
  
  print(paste(state))
  
}

remDr$close()

# rename columns
colnames(policestations_table) <- c("State", "Police Station", "Police District")

# rename states
policestations_table$State[policestations_table$State =="3"] = "Andhra Pradesh"
policestations_table$State[policestations_table$State =="6"] = "Bihar"
policestations_table$State[policestations_table$State =="8"] = "Chhattisgarh"
policestations_table$State[policestations_table$State =="17"] = "Jharkhand"
policestations_table$State[policestations_table$State =="22"] = "Madhya Pradesh"
policestations_table$State[policestations_table$State =="23"] = "Maharashtra"
policestations_table$State[policestations_table$State =="28"] = "Odisha"
policestations_table$State[policestations_table$State =="34"] = "Telangana"
policestations_table$State[policestations_table$State =="38"] = "West Bengal"
policestations_table$State[policestations_table$State =="19"] = "Kerala"

# save dataset
write.csv(policestations_table,"DATA/RAW_DATA/LAW_ENFORCEMENT/POLICE_STATIONS_SUBDISTRICT.csv")
