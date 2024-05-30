library(tidyverse)
library(lubridate)
library(here)
library(plotly)


# make a list of all your files
data_files <- grep(list.files('R:\\_04_Project_Data\\R\\FLOURISH\\data\\from_WISKI'), pattern='.log', invert=TRUE, value=TRUE)

# set precip zones
LOW <- c('STUBBS','GREENE','ZIMMER','MILLER','MCKAY','DEWALD')
MED <- c('DOBBINS','APPEL','SCHEIBE','ELLIOTT','BISHOP','SCHUSTER','ERIKSEN')
HIGH <- c('OLESEN','ZENNER','BATES','DRUFFEL','ZAKARISON','ECKHART','RIDDLE') 


# function to clean datafiles and pull info about them from their filenames
FLOURISH_manager <- function(data){
  
  #pulling variables from file name
  Site<- sapply(strsplit(data,"_"), `[`, 1)
  Param <- sapply(strsplit(data,"_"), `[`, 2)
  Depth <- sapply(strsplit(data,"_"), `[`, 3) %>% 
    str_remove('.csv')
  
  #bringing in data and formatting it
  test_data <- read_csv(file = paste0('R:/_04_Project_Data/R/FLOURISH/data/from_WISKI',"/",data), 
                        skip = 16,
                        col_names = c("Date","Time","Value","State of Value"),
                        na = "---")
  test_data$Date <- mdy(test_data$Date)
  test_data$datetime <- paste(test_data$Date,test_data$Time) 
  test_data$datetime <- ymd_hms(test_data$datetime)
  test_data <- test_data %>% 
    select(5,3) %>% 
    mutate(Site = Site,
           Param = Param,
           Depth = Depth) %>% 
    mutate(Value = as.double(Value)) %>% 
        select(1,3,4,5,2)
} 

# applying function to list of files and combining them all into one dataframe
FLOURISH_MASTER <- lapply(data_files,FLOURISH_manager) %>% 
  bind_rows()%>% 
  mutate(producer_name = case_when(
    startsWith(Site, "BIS") ~ "BISHOP",
    startsWith(Site, "ZAK") ~ "ZAKARISON",
    startsWith(Site, "APP") ~ "APPEL",
    startsWith(Site, "MIL") ~ "MILLER",
    startsWith(Site, "ZIM") ~ "ZIMMER",
    startsWith(Site, "WOL") ~ "WOLF",
    startsWith(Site, "DRU") ~ "DRUFFEL",
    startsWith(Site, "MCK") ~ "MCKAY",
    startsWith(Site, "BRE") ~ "BREWER",
    startsWith(Site, "SCH") ~ "SCHEIBE",
    startsWith(Site, "GRE") ~ "GREENE",
    startsWith(Site, "CHU") ~ "SCHUSTER",
    startsWith(Site, "ELL") ~ "ELLIOTT",
    startsWith(Site, "DOB") ~ "DOBBINS",
    startsWith(Site, "EIR") ~ "ERIKSEN",
    startsWith(Site, "DEW") ~ "DEWALD",
    startsWith(Site, "ZEN") ~ "ZENNER",
    startsWith(Site, "ECK") ~ "ECKHART",
    startsWith(Site, "OLE") ~ "OLESEN",
    startsWith(Site, "BAT") ~ "BATES",
    startsWith(Site, "MOR") ~ "MORTER",
    startsWith(Site, "DOU") ~ "POOLE",
    startsWith(Site, "RID") ~ "RIDDLE",
    startsWith(Site, "STU") ~ "STUIVENGA",
    startsWith(Site, "THR") ~ "THROOP",
    startsWith(Site, "STB") ~ "STUBBS",)) %>% 
  mutate(precip_zone = case_when(
    producer_name %in% LOW  ~ "LOW",
    producer_name %in% MED  ~ "MED",
    producer_name %in% HIGH  ~ "HIGH"
  ))

write_csv(FLOURISH_MASTER, file = "R:\\_04_Project_Data\\R\\FLOURISH\\data\\FLOURISH_MASTER.csv")
write_csv(FLOURISH_MASTER, file = "R:\\_04_Project_Data\\R\\FLOURISH\\shiny\\FLOURISH_mailer_test\\data\\FLOURISH_MASTER.csv")
write_csv(FLOURISH_MASTER, file = "R:\\_04_Project_Data\\R\\FLOURISH\\shiny\\FLOURISH_mailer_mobile\\data\\FLOURISH_MASTER.csv")


