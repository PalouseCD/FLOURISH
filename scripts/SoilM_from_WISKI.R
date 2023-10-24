# Gerrit Bass May 2023
# Code to compile FLOURISH data files coming out of WISKI into a single, cleaned, dataframe



library(plyr)
library(tidyverse)
library(lubridate)


# make a list of all your files
data_files <- grep(list.files('./data/from_WISKI'), pattern='.log', invert=TRUE, value=TRUE)


# function to clean datafiles and pull info about them from their filenames
FLOURISH_manager <- function(data){

#pulling variables from file name
Site<- sapply(strsplit(data,"_"), `[`, 1)
Param <- sapply(strsplit(data,"_"), `[`, 2)
Depth <- sapply(strsplit(data,"_"), `[`, 3) %>% 
  str_remove('.csv')

#bringing in data and formatting it
test_data <- read_csv(file = paste0('./data/from_WISKI/',data), skip = 15,locale=locale(encoding="latin1"), name_repair = 'check_unique') %>% 
  mutate(datetime = mdy_hms(paste(Date, Time))) %>% 
  select(5,3) %>% 
  mutate(Site = Site,
         Param = Param,
         Depth = Depth) %>% 
  dplyr::rename(Value = 2) %>% 
  select(1,3,4,5,2)
} 

# applying function to list of files and combining them all into one dataframe
FLOURISH_MASTER <- lapply(data_files,FLOURISH_manager) %>% 
  bind_rows()

#----------------------------------------------------------------------------------------------
#SUBSETTING AND GRAPHING DATA

name <- "ZENFL-CC01"

flourish_by_name <- FLOURISH_MASTER %>% 
  subset(Site == name)


# soil moisture graph
SoilMGraph <- flourish_by_name %>%
  subset(Param == 'SoilM') %>% 
  ggplot(aes(x = datetime, y = Value, color = Depth, group = Depth))+
  geom_line()+ #stat and fun.y are to set up stat summary so points can be connected despite being discrete data
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  labs(x = 'Date',
       y = 'Soil Moisture (%)')+
  theme_minimal()

SoilMGraph

#soil temperature graph
SoilTGraph <- flourish_by_name %>%
  subset(Param == 'SoilT') %>% 
  ggplot(aes(x = datetime, y = Value, color = Depth, group = Depth))+
  geom_line()+ #stat and fun.y are to set up stat summary so points can be connected despite being discrete data
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  labs(x = 'Date',
       y = 'Soil Temperature (°C)')+
  theme_minimal()

SoilTGraph

 


