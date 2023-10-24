## Script for formatting FLOURISH soil Moisture data to go into WISKI
## Written by Gerrit Bass May 2023


# load in libraries
library(tidyverse)
library(readxl)
library(lubridate)# doesn't load with the rest of the tidyverse


# create list of Soil Moisture data files
SoilM_files <- list.files(path = "./data/SoilMoisture_FILE_DROP", pattern = "*.csv")


#function to be used later to move files to archive
move_to_archive <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)}


#function to clean and move files
SoilM_cleaner <- function(data){

  filename <- sapply(strsplit(data,".csv"),"[",1) #removing ".xls" from file name for use in file naming

  SoilM_data <- read_csv(file = paste("./data/SoilMoisture_FILE_DROP/",data,sep = ""))

  cleaned_data <- SoilM_data %>% 
    select(1,9,3:8) %>% 
    rename("siteID" = 2,
          "Depth" = 3,
          "SoilM" = 4,
          "SoilT" = 5,
          "perm" = 6,
          "EC" = 7,
          "pwec" = 8) %>% 
    mutate(Depth = case_when(Depth == paste(siteID,"-","12", sep = "") ~ 12,
                             Depth == paste(siteID,"-","36", sep = "") ~ 36)) %>% 
    pivot_longer(cols = 4:8) %>% 
    rename("Param" = 4)

  write_csv(cleaned_data,paste("./data/to_WISKI/",filename,".csv",sep = ""))

  move_to_archive(from = paste("./data/SoilMoisture_FILE_DROP/", filename,".csv", sep=""),
                  to = paste("./data/SoilMoisture_FILE_DROP/archive/", filename, ".csv", sep=""))
  
}

# To Run function on list of files
lapply(SoilM_files,SoilM_cleaner)


