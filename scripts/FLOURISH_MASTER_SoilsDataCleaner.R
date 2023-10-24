# FLOURISH Data Cleaning and Compiling Script
## This script cleans and compiles all bulk density, infiltration and soil sampling
## data into one csv for analysis purposes. It also exports separate csvs of chemical, biological
## and physical data to shiny app folders ready for deployment.

library(tidyr)
library(readr)
library(dplyr)
library(lubridate)
library(ggthemes)
library(plotly)
library(here)
library(wesanderson)
library(janitor)
library(stringr)
library(tidyverse)
library(data.table)
library(janitor)

#-------------------------------------------------------------------------------------------------------------------------------
### BULK DENSITY DATA CLEANING ###

# Reading in data and removing unneeded rows
bddata <- read_csv(here("data","FLOURISH_BulkDensityData_2023.csv"))
bddata <- clean_names(bddata)
bddata <- bddata[,c(6,3,2,4,5,7,8)]
bddata$sample_date <- mdy(bddata$sample_date)
bddata$sample_date <- year(bddata$sample_date)

#Calculations

#calculating Gravametric Soil Moisture
bddata$grav_soil_moisture <- round(((bddata$wet_weight_g - bddata$soil_tin_weight) - (bddata$dry_weight_g - bddata$soil_tin_weight)) /(bddata$dry_weight_g - bddata$soil_tin_weight),2)

#calculating Volumetric Soil Moisture
bddata$vol_soil_moisture <- round(bddata$grav_soil_moisture * bddata$bulk_density_g_cm3/1,2)

#calculating degree of saturation
bddata$degree_sat <- round(bddata$vol_soil_moisture/(1-(bddata$bulk_density_g_cm3/2.65))*100,2)


#checking to see if there are outliers in bulk density data
p <- bddata %>%
  ggplot(aes(bulk_density_g_cm3))+
  geom_histogram()
ggplotly(p)

summary(bddata[,7:10])

#Remove outliers - THIS WILL NEED TO BE CHANGED MANUALLY
#finding the row number of the outlier 
which(bddata$bulk_density_g_cm3 == 0.26)

#removing the outlier based on row number
bddata <- bddata[-23,]

#Filtering out data (rows) with producers no longer participating
bddata <- subset(bddata, bddata$field_id != "THRFL-CC01" & bddata$field_id != "THRFL-BAU01")

#Renaming parameters
bddata <- rename(bddata,
                 "Bulk Density (g/cm3)"="bulk_density_g_cm3",
                 "Volumetric Soil Moisture"="vol_soil_moisture",                         
                 "Gravametric Soil Moisture"="grav_soil_moisture",
                 "Degrees of Saturation" = "degree_sat"
)

#Filtering out unneeded columns
bddata1 <- bddata[,c(2,7:10)]

#Creating long dataframe
lbd <- bddata1 %>% 
  pivot_longer(!field_id,
               names_to = "Parameter",
               values_to = "Value"
              )
lbd <- cbind(lbd, "Year" = bddata$sample_date)    # Adding year

#-----------------------------------------------------------------------------------------------------------------------------------------

### INFILTRATION DATA CLEANING ###

#Reading in data
Infil <- read_csv(here("data", "Infiltration Rates_Summer2023.csv"))

# Cleaning Infiltration Data and converting to in/hr
Infil <- Infil[,c(1:4)]
Infil <- Infil %>%
  rename("field_id" = "Site ID")
Infil2 <- Infil[Infil$test == 2,]
Infil2$time[Infil2$time == ">15:00"] <- "30:00"   # Replacing >15:00 values with 30:00
Infil2$time <-  gsub(pattern = ">", replacement = "", (Infil2$time))
Infil2$time <- str_replace(Infil2$time, "^0", "")
Infil2$time <- lubridate::ms(Infil2$time)
Infil2$time <- period_to_seconds(Infil2$time)
Infil2$time <- Infil2$time/3600
Infil2 <- Infil2 %>%
  rename("Infiltration (In/Hr)" = "time")
Infil2$field_id <- str_sub(Infil2$field_id, end = -3) # removing A, B, and C from field id
Infil2 <- subset(Infil2, select = -c(test)) #removing test number column

#Filtering out data (rows) with producers no longer participating
Infil2 <- subset(Infil2, Infil2$field_id != "THRFL-CC01" & Infil2$field_id != "THRFL-BAU01")

# Filtering for only needed columns
Infil3 <- Infil2[,c(1:2)]


#Transforming to long
linfil <- Infil3 %>%
  pivot_longer(!field_id,
               names_to = "Parameter",
               values_to = "Value")
linfil <- cbind(linfil, "Year" = Infil2$year)    # Adding year
#------------------------------------------------------------------------------------------------------------------------------------------

### SOILS LAB DATA CLEANING ###

#Creating list of csv files in data folder and combining into one dataframe  
  labdata <-
  list.files(path = 'R:/_04_Project_Data/R/FLOURISH/data/From_RegenAg/', pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv)
labdata <- rbindlist(labdata, fill = TRUE)
labdata <- labdata[rowSums(is.na(labdata)) !=ncol(labdata),]

# Filtering columns
labdata <- labdata[,-c(1:3,5:9,11:15,50)]

# Renaming columns
labdata <- rename(labdata,
               "sample_date" = "Date Recd",
               "field_id"="Field ID",
               "pH"="Soil pH 1:1",                         
               "Modified WDRF BpH"="Wdrf Buffer",
               "Conductivity (dS/m)"="1:1 Electrical Conductivity, mmho/cm",
               "Excess Lime"="Excess Lime",
               "Organic Matter"="Organic Matter, % LOI",                
               "Nitrate (PPM)"="1N KCl-Nitrate, ppm NO3-N",
               "Nitrate (LBs)"="1N KCl-Nitrate, lbs/A N",             
               "Phosphate (PPM)"="M3-Phosphate, ppm PO4-P",
               "Sulfer (PPM)"="M3-Sulfur, ppm S",                     
               "Potassium (PPM)"="NH4OAc-Potassium, ppm K",
               "Calcium (PPM)"="NH4OAc-Calcium, ppm Ca",              
               "Magnesium (PPM)"="NH4OAc-Magnesium, ppm Mg",
               "Sodium (PPM)"="NH4OAc-Sodium, ppm Na",               
               "Zinc (PPM)"="DTPA-Zinc, ppm Zn",
               "Iron (PPM)"="DTPA-Iron, ppm Fe",                    
               "Manganese (PPM)"="DTPA-Manganese, ppm Mn",
               "Copper (PPM)"="DTPA-Copper, ppm Cu",                  
               "Boron (PPM)"="H2O-Boron, ppm B",
               "Ammonium (PPM)"="Ammonium, ppm NH4-N",                  
               "Cation Exchange Capacity (meq/100g)"="CEC, meq/100g",
               "Base Saturation"="Base Saturation, %",                   
               "Hydrogen Saturation"="Hydrogen, % Sat",
               "Calcium Saturation"="Calcium, % Sat",                       
               "Magnesium Saturation"="Magnesium, % Sat",
               "Potassium Saturation"="Potassium, % Sat",                     
               "Sodium Saturation"="Sodium, % Sat",
               "Total Organic Carbon"="Total Organic Carbon, % C",            
               "β.glucosidase enzyme"="β-glucosidase, µg pNP g¯¹ h¯¹",
               "Permanganate Oxidizable Carbon (PPM)"="Permanganate Oxidizable Carbon, ppm C",
               "Soil Respiration (PPM)"="Soil Respiration, ppm CO2-C",
               "Ace Protein"="ACE Protein, g ACE kg¯¹",                          
               "Macroaggregates"="Macroaggregates, % >0.25mm",
               "Microaggregates"="Microaggregates, % <0.25, >0.053mm",   
               "Total Aggregates"="Total Aggregates, %")

#Filtering out data (rows) with producers no longer participating
labdata <- subset(labdata, labdata$field_id != "THRFL-CC01" & labdata$field_id != "THRFL-BAU01")

# Making all data numeric
labdata$`Excess Lime` <- ifelse(labdata$`Excess Lime` == "HIGH", 1, 0) #NEED TO TURN BACK INTO CHR AFTER/BEFORE CALCULATIONS
labdata$`Hydrogen Saturation` <- ifelse(labdata$`Hydrogen Saturation` == "-", NA, labdata$`Hydrogen Saturation`) %>%
  as.numeric()
labdata$`Permanganate Oxidizable Carbon (PPM)` <-  gsub(pattern = "<", replacement = "", (labdata$`Permanganate Oxidizable Carbon (PPM)`)) %>%
  as.numeric()
labdata$Microaggregates <-  gsub(pattern = "<", replacement = "", (labdata$Microaggregates)) %>%
  as.numeric()
labdata$sample_date <- mdy(labdata$sample_date)
labdata$sample_date <- year(labdata$sample_date)

# Use this piece of code when we don't have ACE Protein (or other analysis) results back yet
#labdata[labdata == "N/A"] <- NA #replacing chr N/A data with NA - this is only because we didn't have this data yet (and so we can pivot longer)
#labdata$`Ace Protein` <- as.numeric(labdata$`Ace Protein`)

# # Filtering for only needed columns
labdata1 <- labdata[,c(2:36)]

# Reshaping dataframe from wide to long
llab <- labdata1%>%
  pivot_longer(cols=!field_id, names_to = "Parameter",values_to="Value")
llab <- cbind(llab, "Year" = labdata$sample_date)    # Adding year 

#------------------------------------------------------------------------------------------------------------------------------------------ 
### DOMINANT SOIL TYPE DATA ###

# Reading in data
DST <- read_csv(here("data", "DomSoilTypes_2023.csv"))
DST <- clean_names(DST)

DST <- DST[,c("field_id","description", "year")] #removing unneeded columns
DST <- DST %>%
  rename("Year" = "year",
         "soil_type" = "description")
DST <- DST[!is.na(DST$soil_type),] #removing producers we didn't sample

#------------------------------------------------------------------------------------------------------------------------------------------
### COMBINING DATA ###

# Combining data into csv for analysis
datajoin <- rbind(lbd,linfil)
MASTER_SoilsData <- rbind(datajoin,llab)

#Fixing the mistake in Eirksen Field_id and the Zenner cc field id errors
MASTER_SoilsData <- MASTER_SoilsData %>% 
  mutate(field_id =case_match(field_id, 
                              "ERIFL-CC01" ~ "EIRFL-CC01",
                              "ERIFL-BAU01" ~ "EIRFL-BAU01",
                              "ZENFL-CC02" ~ "ZENFL-CC01",
                              "ZENFL-CC03" ~ "ZENFL-CC01",
                              .default = field_id)) %>% 
  arrange(field_id)


MASTER_SoilsData <- merge(MASTER_SoilsData, DST, by = c("field_id", "Year"), all = TRUE)

#Adding management type
MASTER_SoilsData <- MASTER_SoilsData %>%
  mutate(Management = case_when(
    endsWith(field_id, "BAU01") ~ "Business as Usual",
    endsWith(field_id, "CC01") ~ "Cover Crop",
    endsWith(field_id, "CC02") ~ "Cover Crop",
    ))

# set precip zones
LOW <- c('STUBBS','GREENE','ZIMMER','MILLER','MCKAY','DEWALD')
MED <- c('DOBBINS','APPEL','SCHEIBE','ELLIOTT','BISHOP','SCHUSTER','ERIKSEN')
HIGH <- c('OLESEN','ZENNER','BATES','DRUFFEL','ZAKARISON','ECKHART','RIDDLE')

# Adding producer last name
MASTER_SoilsData <- MASTER_SoilsData %>%
  mutate(producer_name = case_when(
    startsWith(field_id, "BIS") ~ "BISHOP",
    startsWith(field_id, "ZAK") ~ "ZAKARISON",
    startsWith(field_id, "APP") ~ "APPEL",
    startsWith(field_id, "MIL") ~ "MILLER",
    startsWith(field_id, "ZIM") ~ "ZIMMER",
    startsWith(field_id, "WOL") ~ "WOLF",
    startsWith(field_id, "DRU") ~ "DRUFFEL",
    startsWith(field_id, "MCK") ~ "MCKAY",
    startsWith(field_id, "BRE") ~ "BREWER",
    startsWith(field_id, "SCH") ~ "SCHEIBE",
    startsWith(field_id, "GRE") ~ "GREENE",
    startsWith(field_id, "CHU") ~ "SCHUSTER",
    startsWith(field_id, "ELL") ~ "ELLIOTT",
    startsWith(field_id, "DOB") ~ "DOBBINS",
    startsWith(field_id, "EIR") ~ "ERIKSEN",
    startsWith(field_id, "DEW") ~ "DEWALD",
    startsWith(field_id, "ZEN") ~ "ZENNER",
    startsWith(field_id, "ECK") ~ "ECKHART",
    startsWith(field_id, "OLE") ~ "OLESEN",
    startsWith(field_id, "BAT") ~ "BATES",
    startsWith(field_id, "MOR") ~ "MORTER",
    startsWith(field_id, "DOU") ~ "POOLE",
    startsWith(field_id, "RID") ~ "RIDDLE",
    startsWith(field_id, "STU") ~ "STUIVENGA",
    startsWith(field_id, "STB") ~ "STUBBS",)) %>% 
  mutate(precip_zone = case_when(
    producer_name %in% LOW  ~ "LOW",
    producer_name %in% MED  ~ "MED",
    producer_name %in% HIGH  ~ "HIGH"
  ))

# Finding average of field
#data <- data %>% 
#  group_by(field_id,Parameter,Year) %>% 
#  summarise(FieldAv = mean(Value, na.rm = TRUE)
# ungroup()

write_csv(MASTER_SoilsData,(here("data", "FLOURISH_MASTER_SoilsData.csv")))

#-----------------------------------------------------------------------------------------------------------------------------
 
 
### Shiny App Data Prep ###

# This function finds the mean and sd for each field, precipitation zone and whole project
# Data is then exported to corresponding shiny app folders

FLOURISH_combine <- function(data){
  
  # Finding average of field
  sum_data <- data %>% 
    group_by(field_id,Parameter,Year) %>% 
    summarise(FieldAv = mean(Value, na.rm = TRUE),
              FieldAv_sd = sd(Value, na.rm = TRUE)) %>%
    ungroup()
  
  #Adding management type
  sum_data <- sum_data %>%
    mutate(Management = case_when(
      endsWith(field_id, "BAU01") ~ "Business as Usual",
      endsWith(field_id, "CC01") ~ "Cover Crop",
      endsWith(field_id, "CC02") ~ "Cover Crop"
    ))
  
  # Finding average of project
  sum_data <- sum_data %>% group_by(Management, Parameter,Year) %>%
    mutate(ProjAve = mean(FieldAv, na.rm = TRUE),
           ProjAv_sd = sd(FieldAv, na.rm = TRUE)) %>%
    ungroup()
  
  # set precip zones
  LOW <- c('STUBBS','GREENE','ZIMMER','MILLER','MCKAY','DEWALD')
  MED <- c('DOBBINS','APPEL','SCHEIBE','ELLIOTT','BISHOP','SCHUSTER','ERIKSEN')
  HIGH <- c('OLESEN','ZENNER','BATES','DRUFFEL','ZAKARISON','ECKHART','RIDDLE') 
  
  # Adding producer last name
  sum_data <- sum_data %>%
    mutate(producer_name = case_when(
      startsWith(field_id, "BIS") ~ "BISHOP",
      startsWith(field_id, "ZAK") ~ "ZAKARISON",
      startsWith(field_id, "APP") ~ "APPEL",
      startsWith(field_id, "MIL") ~ "MILLER",
      startsWith(field_id, "ZIM") ~ "ZIMMER",
      startsWith(field_id, "WOL") ~ "WOLF",
      startsWith(field_id, "DRU") ~ "DRUFFEL",
      startsWith(field_id, "MCK") ~ "MCKAY",
      startsWith(field_id, "BRE") ~ "BREWER",
      startsWith(field_id, "SCH") ~ "SCHEIBE",
      startsWith(field_id, "GRE") ~ "GREENE",
      startsWith(field_id, "CHU") ~ "SCHUSTER",
      startsWith(field_id, "ELL") ~ "ELLIOTT",
      startsWith(field_id, "DOB") ~ "DOBBINS",
      startsWith(field_id, "ERI") ~ "ERIKSEN",
      startsWith(field_id, "DEW") ~ "DEWALD",
      startsWith(field_id, "ZEN") ~ "ZENNER",
      startsWith(field_id, "ECK") ~ "ECKHART",
      startsWith(field_id, "OLE") ~ "OLESEN",
      startsWith(field_id, "BAT") ~ "BATES",
      startsWith(field_id, "MOR") ~ "MORTER",
      startsWith(field_id, "DOU") ~ "POOLE",
      startsWith(field_id, "RID") ~ "RIDDLE",
      startsWith(field_id, "STU") ~ "STUIVENGA",
      startsWith(field_id, "STB") ~ "STUBBS",)) %>% 
    mutate(precip_zone = case_when(
      producer_name %in% LOW  ~ "LOW",
      producer_name %in% MED  ~ "MED",
      producer_name %in% HIGH  ~ "HIGH"
    ))
  
  # Finding precip zone average
  sum_data <- sum_data %>% group_by(precip_zone, Management, Parameter, Year) %>%
    mutate(PrecipZoneAv = mean(FieldAv, na.rm = TRUE),
           PrecipZoneAv_sd = sd(FieldAv, na.rm = TRUE)) %>%
    ungroup()

  sum_data <- sum_data %>%
    mutate(Management = case_when(
      endsWith(field_id, "BAU01") ~ "Business as Usual",
      endsWith(field_id, "CC01") ~ "Cover Crop",
      endsWith(field_id, "CC02") ~ "Cover Crop 2"
    ))
  
}


# Running function on BD, Infil and Lab data
bddataclean <- FLOURISH_combine(lbd)
infildataclean <- FLOURISH_combine(linfil)
labdataclean <- FLOURISH_combine(llab)

# Merging dominant soil type with data
bddataclean <- merge(bddataclean, DST, by = c("field_id", "Year"), all = TRUE)
infildataclean <- merge(infildataclean, DST, by = c("field_id", "Year"), all = TRUE)
labdataclean <- merge(labdataclean, DST, by = c("field_id", "Year"), all = TRUE)

# Joining BD and Infil data
infilbdclean <- rbind(bddataclean, infildataclean)

### EXPORTING CSVS FOR SHINY APPS ###

# Creating csv for Physical Data
phys_data <- subset(labdataclean, subset = Parameter %in% c(	
  'Total Aggregates',	
  'Microaggregates',
  'Macroaggregates'))
# Binding with infiltration and bulk density data
SoilPhysData <- rbind(infilbdclean, phys_data)
write_csv(SoilPhysData,(here("shiny", "FLOURISH_PhysicalData", "data", "Soil_PhysicalData.csv")))


# Creating csv for Chemical Data
# Filtering lab data to only include chemical parameters
SoilChemData <- subset(labdataclean, labdataclean$Parameter != 'Total Aggregates'
                       & labdataclean$Parameter !='Microaggregates'
                       & labdataclean$Parameter !='Macroaggregates'
                       & labdataclean$Parameter !='β.glucosidase enzyme'
                       & labdataclean$Parameter !='Soil Respiration (PPM)'
                       & labdataclean$Parameter !='Permanganate Oxidizable Carbon (PPM)')
write_csv(SoilChemData,(here("shiny", "FLOURISH_ChemicalData", "data", "Soil_ChemicalData.csv")))


# Creating csv for Biological Data
SoilBioData <- subset(labdataclean, subset = Parameter %in% c(
  'β.glucosidase enzyme',
  'Permanganate Oxidizable Carbon (PPM)',
  'Soil Respiration (PPM)',
  'Organic Matter',
  'Ace Protein'))
write_csv(SoilBioData, (here("shiny", "FLOURISH_BiologicalData", "data", "Soil_BiologicalData.csv")))


#----------------------------------------------------------------------------------------------------------------------------------------


### GRAPHING DATA ###
#read in data
data <- read_csv(here("data","FLOURISH_MASTER_SoilsData.csv"))

### Test Graphs ###

om <- data %>% 
  filter(Parameter == "Organic Matter")
omplot <- ggplot(om,aes(Management,Value))+
  geom_boxplot()+
  facet_grid(.~Parameter)
  ggplotly(omplot)
  
  
omavg <- om %>% 
  group_by(field_id,Parameter,Year) %>% 
  mutate(avg = mean(Value, na.rm = TRUE),
            stdev = sd(Value, na.rm = TRUE)) %>%
  ungroup() %>% 
  distinct(avg, stdev, .keep_all = TRUE)
  
ombar <- ggplot(omavg,aes(prod, FieldAv, color = Management, fill = Management))+
  geom_bar(stat = "identity",position = position_dodge())+
  geom_errorbar(aes(ymin = FieldAv-FieldAv_sd, ymax=FieldAv+FieldAv_sd))
  facet_grid(.~Parameter)
  
  ggplotly(ombar)


  
  
  
# Histograms

par <- data %>%
  filter(Parameter == "Volumetric Soil Moisture")
parplot <- ggplot(par, aes(Value))+
    geom_histogram()
  ggplotly(parplot)
  
  
###
# box plots
bp <- data %>% 
  ggplot(aes(factor(field_id),bulk_density_g_cm3, color = Management))+
  geom_boxplot()

ggplotly(bp)



### BAR GRAPHS ###

# Subsetting Data
#enter producer last name and soil parameter here to generate graphs for only that producer and parameter
Producer_name = "APPEL"
param = "pH"

# Subsetting by producer and parameter
graphdata <- Bd_Infil_Data %>%
  subset(producer_name == Producer_name) %>%
  subset(Parameter == param)


# Creating bar graphs

soilsgraph <- graphdata %>% plot_ly()

soilsgraph <- soilsgraph %>% 
  add_trace(x = ~Management,
            y = ~FieldAv, type = 'bar', name = graphdata$Management, error_y = ~list(
              array = FieldAv_sd,
              color = '#000000')) %>%
  add_trace(x = ~Management,
            y = ~PrecipZoneAv, type = 'bar', name = 'Average Across Precip Zone', error_y = ~list(
              array = PrecipZoneAv_sd,
              color = '#000000')) %>%
  add_trace(x = ~Management,
            y = ~ProjAve, type = 'bar', name = 'Average Across Project', error_y = ~list(
              array = ProjAv_sd,
              color = '#000000'))

soilsgraph <- soilsgraph %>% layout(yaxis = list(title = param), barmode = 'group')
soilsgraph

print(unique(graphdata$Parameter))
