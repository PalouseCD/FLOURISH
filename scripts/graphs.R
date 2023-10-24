#enter producers name here to generate this document for only that producer
field_name = "CHUFL-CC01"
last_name = "SCHUSTER"

# code here will pull in file with all the FLOURISH data we have pulled out of WISKI. Then it  will sort by whatever fieldID we give it to isolate single producer data. Then it will use the field ID to populate the producers first name and a FIRST_DATE and LAST DATE.

library(tidyverse)
library(lubridate)
library(ggthemes)
library(plotly)
library(here)
library(wesanderson)

FLOURISH_MASTER <- read_csv(here("outputs","FLOURISH_MASTER.csv") )


#subsets data by producer
flourish_by_name <- subset(FLOURISH_MASTER, Site == field_name)


#soil Moisture graph
SoilMGraph <- FLOURISH_MASTER %>%
  subset(producer_name == last_name) %>% 
  subset(Param == 'SoilM') %>% 
  mutate(Depth = as.factor(Depth)) %>% 
  ggplot(aes(x = datetime, y = Value, color = Depth, group = Depth))+
  geom_line()+ #stat and fun.y are to set up stat summary so points can be connected despite being discrete data
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  labs(x = "",
       y = 'Soil Moisture (%)',
       color = "Depth (in)")+
  theme_minimal()+
  scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks")+
  scale_y_continuous(limits = c(0,max(FLOURISH_MASTER$Value)+10))

ggplotly(SoilMGraph)


SoilMGraph


#soil temperature graph
SoilTGraph <- FLOURISH_MASTER %>%
  subset(producer_name == last_name) %>% 
  subset(Param == 'SoilT') %>% 
  mutate(Depth = as.factor(Depth)) %>%
  #mutate(Value = ((9/5)*Value + 32)) %>% 
  ggplot()+
  geom_line(aes(x = datetime, y = Value, color = Depth, group = Depth))+ 
  #geom_line(data = proj_avg_soilT_12,linetype = "dashed" , aes(x = datetime, y = value, color = "12 (project average)"))+
  #geom_line(data = proj_avg_soilT_36,linetype = "dashed", aes(x = datetime, y = value, color = "36 (project average)"))+
  labs(x = "",
       y = 'Soil Temperature (°F)',
       color = "Depth (in)")+
  #scale_color_manual(values=c("12"= "#E69F00","36" = "#56B4E9","12 (project average)" = "#E69F00","36 (project average)" = "#56B4E9"))+
  theme_minimal()
  #scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks", limits = c(prod_range$datetime_min,prod_range$datetime_max))

SoilTGraph 


ggplotly(SoilTGraph)

#

wider <- pivot_wider(flourish_by_name, names_from = Param, values_from = Value,values_fn = mean)

SoilTGraph <- wider %>%
  mutate(Depth = as.factor(Depth)) %>%
  mutate(SoilT = ((9/5)*SoilT + 32)) %>%
  arrange(datetime) %>% 
  ggplot(aes(x = datetime, y = SoilT, color = Depth, group = Depth))+
  geom_line()+ #stat and fun.y are to set up stat summary so points can be connected despite being discrete data
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  labs(x = '',
       y = 'Soil Temperature (°F)',
       color = "Depth (in)")+
  theme_minimal()+
  scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks")


temp <- flourish_by_name %>% 
  subset(Param == "SoilT")


#### All plots/ryan's garbage ###

data <- "CHUFL-CC01_SoilT_12.csv"

#pulling variables from file name
Site<- sapply(strsplit(data,"_"), `[`, 1)
Param <- sapply(strsplit(data,"_"), `[`, 2)
Depth <- sapply(strsplit(data,"_"), `[`, 3) %>% 
  str_remove('.csv')



#bringing in data and formatting it
test_data <- read_csv(here("data","from_WISKI","CHUFL-CC01_SoilT_12.csv"), 
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
  select(1,3,4,5,2)


p <- ggplot(test_data,aes(datetime,Value))+
  geom_line()

ggplotly(p)





tempcheck <- test_data %>% 
  #filter(Param == "SoilT") %>%
  #mutate(Value = ((9/5)*Value + 32)) %>%
  #arrange(datetime) %>% 
  ggplot(aes(datetime, Value))+
  geom_line()+
  scale_color_manual(values= wes_palette("FantasticFox1", n = 1, type = c("continuous")))+
  labs(x = 'Date',
       y = 'Soil Temperature (°F)',
       color = "Data type")+
  #facet_grid(rows = vars(Depth))+
  theme_minimal()
ggplotly(tempcheck)


glimpse(FLOURISH_MASTER)
summary(FLOURISH_MASTER)

chu <- FLOURISH_MASTER %>% 
  filter(producer_name == "SCHUSTER")
  

chuavg <- chu %>% 
  group_by(datetime,Param,Depth) %>% 
  summarise(Avgvalue = mean(Value)) %>% 
  filter(Param == "SoilT") %>% 
    mutate(Avgvalue = ((9/5)*Avgvalue + 32))


chuavg$producer_name <- "SCHUSTER"
chuavg$Site <- "CHUFL-CC01"
chuavg <- chuavg[,c(1,6,2,3,4,5)]

chuavg <- chuavg %>% 
  rename(Value = Avgvalue)

chuavg$it <- "average"


temp2 <- FLOURISH_MASTER %>% 
  filter(Param == "SoilT" & producer_name == "SCHUSTER") %>%
  mutate(Value = ((9/5)*Value + 32))

temp2$it <- "Orignal"

all_temp <- bind_rows(chuavg,temp2)

tempcheck <- all_temp %>% 
  filter(Param == "SoilT") %>%
  mutate(Value = ((9/5)*Value + 32)) %>%
  arrange(datetime) %>% 
  ggplot(aes(datetime, Value, color = it))+
  geom_line()+
  scale_color_manual(values= wes_palette("FantasticFox1", n = 2, type = c("continuous")))+
  labs(x = 'Date',
       y = 'Soil Temperature (°F)',
       color = "Data type")+
  facet_grid(rows = vars(Depth))+
  theme_minimal()
ggplotly(tempcheck)


temp2g <- FLOURISH_MASTER %>% 
  filter(Param == "SoilT" & producer_name == "SCHUSTER") %>%
  mutate(Value = ((9/5)*Value + 32)) %>%
  arrange(datetime) %>% 
  ggplot(aes(datetime, Value, color = factor(Site)))+
  geom_line()+
  scale_color_manual(values= wes_palette("FantasticFox1", n = 21, type = c("continuous")))+
  labs(x = 'Date',
       y = 'Soil Temperature (°F)',
       color = "Site ID")+
  facet_grid(rows = vars(Depth))+
  theme_minimal()

ggplotly(temp2g)



temp2g <- FLOURISH_MASTER %>% 
  filter(Param == "SoilT") %>%
  mutate(Value = ((9/5)*Value + 32)) %>%
  arrange(datetime) %>% 
  ggplot(aes(datetime, Value, color = factor(Site)))+
  geom_line()+
  scale_color_manual(values= wes_palette("FantasticFox1", n = 21, type = c("continuous")))+
  labs(x = 'Date',
       y = 'Soil Temperature (°F)',
       color = "Site ID")+
  facet_grid(rows = vars(Depth))+
  theme_minimal()

ggplotly(temp2g)
  
smg <- FLOURISH_MASTER %>% 
  filter(Param == "SoilM") %>%
  arrange(datetime) %>% 
  ggplot(aes(datetime, Value, color = factor(Site)))+
  geom_line()+
  labs(x = 'Date',
       y = 'Soil Temperature (°F)',
       color = "Site ID")+
  facet_grid(rows = vars(Depth))
  

ggplotly(smg)

####### daily averages #######

FLOURISH_MASTER$date <- date(FLOURISH_MASTER$datetime)

day <- FLOURISH_MASTER %>% 
  group_by(Site,date,Depth,Param) %>% 
  summarise(value = mean(Value))
  

tempday <- day %>% 
  filter(Param == "SoilT") %>%
  mutate(value = ((9/5)*value + 32)) %>%
  arrange(date) %>% 
  ggplot(aes(date, value, color = factor(Site)))+
  geom_path()+
  scale_color_manual(values= wes_palette("FantasticFox1", n = 19, type = c("continuous")))+
  labs(x = 'Date',
       y = 'Soil Temperature (°F)',
       color = "Site ID")+
  facet_grid(rows = vars(Depth))+
  theme_minimal()

ggplotly(tempday)

smday <- day %>% 
  filter(Param == "SoilM") %>%
  arrange(date) %>% 
  ggplot(aes(date, value, color = factor(Site)))+
  geom_path()+
  scale_color_manual(values= wes_palette("Zissou1", n = 19, type = c("continuous")))+
  labs(x = 'Date',
       y = 'Soil Moisture (°F)',
       color = "Site ID")+
  facet_grid(rows = vars(Depth))


ggplotly(smday)
