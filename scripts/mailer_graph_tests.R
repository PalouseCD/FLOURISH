
mutate(value = ((9/5)*value + 32))


library(tidyverse)
library(timetk)

FLOURISH_MASTER <- read_csv(file ='R:\\_04_Project_Data\\R\\FLOURISH\\outputs\\FLOURISH_MASTER.csv' )

#dataframe with daily average soilM at 12 and 36 inches
proj_avg_soilM <- FLOURISH_MASTER %>% 
  subset(Param == 'SoilM') %>% 
  drop_na() %>%
  group_by(Depth) %>% 
  summarize_by_time(
    .date_var = datetime,
    .by = "day",
    value = mean(Value)) %>% 
  mutate(depth = as.factor(Depth))

proj_avg_soilM_12 <- proj_avg_soilM %>% 
  subset(depth == 12)

proj_avg_soilM_36 <- proj_avg_soilM %>% 
  subset(depth == 36)

last_name = "APPEL"

# creates date range to go into graph based on producer and their data range
prod_range <- FLOURISH_MASTER %>%
  subset(producer_name == last_name) %>%
  summarise(across(datetime, list(min = min, max = max)))

#graph
SoilMGraph <- FLOURISH_MASTER %>%
  subset(producer_name == last_name) %>% 
  subset(Param == 'SoilM') %>% 
  mutate(Depth = as.factor(Depth)) %>% 
  ggplot()+
  geom_line(aes(x = datetime, y = Value, color = Depth, group = Depth))+ 
  geom_line(data = proj_avg_soilM_12,linetype = "dashed" , aes(x = datetime, y = value, color = "12 (project average)"))+
  geom_line(data = proj_avg_soilM_36,linetype = "dashed", aes(x = datetime, y = value, color = "36 (project average)"))+
  labs(x = "",
       y = 'Soil Moisture (%)',
       color = "Depth (in)")+
  scale_color_manual(values=c("12"= "#E69F00","36" = "#56B4E9","12 (project average)" = "#E69F00","36 (project average)" = "#56B4E9"))+
  theme_minimal()+
  scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks", limits = c(prod_range$datetime_min,prod_range$datetime_max))+
  scale_y_continuous(limits = c(0,max(FLOURISH_MASTER$Value)+10))

ggplotly(SoilMGraph)

SoilMGraph


