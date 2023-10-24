library(tidyverse)





# LOW Precip Zone --------------------------------------------------------------------------------------------------------------------------------------------------

#dataframe with daily average soilM at 12 and 36 inches
low_precip_SoilM <- FLOURISH_MASTER %>% 
  subset(Param == 'SoilM' & precip_zone == "LOW") %>%
  drop_na() %>%
  group_by(Depth) %>% 
  summarize_by_time(
    .date_var = datetime,
    .by = "day",
    value = mean(Value)) %>% 
  mutate(depth = as.factor(Depth))

low_precip_SoilM_12 <- low_precip_SoilM %>% 
  subset(depth == 12)

low_precip_SoilM_36 <- low_precip_SoilM %>% 
  subset(depth == 36)

#graph 12 inch low precip
SoilMGraph12_low <- FLOURISH_MASTER %>%
  subset(Site != "ECKFL-CC02" ) %>% 
  subset(Param == 'SoilM' & Depth == 12 & precip_zone == "LOW") %>% 
  ggplot()+
  geom_line(aes(x = datetime, y = Value, color = producer_name, group = producer_name))+ 
  geom_line(data = low_precip_SoilM_12,linetype = "dashed" , aes(x = datetime, y = value, color = "12 (precip zone average)"))+
  labs(x = "",
       y = 'Soil Moisture (%)',
       color = "Depth (in)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks")

ggplotly(SoilMGraph12_low)

#graph 36 inch low precip
  subset(Site != "ECKFL-CC02" ) %>% 
  subset(Param == 'SoilM' & Depth == 36 & precip_zone == "LOW") %>% 
  ggplot()+
  geom_line(aes(x = datetime, y = Value, color = producer_name, group = producer_name))+ 
  geom_line(data = low_precip_SoilM_36,linetype = "dashed", aes(x = datetime, y = value, color = "36 (precip zone average)"))+
  labs(x = "",
       y = 'Soil Moisture (%)',
       color = "Depth (in)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks")

ggplotly(SoilMGraph36_low)




# MEDIUM Precip Zone --------------------------------------------------------------------------------------------------------------------------------------------------

#dataframe with daily average soilM at 12 and 36 inches
med_precip_SoilM <- FLOURISH_MASTER %>% 
  subset(Param == 'SoilM' & precip_zone == "MED") %>%
  drop_na() %>%
  group_by(Depth) %>% 
  summarize_by_time(
    .date_var = datetime,
    .by = "day",
    value = mean(Value)) %>% 
  mutate(depth = as.factor(Depth))

med_precip_SoilM_12 <- med_precip_SoilM %>% 
  subset(depth == 12)

med_precip_SoilM_36 <- med_precip_SoilM %>% 
  subset(depth == 36)

#graph 12 inch med precip
SoilMGraph12_med <- FLOURISH_MASTER %>%
  subset(Site != "ECKFL-CC02" ) %>% 
  subset(Param == 'SoilM' & Depth == 12 & precip_zone == "MED") %>% 
  ggplot()+
  geom_line(aes(x = datetime, y = Value, color = producer_name, group = producer_name))+ 
  geom_line(data = med_precip_SoilM_12,linetype = "dashed" , aes(x = datetime, y = value, color = "12 (precip zone average)"))+
  labs(x = "",
       y = 'Soil Moisture (%)',
       color = "Depth (in)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks")

ggplotly(SoilMGraph12_med)

#graph 36 inch med precip
SoilMGraph36_med <- FLOURISH_MASTER %>%
  subset(Site != "ECKFL-CC02" ) %>% 
  subset(Param == 'SoilM' & Depth == 36 & precip_zone == "MED") %>% 
  ggplot()+
  geom_line(aes(x = datetime, y = Value, color = producer_name, group = producer_name))+ 
  geom_line(data = med_precip_SoilM_36,linetype = "dashed", aes(x = datetime, y = value, color = "36 (precip zone average)"))+
  labs(x = "",
       y = 'Soil Moisture (%)',
       color = "Depth (in)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks")

ggplotly(SoilMGraph36_med)


# HIGH Precip Zone --------------------------------------------------------------------------------------------------------------------------------------------------

#dataframe with daily average soilM at 12 and 36 inches
high_precip_SoilM <- FLOURISH_MASTER %>% 
  subset(Param == 'SoilM' & precip_zone == "HIGH") %>%
  drop_na() %>%
  group_by(Depth) %>% 
  summarize_by_time(
    .date_var = datetime,
    .by = "day",
    value = mean(Value)) %>% 
  mutate(depth = as.factor(Depth))

high_precip_SoilM_12 <- high_precip_SoilM %>% 
  subset(depth == 12)

high_precip_SoilM_36 <- high_precip_SoilM %>% 
  subset(depth == 36)

#graph 12 inch high precip
SoilMGraph12_high <- FLOURISH_MASTER %>%
  subset(Site != "ECKFL-CC02" ) %>% 
  subset(Param == 'SoilM' & Depth == 12 & precip_zone == "HIGH") %>% 
  ggplot()+
  geom_line(aes(x = datetime, y = Value, color = producer_name, group = producer_name))+ 
  geom_line(data = high_precip_SoilM_12,linetype = "dashed" , aes(x = datetime, y = value, color = "12 (precip zone average)"))+
  labs(x = "",
       y = 'Soil Moisture (%)',
       color = "Depth (in)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks")

ggplotly(SoilMGraph12_high)

#graph 36 inch high precip
SoilMGraph36_high <- FLOURISH_MASTER %>%
  subset(Site != "ECKFL-CC02" ) %>% 
  subset(Param == 'SoilM' & Depth == 36 & precip_zone == "HIGH") %>% 
  ggplot()+
  geom_line(aes(x = datetime, y = Value, color = producer_name, group = producer_name))+ 
  geom_line(data = high_precip_SoilM_36,linetype = "dashed", aes(x = datetime, y = value, color = "36 (precip zone average)"))+
  labs(x = "",
       y = 'Soil Moisture (%)',
       color = "Depth (in)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks")

ggplotly(SoilMGraph36_high)


