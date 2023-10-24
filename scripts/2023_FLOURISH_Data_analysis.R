### FLOURISH preliminary data analysis ###
## started by Ryan ####
install.packages("ggcorrplot")

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
library(ggcorrplot)

### GRAPHING DATA ###
#read in data
data <- read_csv(here("data","FLOURISH_MASTER_SoilsData.csv"))

data <- clean_names(data)

summary(data)

data <- drop_na(data)

data <- data %>% 
  filter (!parameter %in% c("Degrees of Saturation",
            "Excess Lime",
            "Modified WDRF BpH",
            "Base Saturation",
            "Hydrogen Saturation",
            "Calcium Saturation",
            "Magnesium Saturation",
            "Potassium Saturation",
            "Sodium Saturation",
            "Macroaggregates",
            "Microaggregates"))

data <- data %>% 
  mutate(across(c(field_id,
                  parameter,
                  soil_type,
                  management,
                  producer_name,
                  precip_zone),factor))

var <- levels(data$parameter)

### Test Graphs ###

### this will work for Box plots
boxp <- function(para){
  data %>% 
    filter(parameter == para) %>% 
    ggplot(aes(x= management,y = value, fill = management))+
    geom_boxplot()+
    facet_grid(.~parameter)
}

lapply(var,boxp)


#box plots by field ID
boxp2 <-function(para){ 
  data %>% 
    filter(parameter == para) %>% 
  ggplot(aes(x = producer_name, y =value))+
  geom_boxplot(aes(fill = management))+
  facet_grid(.~parameter)+
  theme(axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45))
}

lapply(var,boxp2)

#box plots by field ID
omplot3 <- ggplot(om,aes(x = management, y =value))+
  geom_boxplot(aes(color = management))+
  facet_grid(parameter~precip_zone)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45))

ggplotly(omplot3) %>% layout(boxmode = "group")

#averages and Standard Deviations
dataavg <- data %>% 
  group_by(field_id,parameter,year) %>% 
  mutate(avg = mean(value, na.rm = TRUE),
         stdev = sd(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  distinct(avg, stdev, .keep_all = TRUE)

## bargraphs of by field IDs with standard deviations
bar <- function(para){
  dataavg %>% 
    filter(parameter == para) %>% 
      ggplot(aes(producer_name, avg, fill = management))+
      geom_bar(stat = "identity",position = position_dodge())+
      geom_errorbar(aes(ymin = avg-stdev, ymax = avg+stdev),position = position_dodge())+
      facet_grid(.~parameter)+
      theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45))
      #ggplotly(barg) %>% layout(barmode = "stack")
}

lapply(var,bar)



# Histograms

par <-  ggplot(om,aes(value))+
  geom_histogram()+
  facet_grid(.~parameter)
ggplotly(par)

# T test
t.test(value~management, data = om)



# going from long to wide for all the data to prep for cor tables
wdata <- data %>% 
  pivot_wider(
    names_from = parameter,
    values_from = value,
    values_fn = mean) %>% 
   mutate(management =case_match(management, 
                              "Business as Usual" ~ 1,
                              "Cover Crop" ~ 2)) %>% 
  mutate(precip_zone = case_match(precip_zone,
                                  "MED" ~ 2,
                                  "HIGH"~ 3,
                                  "LOW"~ 1))

cdata <- wdata[,c(4,6,7:9,11,12,14,16:31,38:42,45)]

corr <- round(cor(cdata),3)
p.mat <- cor_pmat(cdata)

ggcorrplot(corr,hc.order = TRUE,
           outline.color = "white",
           type = "lower",
            p.mat = p.mat)



#glm check out this website for more details: https://www.guru99.com/r-generalized-linear-model.html
gdata <- wdata %>% 
  mutate(management =case_match(management, 
                                1 ~ "Business as Usual",
                                2~ "Cover Crop" )) %>% 
  mutate(precip_zone = case_match(precip_zone,
                                  2~ "MED" ,
                                  3 ~"HIGH",
                                  1 ~ "LOW"))

continous <- select_if(gdata,is.numeric)
summary(continous)

gdata_rescale <- gdata %>% 
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(gdata_rescale)



gdata <- gdata %>% 
  mutate(across(c(field_id,
                   soil_type,
                   management,
                   producer_name,
                   precip_zone),factor))




