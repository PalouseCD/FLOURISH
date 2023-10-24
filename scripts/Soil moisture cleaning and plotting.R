

##### Soil moisture sensors cleaning and plotting
### Ryan 
### 5/2/2023


library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)
library(readr)
library(here)
library(janitor)
library(lubridate)

md <- read_csv(here("data","802033_Data.csv"),
               col_names= c("time",
                            "datasnap",
                            "id",
                            "logtype",
                            "depth",
                            "SoilM",
                            "temperature_c",
                            "perm",
                            "EC",
                            "pwec"),
                skip = 1)
  
  md <- md[-c(1:77),c(1,3,5:10)]### this is just to select the appropriate rows and columns for this data set
  md$time <-  dmy_hms(md$time)
  md$depth <- as.character(md$depth)
  
  mdn <- md %>% pivot_longer(cols = 4:8)

write_csv(md,here("data","cleaned_802033_Data.csv"))
  

plotmd <- function(data,y,title,ytitle){
  plot <- plot_ly(data = data, 
                x = ~time, 
                y = data[[y]],
                split = ~depth,
                type = 'scatter',
                mode = 'lines') %>%
    layout(legend=list(title=list('depth')),
           title = title,
           plot_bgcolor='#e5ecf6',
           xaxis = list( 
             title = "Date",
             zerolinecolor = '#ffff', 
             zerolinewidth = 2,
             gridcolor = 'ffff'),
           yaxis = list( 
             title = ytitle,
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff') ) 
  return(plot)                       
}

plotmd(md,"temperature_c","Soil Temperature","Temperature (C)")
plotmd(md,"moisture_per", "Soil Moisture data", "Soil Moisture %")

        
