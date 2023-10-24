#Shiny App for FLOURISH Physical Data
# Gabby Hannen September 2023

library(tidyr)
library(dplyr)
library(shiny)
library(rsconnect)
library(plotly)
library(shinyWidgets)
library(shinythemes)
library(lubridate)
library(anytime)
library(gt)
library(kableExtra)
library(here)
library(tidyverse)
library(ggthemes)

### CREATING DATAFRAME ###
cleanphysdata <- read_csv(here("data", "Soil_PhysicalData.csv"))
#cleanphysdata <- read_csv(here("shiny", "FLOURISH_PhysicalData", "data", "Soil_PhysicalData.csv"))

rsconnect::setAccountInfo(name='pcdrm',
                          token='22F8B9DB932C3F6582F2B3E5DDDE6A14',
                          secret='T0EH/V2HUtckmKizHDYSeLK4Q3pCxOyYZ0bY+Qzj')



# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("FLOURISH Chemical Soil Data"),
  
  # Sidebar with a dropdown for parameter choice 
  sidebarLayout(
    sidebarPanel(
      uiOutput("Producer_name"),
      selectInput("para",
                  label = "Select Desired Parameter",
                  choices = c(
                    "Infiltration (In/Hr)",
                    "Bulk Density (g/cm3)",
                    "Degrees of Saturation",
                    "Gravametric Soil Moisture",
                    "Volumetric Soil Moisture",
                    "Total Aggregates",
                    "Microaggregates",
                    "Macroaggregates"
                  )),
      selectInput("prod",
                  label = "Select Your Last Name",
                  choices = c(
                    "BISHOP",
                    "ZAKARISON",
                    "APPEL",
                    "MILLER",
                    "ZIMMER",
                    "WOLF",
                    "DRUFFEL",
                    "MCKAY",
                    "BREWER",
                    "SCHEIBE",
                    "GREENE",
                    "SCHUSTER",
                    "ELLIOTT",
                    "DOBBINS",
                    "ERIKSEN",
                    "DEWALD",
                    "ZENNER",
                    "ECKHART",
                    "OLESEN",
                    "BATES",
                    "MORTER",
                    "POOLE",
                    "RIDDLE",
                    "STUIVENGA",
                    "STUBBS"
                  )
      )
    ),
    
    # Show a plot of the selected data
    mainPanel(
      img(src = "PCD_LOGO.jpg", align = "center"),
      img(src = "FLOURISH.png", height = "25%", width = "25%", align = "center" ),
      img(src = "NRCS.jpg", height = "30%", width = "30%", align = "center"),
      plotlyOutput("physdata")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  physdata <-reactive({
    new_data <-filter(cleanphysdata, producer_name == input$prod & Parameter == input$para)
    return(new_data)
  }) 
  
  output$physdata <- renderPlotly({
    plot_ly(physdata(), x = ~Management)%>%
      add_trace(y = ~FieldAv, type = 'bar', name = 'Your Field', marker = list(color = 'rgb(42, 212, 48)'), error_y = ~list(
        array = FieldAv_sd,
        color = '#000000')) %>%
      add_trace(y = ~PrecipZoneAv, type = 'bar', name = 'Average Across Precip Zone', marker = list(color = 'rgb(160, 178, 172)'), error_y = ~list(
        array = PrecipZoneAv_sd,
        color = '#000000')) %>%
      add_trace(y = ~ProjAve, type = 'bar', name = 'Average Across Project', marker = list(color = 'rgb(210, 226, 221)'), error_y = ~list(
        array = ProjAv_sd,
        color = '#000000'))%>%
      
      layout(yaxis = list(title = input$para), barmode = 'group')
    
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

