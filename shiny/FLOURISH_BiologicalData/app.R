#Shiny App for FLOURISH Biological Data
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
cleanbiodata <- read_csv(here("data", "Soil_BiologicalData.csv"))
#cleanbiodata <- read_csv(here("shiny", "FLOURISH_BiologicalData", "data", "Soil_BiologicalData.csv"))

rsconnect::setAccountInfo(name='pcdrm',
                          token='22F8B9DB932C3F6582F2B3E5DDDE6A14',
                          secret='T0EH/V2HUtckmKizHDYSeLK4Q3pCxOyYZ0bY+Qzj')



# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("FLOURISH Biological Soil Data"),
  
  # Sidebar with a dropdown for parameter choice 
  sidebarLayout(
    sidebarPanel(
      uiOutput("Producer_name"),
      selectInput("para",
                  label = "Select Desired Parameter",
                  choices = c(
                    'Ace Protein',
                    'Permanganate Oxidizable Carbon (PPM)',
                    'Soil Respiration (PPM)',
                    'β.glucosidase enzyme',
                    'Organic Matter'
                    
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
      plotlyOutput("biodata")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  biodata <-reactive({
    new_data <-filter(cleanbiodata, producer_name == input$prod & Parameter == input$para)
    return(new_data)
  }) 
  
  output$biodata <- renderPlotly({
    plot_ly(biodata(), x = ~Management)%>%
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

