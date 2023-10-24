#Shiny App Test for FLOURISH Chemical and Physical Data


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
#when running in RStudio use:
cleanphysdata <- read_csv(here("data", "Bd_Infil_CombinedData.csv"))
cleanchemdata <- read_csv(here("data", "cleaned_chemdata_test.csv"))

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
                          "Ace Protein",                          "Ammonium (PPM)",                       "Base Saturation",                     
                          "Boron (PPM)",                          "Calcium (PPM)",                        "Calcium Saturation",                  
                          "Carbon Exchange Capacity (meq/100g)",  "Conductivity (dS/m)",                  "Copper (PPM)",                        
                          "Excess Lime",                          "Hydrogen Saturation",                  "Iron (PPM)",                          
                          "Macroaggregates",                      "Magnesium (PPM)",                      "Magnesium Saturation",                
                          "Manganese (PPM)",                      "Microaggregates",                      "Modified WDRF BpH",                   
                          "Nitrate (LBs)",                        "Nitrate (PPM)",                        "Organic Matter",                      
                          "Permanganate Oxidizable Carbon (PPM)", "pH",                                   "Phosphate (PPM)",                     
                          "Potassium (PPM)",                      "Potassium Saturation",                 "Sodium (PPM)",                        
                          "Sodium Saturation",                    "Soil Respiration (PPM)",               "Sulfer (PPM)",                        
                          "Total Aggregates",                     "Total Organic Carbon",                 "Zinc (PPM)",                          
                          "β.glucosidase enzyme"
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
                        "THROOP",
                        "STUBBS"
                      )
          )
        ),

        # Show a plot of the selected data
        mainPanel(
           plotlyOutput("chemdata")
        )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {

  chemdata <-reactive({
    new_data <-filter(cleanchemdata, producer_name == input$prod & Parameter == input$para)
    return(new_data)
  }) 
  
    output$chemdata <- renderPlotly({
        plot_ly(chemdata(), x = ~Management)%>%
      add_trace(y = ~FieldAv, type = 'bar', name = 'Your Field', error_y = ~list(
                  array = FieldAv_sd,
                  color = '#000000')) %>%
      add_trace(y = ~PrecipZoneAv, type = 'bar', name = 'Average Across Precip Zone', error_y = ~list(
                  array = PrecipZoneAv_sd,
                  color = '#000000')) %>%
      add_trace(y = ~ProjAve, type = 'bar', name = 'Average Across Project', error_y = ~list(
                  array = ProjAv_sd,
                  color = '#000000'))%>%
      
      layout(yaxis = list(title = input$para), barmode = 'group')
    
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
