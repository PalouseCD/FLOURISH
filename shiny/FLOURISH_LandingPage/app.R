### Landing Page Test ##
# Gabrielle Hannen, 09/18/23


library(shiny)
library(shinyBS)
library(shinyLP)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(rsconnect)

rsconnect::setAccountInfo(name='pcdrm',
                          token='22F8B9DB932C3F6582F2B3E5DDDE6A14',
                          secret='T0EH/V2HUtckmKizHDYSeLK4Q3pCxOyYZ0bY+Qzj')

# Define UI for application
ui <- 

  # Include a fliudPage above the navbar to incorporate a icon in the header
  # Source: http://stackoverflow.com/a/24764483
  fluidPage(
    
    
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle="FLOURISH Data Dashboard"
        )
    ),
  
      
    navbarPage(title=div(img(src="PCD_LOGO.jpg",
                             height = 80,
                             width = 175)),
               tags$head(
                 tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                   padding-top:5px !important; 
                   padding-bottom:5px !important;
                   height: 90px;
                 }
                 .navbar {min-height:60px !important;}
                 .navbar { background-color: #2D200D;
                           font-family: Arial;
                           font-size: 20px;
                           color: #FFFFFF;}'))
               ),
               
     # Create Right Side Logo/Image with Link       
     tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://www.nrcs.usda.gov/\"><img src=\"NRCS.jpg\" alt=\"alt\" style=\"float:right;width:200px;height:85px;padding-top:15px;\"> </a></div>');
    console.log(header)")
     ),
      
     # Create Right Side Logo/Image with Link       
     tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"URL\"><img src=\"FLOURISH2.png\" alt=\"alt\" style=\"float:right;width:110px;height:100px;padding-top:5px\"> </a></div>');
    console.log(header)")
     ),

     jumbotron("FLOURISH Data Dashboard",
               "Welcome to the FLOURISH Data Dashboard. This web application was created to display all soil health data collected by the Palouse Conservation District Research & Monitoring Program for FLOURISH producers. 
                All data was collected by Palouse Conervation District and analyzed by Regen Ag Laboratory. Data is broken up into physical, chemical, biological and sensor data. To begin, click the buttons below.",
               button = FALSE),
     
             
                        
   
   # tabPanel("Home",
             setBackgroundImage(src = "covercrop3.jpg"),
            mainPanel(width=12,  #style = "align: center; opacity: 0.8; background-color: white",
          
                       
          
            fluidRow(
              column(6, thumbnail_label(image = 'phys2.png', label = 'Physical Data',
                                        content = 'Physical data includes density, infiltration, aggregates and temperature. ',
                                        button_link = 'https://pcdrm.shinyapps.io/FLOURISH_PhysicalData/', button_label = 'Click me')
              ),
              
              column(6, thumbnail_label(image = 'beaker2.png', label = 'Chemical Data',
                                         content = 'Chemical data includes soil nutrients such as nitrogen, phosphorus and ammonia. ',
                                         button_link = 'https://pcdrm.shinyapps.io/FLOURISH_ChemicalData/', button_label = 'Click me')
                     )),

              fluidRow(
              column(6, thumbnail_label(image = 'bio2.png', label = 'Biological Data',
                                        content = 'Biological data includes living components of soil such as microbial activity and enzymes. ',
                                        button_link = 'https://pcdrm.shinyapps.io/FLOURISH_BiologicalData/', button_label = 'Click me')
                     ),
              column(6, thumbnail_label(image = 'sensor2.png', label = 'Soil Sensor Data',
                                        content = 'Soil sensor data includes soil moisture and soil temperature. ',
                                        button_link = 'https://pcdrm.shinyapps.io/FLOURISH_data/', button_label = 'Click me')
                     )
                        ),

     div(style = "align: center; opacity: 0.8; background-color: white; padding-top; 5px, padding-bottom; 5px",
             tags$i("This application was designed by the Palouse Conservation District Research & Monitoring Program. 
                      This application is intended to be a useful tool to view soil data from samples collected by PCD 
                      and analyzed by Regen Ag laboratory. If you come across any issues or have questions about
                      your data, please contact Ryan Boylan (ryanb@palousecd.org), Gerrit Bass (gerritb@palousecd.org
                      or Gabby Hannen (gabrielleh@palousecd.org)."),
             ))),

      # Hide Errors
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
   ),
   options(shiny.sanitize.errors = TRUE)   
    
       )
    
  

# Define server logic
server <- function(input, output) {


  
}


shinyApp(ui = ui, server = server)

