# FLOURISH Soil Moisture Data Shiny APP
# Gerrit Bass Summer 2023
# This app provides a platform for producers in FLOURISH project to view the data from the Soil Moisture sensors in their cover crop fields



#SETUP ---------------------------------------------------------------------------------------------------------------------------------------------

library(shiny)
library(readr)
library(plotly)
library(dplyr)
library(tidyr)
library(here)
library(timetk)


# import data
FLOURISH_MASTER <- read_csv(here("data","FLOURISH_MASTER.csv")) %>% 
  subset(Site != "ECKFL-CC02" )

# connect to shiny.io server
rsconnect::setAccountInfo(name='pcdrm',
                          token= '22F8B9DB932C3F6582F2B3E5DDDE6A14',
                          secret= 'T0EH/V2HUtckmKizHDYSeLK4Q3pCxOyYZ0bY+Qzj')


# USER INTERFACE------------------------------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  
    titlePanel("FLOURISH Data Summary"),
  
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            uiOutput("prod_name")
        ),

        mainPanel(
          img(src = "PCD_LOGO.jpg", align = "center"),
          img(src = "FLOURISH.png", height = "25%", width = "25%", align = "center" ),
          img(src = "NRCS.jpg", height = "30%", width = "30%", align = "center"),
          h2("Here's your data!"),
          h4("Thank you for participating in the FLOURISH project.
             Below you will find the Soil Moisture and Soil Temperature data from the sensors in your cover crop field.
             We have added a project average to help you see where your values fall compared to others in the project.
             In the future, we will be working to include this same stat, but, for your precipitation zone to make it more comparable."),
          h4("When using the graphs below there are a few features to note. You can click objects in the legend to turn them on and off. You can zoom in and out.
             The camera icon allows you to save a picture of your graph. And the one or two tab icons allow you to view exatct values of a single or multiple lines at once."),
          h4('As the project continues we will be working on finding the best way to get your data to you, so please do not hesitate to offer up questions or comments on how it is displayed.
             Feel free to reach out to Gerrit (gerritb@palousecd.org) or Ryan (ryanb@palousecd.org) with any questions or comments!'),
           plotlyOutput("SoilM"),
           plotlyOutput("SoilT")
        )
    )
)

# SERVER----------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  
  # Define output for Producer name drop down
    output$prod_name <- renderUI({
      selectInput("last_name","Select your last name from the dropdown",
                  unique(FLOURISH_MASTER$producer_name))})
    
  # Define Soil Moisture Plot---------------------------------------------------------------------------------------------------------------
    output$SoilM <- renderPlotly({
      
      prod_precip_df <- FLOURISH_MASTER %>%  
        subset(producer_name == input$last_name)
      
      prod_precip_zone <- prod_precip_df$precip_zone[1]
      
      #dataframe with daily average soilM at 12 and 36 inches
      precip_avg_soilM <- FLOURISH_MASTER %>% 
        subset(Param == 'SoilM' & precip_zone == prod_precip_zone) %>%
        drop_na() %>%
        group_by(Depth) %>% 
        summarize_by_time(
          .date_var = datetime,
          .by = "day",
          value = mean(Value)) %>% 
        mutate(depth = as.factor(Depth))
      
      precip_avg_soilM_12 <- precip_avg_soilM %>% 
        subset(depth == 12)
      
      precip_avg_soilM_36 <- precip_avg_soilM %>% 
        subset(depth == 36)
      
      # creates date range to go into graph based on producer and their data range
      prod_range <- FLOURISH_MASTER %>%
        subset(producer_name == input$last_name) %>%
        summarise(across(datetime, list(min = min, max = max)))
      
      #graph
      SoilMGraph <- FLOURISH_MASTER %>%
        subset(producer_name == input$last_name) %>% 
        subset(Param == 'SoilM') %>% 
        mutate(Depth = as.factor(Depth)) %>% 
        ggplot()+
        geom_line(aes(x = datetime, y = Value, color = Depth, group = Depth))+ 
        geom_line(data = precip_avg_soilM_12,linetype = "dashed" , aes(x = datetime, y = value, color = "12 (precip zone average)"))+
        geom_line(data = precip_avg_soilM_36,linetype = "dashed", aes(x = datetime, y = value, color = "36 (precip zone average)"))+
        labs(x = "",
             y = 'Soil Moisture (%)',
             color = "Depth (in)")+
        scale_color_manual(values=c("12"= "#E69F00","36" = "#56B4E9","12 (precip zone average)" = "#E69F00","36 (precip zone average)" = "#56B4E9"))+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45))+
        scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks", limits = c(prod_range$datetime_min,prod_range$datetime_max))+
        scale_y_continuous(limits = c(0,max(FLOURISH_MASTER$Value)+10))
      
      ggplotly(SoilMGraph)
    })
    
    #Soil Temp Graph ------------------------------------------------------------------------------------------------------------------------------
    output$SoilT <- renderPlotly({
      
      prod_precip_df <- FLOURISH_MASTER %>%  
        subset(producer_name == input$last_name)
      
      prod_precip_zone <- prod_precip_df$precip_zone[1]
       
      #dataframe with daily average soilM at 12 and 36 inches
      precip_avg_soilT <- FLOURISH_MASTER %>% 
        subset(Param == 'SoilT'& precip_zone == prod_precip_zone) %>% 
        drop_na() %>%
        group_by(Depth) %>% 
        summarize_by_time(
          .date_var = datetime,
          .by = "day",
          value = mean(Value)) %>% 
        mutate(depth = as.factor(Depth)) %>% 
        mutate(value = ((9/5)*value + 32))
      
      precip_avg_soilT_12 <- precip_avg_soilT %>% 
        subset(depth == 12)
      
      precip_avg_soilT_36 <- precip_avg_soilT %>% 
        subset(depth == 36)
      
      # creates date range to go into graph based on producer and their data range
      prod_range <- FLOURISH_MASTER %>%
        subset(producer_name == input$last_name) %>%
        summarise(across(datetime, list(min = min, max = max)))
      
      #graph
      SoilTGraph <- FLOURISH_MASTER %>%
        subset(producer_name == input$last_name) %>% 
        subset(Param == 'SoilT') %>% 
        mutate(Depth = as.factor(Depth)) %>%
        mutate(Value = ((9/5)*Value + 32)) %>% 
        ggplot()+
        geom_line(aes(x = datetime, y = Value, color = Depth, group = Depth))+ 
        geom_line(data = precip_avg_soilT_12,linetype = "dashed" , aes(x = datetime, y = value, color = "12 (precip zone average)"))+
        geom_line(data = precip_avg_soilT_36,linetype = "dashed", aes(x = datetime, y = value, color = "36 (precip zone average)"))+
        labs(x = "",
             y = 'Soil Temperature (°F)',
             color = "Depth (in)")+
        scale_color_manual(values=c("12"= "#E69F00","36" = "#56B4E9","12 (precip zone average)" = "#E69F00","36 (precip zone average)" = "#56B4E9"))+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45))+
        scale_x_datetime(date_labels = "%m/%d/%y", date_breaks = "weeks", limits = c(prod_range$datetime_min,prod_range$datetime_max))
      
      ggplotly(SoilTGraph)
    })
}

# Run the application -----------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
