library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(stringr)
sbi <- read.csv('D:/gitHub/NTU_R/Final/Youbike_sbi(1).csv')
sbi_line <- sbi%>%gather( Time, value, X2018.8.1.7:X2018.8.4.12, na.rm = TRUE)
sbi_line<-select(sbi_line,sarea,Time,value)
sbi_line<-sbi_line%>%group_by(sarea,Time)%>%summarise(mean=mean(value))
sbi_line$Time <- str_replace(sbi_line$Time, 'X', '')
sbi_line$Time <- ymd_h(sbi_line$Time)
sbi_line<-separate(sbi_line,Time,c("day","hour"),sep=" ",remove=FALSE)

all_regions <- sort(unique(sbi_line$sarea))
min_date <- min(sbi_line$Time)
max_date <- max(sbi_line$Time)


shade <- read.csv('D:/gitHub/NTU_R/Final/shade.csv')
shade$x1<-ymd_h(shade$x1)
shade$x2<-ymd_h(shade$x2)


# UI
ui <- fluidPage(
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
      dateRangeInput(inputId = "Time",
                     label = "Select Time:",
                     start = "2018-08-01 07:00:00",
                     end = "2018-08-04 09:00:00",
                     min = min_date, max = max_date,
                     startview="date")
      
      ,
      
      selectInput(inputId = "sarea",
                  label = "Select region:",
                  choices = all_regions,
                  selected = "大安區",
                  multiple=TRUE)
      
    ),
    
    # Output(s)
    mainPanel(plotOutput(outputId = "lineplot")
    )
  )
)

#server
server <- function(input, output) {
  
  # Create data table
  output$lineplot<- renderPlot({
    movies_selected_date <- sbi_line %>%
      mutate(day = as.Date(day)) %>%
    filter(sarea %in%input$sarea,day >= input$day[1] & day <= input$day[2] )
    ggplot() + 
      geom_line(aes_string(x=input$Time, y='mean',color=input$sarea),data= movies_selected_date)+
      geom_rect(data=shade, 
                mapping=aes_string(xmin='x1', xmax='x2', ymin=-Inf, ymax=Inf), color='grey')
    
    
    
  })
  
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)

