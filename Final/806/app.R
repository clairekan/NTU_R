library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(stringr)

sbi <- read.csv('D:/gitHub/NTU_R/Final/Youbike_sbi(1).csv')
sbi_line <- sbi %>% gather( Time, value, X2018.8.1.7:X2018.8.7.14, na.rm = TRUE)
sbi_line <- select(sbi_line,sarea,Time,value)
sbi_line <- sbi_line %>% group_by(sarea,Time) %>% summarise(mean=mean(value))
sbi_line$Time <- str_replace(sbi_line$Time, 'X', '')
sbi_line$Time <- ymd_h(sbi_line$Time)
sbi_line <- separate(sbi_line,Time,c("day","hour"),sep=" ",remove=FALSE)

all_regions <- sort(unique(sbi_line$sarea))

min_date <- min(sbi_line$Time)
max_date <- max(sbi_line$Time)


shade <- read.csv('D:/gitHub/NTU_R/Final/shade.csv')
shade$x1<-ymd_h(shade$x1)
shade$x2<-ymd_h(shade$x2)


# UI
ui <- fluidPage(
    
    # Input(s)
    sidebarPanel(
      
      dateRangeInput(inputId = "Time",
                     label = "Time:",
                     start = "2018-08-01",
                     end = "2018-08-07",
                     min = min_date, max = max_date,
                     startview="date")
     
    ,
      
      selectInput(inputId = "sarea",
                  label = "Region:",
                  choices = all_regions,
                  multiple = T,
                  selected = '大安區'
                  )
      
    ),
    
    # Output(s)
    mainPanel(plotOutput(outputId = "line_plot")
    )

)

# Server
server <- function(input, output) {
 
  
  output$line_plot <- renderPlot({
    
    req(input$Time)
    req(input$sarea)
    
     selected1 <- sbi_line %>% 
      filter(sarea %in% input$sarea) %>%
      filter(day >= as.Date(input$Time[1], "%Y-%m-%d") & day <= as.Date(input$Time[2], "%Y-%m-%d"))
    
    ggplot() + 
      geom_line(data = selected1, aes_string(x = 'Time', y = 'mean', color = 'sarea')) 
      #geom_rect(data = shade, mapping = aes_string(xmin = 'x1', xmax = 'x2', ymin = -Inf, ymax = Inf), fill = '#DDDDDD') +
      
      })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

