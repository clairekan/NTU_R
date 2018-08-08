library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(stringr)
sbi <- read.csv('D:/gitHub/NTU_R/Final/Youbike_sbi(1).csv')
sbi_line <- sbi%>%gather( Time, value, X2018.8.1.7:X2018.8.4.10, na.rm = TRUE)
#sbi <- read.csv('D:/gitHub/NTU_R/Final/Youbike_sbi.csv')
#sbi_line <- sbi%>%gather( Time, value, X2018.8.1.7:X2018.8.1.24, na.rm = TRUE)
sbi_line<-select(sbi_line,sarea,Time,value)
sbi_line<-sbi_line%>%group_by(sarea,Time)%>%summarise(mean=mean(value))
sbi_line$Time <- str_replace(sbi_line$Time, 'X', '')
sbi_line$Time <- ymd_h(sbi_line$Time)
sbi_line<-separate(sbi_line,Time,c("day","hour"),sep=" ",remove=FALSE)

all_regions <- sort(unique(sbi_line$sarea))

shade <- read.csv('D:/gitHub/NTU_R/Final/shade.csv')
shade$x1<-ymd_h(shade$x1)
shade$x2<-ymd_h(shade$x2)

server <- function(input, output) {
  
  # Create data table
  output$lineplot<- renderPlot({
    sbi_line1<-filter(sbi_line,sarea=="南港區",day >= as.Date("2018-08-01") & day <=as.Date("2018-08-03") )
    ggplot() + 
      geom_line(aes_string(x=input$Time, y='mean'),data= sbi_line1)+
      geom_rect(data=shade, 
                mapping=aes_string(xmin='x1', xmax='x2', ymin=-Inf, ymax=Inf), color='grey')
    
    
    
  })
  
}

