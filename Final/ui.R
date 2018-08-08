library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(stringr)
sbi <- read.csv('D:/gitHub/NTU_R/Final/Youbike_sbi(1).csv')
sbi_line <- sbi%>%gather( Time, value, X2018.8.1.7:X2018.8.4.12, na.rm = TRUE)
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
    mainPanel(plotOutput(outputId = "line_plot")
    )
  )
)
