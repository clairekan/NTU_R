library(shiny)
library(shinythemes)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(DT)
library(tools)

# Data
res <- read.csv('data/Youbike_res.csv')

navbarPage(
  # Theme
  theme = shinytheme('flatly'),
  'Youbike Analysis',
  
  tabPanel(
    'Raw Data',
   
    sidebarPanel(
      h2('Data'),
      br(),
      selectInput(inputId = 'rawdata',
                  label = 'Select Data',
                  choices = c('Percentages' = '1',
                              'Quantities' = '2')),
      br(),
      downloadButton(outputId = "download_maindata", label = "Download .CSV")
      
      
    ),
    
    mainPanel(
      # Data Table Output
      DT::dataTableOutput(outputId = "maindata")
    )
  ),
  tabPanel(
    '2D Heatmap',
    
    sidebarPanel(
      h2('Heat Map'),
      hr(),
      # Day Input
      selectInput(inputId = 'day1',
                  label = 'Day',
                  choices = c('Monday' = '1',
                              'Tuesday' = '2',
                              'Wednesday' = '3',
                              'Thursday' = '4',
                              'Friday' = '5',
                              'Saturday' = '6',
                              'Sunday' = '7'),
                  selected = 'Wednesday'),
      
      hr(),
      # Time Input
      selectInput(inputId = 'time1',
                  label = 'Time',
                  choices = c('07:00' = '7',
                              '08:00' = '8',
                              '09:00' = '9',
                              '10:00' = '10',
                              '11:00' = '11',
                              '12:00' = '12',
                              '13:00' = '13',
                              '14:00' = '14',
                              '15:00' = '15',
                              '16:00' = '16',
                              '17:00' = '17',
                              '18:00' = '18',
                              '19:00' = '19',
                              '20:00' = '20',
                              '21:00' = '21',
                              '22:00' = '22',
                              '23:00' = '23',
                              '24:00' = '24')
                   )
    ),

    mainPanel(
      tabsetPanel(type = 'tabs',
                  # Tab 1: Plot
                  tabPanel(title = 'Map',
                           br(),
                           tags$p('Please wait, it takes a few seconds to load the map.'),
                           tags$p('You can point at specific Youbike station for more information.'),
                           plotOutput('heat', hover = "plot_hover"),
                           br(),
                           br(),
                           br(),
                           hr(),
                           dataTableOutput(outputId = "locat")
                           ),
                  # Tab 2 : Animation
                  tabPanel(title = 'Animation',
                           br(),
                           tags$p('Please wait, it takes a few seconds to load the map.'),
                           tags$p('The speed depends on your network connection.'),
                           plotOutput('animation'),
                           br(),
                           br(),
                           br(),
                           br(),
                           h5("Built with",
                              img(src = "https://raw.githubusercontent.com/thomasp85/gganimate/master/man/figures/logo.png", height = "30px"),
                              ".")),
                  # Tab 3 : Bar Chart
                  tabPanel(title = 'Bar Chart'),
                  # Tab 4 : Data
                  tabPanel(title = 'Data',
                           br(),
                           br(),
                           DT::dataTableOutput(outputId = "data"),
                           br(),
                           downloadButton(outputId = "download_data", label = "Download .CSV")))
      
    )
  ))
