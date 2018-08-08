library(shiny)
library(shinythemes)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(DT)
library(tools)
library(vcd)

# Data
res <- read.csv('data/Youbike_res.csv')

# Line Plot Data
sbi <- read.csv('data/Youbike_sbi(1).csv')
sbi_line <- sbi %>% gather( Time, value, X2018.8.1.7:X2018.8.4.9, na.rm = TRUE)
sbi_line <- select(sbi_line,sarea,Time,value)
sbi_line <- sbi_line %>% group_by(sarea, Time) %>% summarise(mean = mean(value), sum = sum(value))
sbi_line$Time <- str_replace(sbi_line$Time, 'X', '')
sbi_line$Time <- ymd_h(sbi_line$Time)
sbi_line <- separate(sbi_line, Time, c("day", "hour"),sep = " ",remove = FALSE)

all_regions <- sort(unique(sbi_line$sarea))

navbarPage(
  # Theme
  theme = shinytheme('flatly'),
  'Youbike Analysis',
  tabPanel(
    'Introduction',
    
    sidebarPanel(
      h2('Introduction')
    ),
    
    mainPanel(
      HTML('<iframe width="960" height="540" src="https://www.youtube.com/embed/wMW9yLWHTnk" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>')
    )
  ),
  
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
                              'Sunday' = '7')),
      
      hr(),
      # Time Input
      selectInput(inputId = 'time1',
                  label = 'Time',
                  choices = c('07:00' = 'X7',
                              '08:00' = 'X8',
                              '09:00' = 'X9',
                              '10:00' = 'X10',
                              '11:00' = 'X11',
                              '12:00' = 'X12',
                              '13:00' = 'X13',
                              '14:00' = 'X14',
                              '15:00' = 'X15',
                              '16:00' = 'X16',
                              '17:00' = 'X17',
                              '18:00' = 'X18',
                              '19:00' = 'X19',
                              '20:00' = 'X20',
                              '21:00' = 'X21',
                              '22:00' = 'X22',
                              '23:00' = 'X23',
                              '24:00' = 'X24')
                   )
    ),

    mainPanel(
      tabsetPanel(type = 'tabs',
                  # Tab 1: Plot
                  tabPanel(title = 'Map',
                           br(),
                           tags$p('Please wait, it takes a few seconds to load the map.'),
                           tags$p('You can point at specific Youbike station for more information in the below.'),
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
                  # Tab 3 : Summary
                  tabPanel(title = 'Summary',
                           br(),
                           plotOutput('barchart')
                           ),
                  # Tab 4 : Data
                  tabPanel(title = 'Data',
                           br(),
                           br(),
                           DT::dataTableOutput(outputId = "data"),
                           br(),
                           downloadButton(outputId = "download_data", label = "Download .CSV")))
      
    )
  ),
  tabPanel(
    'Mosaic Plots',
    
    sidebarPanel(
      h2('Chi-Squared Test'),
      hr(),
      selectInput(inputId = 'day2',
                  label = 'Day',
                  choices = c('Monday' = '1',
                              'Tuesday' = '2',
                              'Wednesday' = '3',
                              'Thursday' = '4',
                              'Friday' = '5',
                              'Saturday' = '6',
                              'Sunday' = '7'))
    ),
    
    mainPanel(
      tabsetPanel(type = 'tabs',
                  # Tab 1: Mosaic
                  tabPanel(title = 'Mosaic',
                           br(),
                           tags$p('The higher the residual, the more overrepresented a segment is.'),
                           tags$p('The lower the residual, the more underrepresented a segment is.'),
                           tags$p('The size of each rectangle represents the quantity of bikes then.'),
                           plotOutput('mosaic')),
                  
                  # Tab 2: Residuals
                  tabPanel(title = 'Residual',
                           br(),
                           DT::dataTableOutput(outputId = "residual")),
                  
                  # Tab 3: Expected
                  tabPanel(title = 'Expected',
                           br(),
                           DT::dataTableOutput(outputId = "expected")),
                  
                  # Tab 4: Observation
                  tabPanel(title = 'Observation',
                           br(),
                           DT::dataTableOutput(outputId = "observation")))
    )
  ),
  tabPanel(
    'Line Plots',
           sidebarPanel(
             h2('Line Plots'),
             hr(),
             h5('Pick dates between 2018-08-01 and 2018-08-07.'),
             dateRangeInput(inputId = "Time",
                            label = "Time:",
                            start = "2018-08-01",
                            end = "2018-08-04",
                            min = "2018-08-01", max = "2018-08-07",
                            startview="date"),
             
             selectInput(inputId = "sarea",
                         label = "Region:",
                         choices = all_regions,
                         multiple = T,
                         selected = '大安區')
           ),
           
           mainPanel(
             tabsetPanel(type = 'tabs',
                         
                         # Tab 1: Mean
                         tabPanel(title = 'Mean',
                                  br(),
                                  plotOutput(outputId = "line_mean")),
                         # Tab 2: Sum
                         tabPanel(title = 'Sum',
                                  br(),
                                  plotOutput(outputId = 'line_sum'))
                         )
           )
           )
  
  )
