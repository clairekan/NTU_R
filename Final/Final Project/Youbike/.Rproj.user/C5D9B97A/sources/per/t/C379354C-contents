library(shiny)
library(shinythemes)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(DT)
library(tools)

function(input, output) {
  # Data
  res <- read.csv('data/Youbike_res.csv')
  res_g <- gather(res, time, per, 6:ncol(res))
  sbi <- read.csv('data/Youbike_sbi.csv')
  sbi_g <- gather(sbi, time, quan, 6:ncol(sbi))
  
  res1 <- read.csv('data/Youbike_res3.csv') # CSV
  res1_g <- gather(res1, time, per, 6:23)

  res2 <- read.csv('data/Youbike_res3.csv') # CSV
  res2_g <- gather(res2, time, per, 6:23)
  
  res3 <- read.csv('data/Youbike_res3.csv')
  res3_g <- gather(res3, time, per, 6:23)
  
  res4 <- read.csv('data/Youbike_res4.csv')
  res4_g <- gather(res4, time, per, 6:23)
  
  res5 <- read.csv('data/Youbike_res5.csv')
  res5_g <- gather(res5, time, per, 6:23)
  
  res6 <- read.csv('data/Youbike_res6.csv')
  res6_g <- gather(res6, time, per, 6:23)
  
  res7 <- read.csv('data/Youbike_res3.csv') # CSV
  res7_g <- gather(res7, time, per, 6:23)
  
  dataset <- list(res1, res2, res3, res4, res5, res6, res7)
  dataset_g <- list(res1_g, res2_g, res3_g, res4_g, res5_g, res6_g, res7_g)
  rawdata <- list(res_g, sbi_g)
  
  
  #  Raw Data
  
    #Reactive raw data
  data_input <- reactive({
    req(input$rawdata)
    temp <- data.frame(rawdata[[as.numeric(input$rawdata)]])
  })
  
  output$maindata <- DT::renderDataTable({
    DT::datatable(data = data_input(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  
  # Download for main data
  output$download_maindata <- downloadHandler(
    filename = function() {paste0(input$rawdata,'.csv')},
    content = function(file) { write_csv(data_input(), path = file) }
  )
  
  
  # Data for heat map
    # Reactive data inout
  data_day <- reactive({
    req(input$day1)
    data.frame(dataset_g[[as.numeric(input$day1)]])
  })
  
  data_time <- reactive({
    req(input$time1)
    data_day() %>%
      filter(time == input$time1)
  })
  
  output$data <- DT::renderDataTable({
    req(input$day1)
    DT::datatable(data = data_time(), 
                  options = list(pageLength = 5), 
                  rownames = FALSE)
  })

  # Data download for heat map
  output$download_data <- downloadHandler(
    filename = function() {paste0(input$day, '-', input$time, '.csv')},
    content = function(file) { write_csv(data_time(), path = file) }
  )
  
  
  # Heat Map
    # Reactive data inout
  data_day1 <- reactive({
    req(input$day1)
    data.frame(dataset[[as.numeric(input$day1)]])
  })
  
    
  output$heat <- renderPlot({
    # Map
    map <- get_map(location = c(min(res$lng), min(res$lat), max(res$lng), max(res$lat)), maptype = "toner-lite")
    
    res.stat.map <- ggmap(map, darken = c(0.5, "white")) %+% data_day1() + aes_string(x = "lng", y = "lat", z = input$time1) +
      stat_summary_2d(fun = median, alpha = 0.6) +
      scale_fill_gradientn(name = 'Median', colours = brewer.pal(11, "RdYlGn"), limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
      labs(x = "Longitude", y = "Latitude") +
      coord_map() +
      ggtitle('Remaining Percentage of Youbike in Taipei') +
      geom_jitter(shape = 1)
    res.stat.map
  }, height = 480)
  
  # animation
  output$animation <- renderImage({
    filename <- normalizePath(file.path(paste0('./data/ani', input$day1, '.gif')))
    
    # Return a list containing the filename and alt text
    list(src = filename, style="display: block; margin-left: auto; margin-right: auto;")
    
  }, deleteFile = FALSE)
  
  # Time Table
  output$locat <- DT::renderDataTable({
    nearPoints(data_day1(), coordinfo = input$plot_hover) %>% 
      dplyr::select(sna, sarea, input$time1)
  })
  
}