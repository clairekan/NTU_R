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

function(input, output) {
  # Data
  res <- read.csv('data/Youbike_res.csv')
  res_g <- gather(res, time, per, 6:ncol(res))
  sbi <- read.csv('data/Youbike_sbi.csv')
  sbi_g <- gather(sbi, time, quan, 6:ncol(sbi))
  
  res1 <- read.csv('data/Youbike_res1.csv')
  res1_g <- gather(res1, time, per, 6:23)
  sbi1 <- read.csv('data/Youbike_sbi1.csv')
  sbi1_g <- gather(sbi1, time, quan, 6:23)

  res2 <- read.csv('data/Youbike_res3.csv') # CSV
  res2_g <- gather(res2, time, per, 6:23)
  sbi2 <- read.csv('data/Youbike_sbi3.csv') # CSV
  sbi2_g <- gather(sbi2, time, quan, 6:23)
  
  res3 <- read.csv('data/Youbike_res3.csv')
  res3_g <- gather(res3, time, per, 6:23)
  sbi3 <- read.csv('data/Youbike_sbi3.csv')
  sbi3_g <- gather(sbi3, time, quan, 6:23)
  
  res4 <- read.csv('data/Youbike_res4.csv')
  res4_g <- gather(res4, time, per, 6:23)
  sbi4 <- read.csv('data/Youbike_sbi4.csv')
  sbi4_g <- gather(sbi4, time, quan, 6:23)
  
  res5 <- read.csv('data/Youbike_res5.csv')
  res5_g <- gather(res5, time, per, 6:23)
  sbi5 <- read.csv('data/Youbike_sbi5.csv')
  sbi5_g <- gather(sbi5, time, quan, 6:23)
  
  res6 <- read.csv('data/Youbike_res6.csv')
  res6_g <- gather(res6, time, per, 6:23)
  sbi6 <- read.csv('data/Youbike_sbi6.csv')
  sbi6_g <- gather(sbi6, time, quan, 6:23)
  
  res7 <- read.csv('data/Youbike_res7.csv')
  res7_g <- gather(res7, time, per, 6:23)
  sbi7 <- read.csv('data/Youbike_sbi7.csv')
  sbi7_g <- gather(sbi7, time, quan, 6:23)
  
  dataset <- list(res1, res2, res3, res4, res5, res6, res7)
  dataset_g <- list(res1_g, res2_g, res3_g, res4_g, res5_g, res6_g, res7_g)
  datasbi <- list(sbi1, sbi2, sbi3, sbi4, sbi5, sbi6, sbi7)
  datasbi_g <- list(sbi1_g, sbi2_g, sbi3_g, sbi4_g, sbi5_g, sbi6_g, sbi7_g)
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
    # Reactive data input
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
  
  # Bar Chart
  output$barchart<- renderPlot({
    ggplot(data = data_day1(),aes_string(x = 'sarea', y = mean('per'))) +
      geom_bar()
  })
  
  
  # Mosaic Plot
    
    #Reactive data for mosaic
  data_day2 <- reactive({
    req(input$day2)
    data.frame(datasbi_g[[as.numeric(input$day2)]])
  })
  
  output$mosaic <- renderPlot({
    mosaic(with(data_day2(), tapply(quan, list(sarea, time), FUN=sum)), shade = T, color = T, labeling = labeling_border(rot_labels = c(90, 90, 0, 0)))
  }, height = 600)
  
    # Residual
  output$residual <- DT::renderDataTable({
    DT::datatable(data = round(chisq.test(with(data_day2(), tapply(quan, list(sarea, time), FUN=sum)))$residuals, 2),
                  options = list(pageLength = 12))
  })
  
    # Expected
  output$expected <- DT::renderDataTable({
    DT::datatable(data = round(chisq.test(with(data_day2(), tapply(quan, list(sarea, time), FUN=sum)))$expected, 0),
                  options = list(pageLength = 12))
  })
  
    # Observation
  output$observation <- DT::renderDataTable({
    DT::datatable(data = with(data_day2(), tapply(quan, list(sarea, time), FUN=sum)),
                  options = list(pageLength = 12))
  })
  
  # Summary
  
  # Function to save range for use in ggplot
  gg_range <- function(x) {
    data.frame(ymin = min(x), # Min
               ymax = max(x)) # Max
  }
  
  # Function to get quantiles
  med_IQR <- function(x) {
    data.frame(y = median(x), # Median
               ymin = quantile(x)[2], # 1st quartile
               ymax = quantile(x)[4])  # 3rd quartile
  }
  output$barchart<- renderPlot({
    ggplot(data_day1(), aes_string(x = 'sarea', y = input$time1)) +
      geom_point(shape = 1, alpha = 0.5, position = position_jitter(width = 0.1)) +
      stat_summary(fun.data = med_IQR, geom = 'linerange', col = "#0088A8", size = 3, alpha = 0.8) +
      stat_summary(fun.data = gg_range, geom = 'linerange', width = 0.2, col = "#0088A8", alpha = 0.2, size = 3) +
      stat_summary(geom = 'point', fun.y = median,
                   size = 3,
                   fill = '#CCEEFF', col = 'red', shape = 21) +
      ggtitle('Five-number Summary Plot')
  })
  
  # Line Plot Data
  sbi <- read.csv('data/Youbike_sbi(1).csv')
  sbi_line <- sbi %>% gather( Time, value, X2018.8.1.7:X2018.8.4.9, na.rm = TRUE)
  sbi_line <- select(sbi_line,sarea,Time,value)
  sbi_line <- sbi_line %>% group_by(sarea, Time) %>% summarise(mean = mean(value), sum = sum(value))
  sbi_line$Time <- str_replace(sbi_line$Time, 'X', '')
  sbi_line$Time <- ymd_h(sbi_line$Time)
  sbi_line <- separate(sbi_line, Time, c("day", "hour"),sep = " ",remove = FALSE)
  
  all_regions <- sort(unique(sbi_line$sarea))
  
  min_date <- min(sbi_line$Time)
  max_date <- max(sbi_line$Time)
  
  
  shade <- read.csv('data/shade.csv')
  shade$x1<-ymd_h(shade$x1)
  shade$x2<-ymd_h(shade$x2)
  
  # Line Plot: mean
  
  selected <- reactive({
    req(input$Time)
    req(input$sarea)
    sbi_line %>% 
      filter(sarea %in% input$sarea) %>%
      filter(day >= as.Date(input$Time[1], "%Y-%m-%d") & day <= as.Date(input$Time[2], "%Y-%m-%d"))
  })
  
  output$line_mean <- renderPlot({
    
    ggplot() + 
      geom_line(data = selected(), aes_string(x = 'Time', y = 'mean', color = 'sarea'))+
      geom_rect(data = shade, mapping = aes_string(xmin = 'x1', xmax = 'x2', ymin = -Inf, ymax = Inf), fill = '#DDDDDD') +
      ylim(5,25)
  }, height = 600)
  
  # Line Plot: Sum
  
  output$line_sum <- renderPlot({
    
    ggplot() + 
      geom_line(data = selected(), aes_string(x = 'Time', y = 'sum', color = 'sarea'))+
      geom_rect(data = shade, mapping = aes_string(xmin = 'x1', xmax = 'x2', ymin = -Inf, ymax = Inf), fill = '#DDDDDD') +
      ylim(100, 1100)
  }, height = 600)
  
}