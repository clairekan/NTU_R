library(shiny)
library(ggplot2)
library(markdown)

res <- res <- read.csv('D:/gitHub/NTU_R/Final/Youbike_res.csv')
choice.type <-
  c('X307', 'X307', 'X308', 'X309')

navbarPage(
  "Ubike",
  tabPanel(
    "Introduction",
    tags$h1("This is an analysis of the Ubike data."),
    tags$p("don't know what to say yet.")
  ),
  tabPanel(
    "Raw Data",
    tags$h1("Let's take a look at the dataset."),
    br(),
    fluidRow(column(
      8,
      tabPanel("Table",
               DT::dataTableOutput("data.raw"))
    ))
  ),
  tabPanel(
    "PartA.",
    tags$h1("Box Plot"),
    sidebarLayout(
      sidebarPanel(
        selectInput('time', 'Time', choice.type, selectize = TRUE),
       width=4
      ),
      mainPanel(
        verbatimTextOutput("x"),
        plotOutput('plot'))
    )
  )
  

)




