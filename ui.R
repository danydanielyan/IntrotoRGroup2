library(ggplot2)
library(shiny)
library(dplyr)
library(stringr)
library(DT)
library(bslib)
library(shinydashboard)
library(dashboardthemes)
library(gganimate)
library(stringr)
library(lubridate)
library(wordcloud)
library(tm)
library(rainbow)
library(ggmosaic)
library(shinycssloaders)
library(shinybusy)
library(shinyalert)
library(reshape2)
library(SnowballC)
library(enquo)
library(echarts4r)

source("clean.R")
source("plot.R")
source("server.R")
source("plot_inter.R")

thematic::thematic_shiny(font = "auto")

if (interactive()) {
  header = dashboardHeader(
    title = "DATA EXPLORER"
    
  )
  
  body = dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    
    fluidRow(
      tabBox(
        title = "Plots", width = 12,
        id = "tabset1", height = "250px",
        tabPanel("Simple Plot", withSpinner(plotOutput("plot"))),
        tabPanel("Interactive Plot", withSpinner(echarts4rOutput("plotI")))
      )),
    
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    fluidRow(
      column(4,(uiOutput('SelectCategory1'))),
      column(4,(uiOutput('SelectPlotType'))),
      column(4,(uiOutput('SelectCategorical')))
    ),
    fluidRow(
      column(4,(uiOutput('SelectCategory2'))),
      column(4,(uiOutput("Histogrambarcount")))
    ),
    fluidRow(
      column(4,(uiOutput('SelectCategory3')))
    ),
    br(),
    br(),
    br(),
    br(),
    fluidRow(
      verbatimTextOutput("strfile")
    ),
  )
  
  sideBar = dashboardSidebar(
    fluidRow(column(6),column(4,actionButton("about", "About"))),
    selectInput('sep','Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
    fileInput("file", "Choose a File"),
    selectInput('clean','Clean Numerics By ', choices = c(Mean = "mean", Median = "median"), selected = 'mean'),
    checkboxInput("outlier","Remove Outliers", value = FALSE),
    selectInput('number','Number Of Variables To Plot ', choices = c(1, 2,3), selected = 1),
    textInput("date", "Date Variables",""),
    fluidRow(column(6),column(3,actionButton("submit", "Done"))),
    checkboxInput("corr","Show Correlation Heatmap", value = FALSE)
  )
  
  ui <- dashboardPage(
    header,
    sideBar,
    body
  )
  
  shinyApp(ui, server)
}
