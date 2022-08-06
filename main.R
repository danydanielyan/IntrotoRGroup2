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

source("clean.R")
source("plot.R")
source("server.R")

thematic::thematic_shiny(font = "auto")

if (interactive()) {
  
  header = dashboardHeader(
    title = "DATA EXPLORER"
  )
  
  body = dashboardBody( 
    shinyDashboardThemes(theme = "grey_dark"),
    fluidRow(plotOutput("plot")),
    
    fluidRow(
      column(4,withSpinner(uiOutput('SelectCategory1'))),
      column(4,withSpinner(uiOutput('SelectPlotType')))
    ),
    fluidRow(
      column(4,withSpinner(uiOutput('SelectCategory2')))
    ),
    fluidRow(
      column(4,withSpinner(uiOutput('SelectCategory3')))
    ),
    
    fluidRow(
      column(4)
    ))

  sideBar = dashboardSidebar(
    fileInput("file", "Choose a File"),
    
    selectInput('sep','Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
    selectInput('clean','Clean Numerics By ', choices = c(Mean = "mean", Median = "median"), selected = 'mean'),
    checkboxInput("outlier","Remove Outliers", value = FALSE),
    selectInput('number','Number Of Variables To Plot ', choices = c(1, 2,3), selected = 1),
    textInput("date", "Date Variables",""),
    fluidRow(column(3),column(4,actionButton("submit", "Done"))))
  
  ui <- dashboardPage(
    header,
    sideBar,
    body
  )

  shinyApp(ui, server)
}
