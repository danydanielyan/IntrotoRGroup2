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

source("clean.R")
source("plot.R")

options(shiny.maxRequestSize = 40*1024^2)
thematic::thematic_shiny(font = "auto")

if (interactive()) {
  
  
  header = dashboardHeader(
    title = "DATA EXPLORER"
  )
  
  
  body = dashboardBody( 
    shinyDashboardThemes(theme = "grey_dark"),
    fluidRow(plotOutput("plot")),
 
    fluidRow(
      column(4,uiOutput('SelectCategory1')),
      column(4,uiOutput('SelectPlotType'))
  ),
    fluidRow(
      column(4,uiOutput('SelectCategory2'))
  ),
    fluidRow(
     column(4,uiOutput('SelectCategory3'))
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
  
  server <- function(input, output) {
    
    dataframe <- reactive({
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      
      data = read.csv(file= inFile$datapath, sep=input$sep)
      dataNotCleared = data
      
      data = fill_missing_values(dataNotCleared,input$clean, input$outlier)
      dataNotDate = data
      if(input$submit > 0)
        data = formate_data(isolate(input$date),dataNotDate)
      return(data)
    })
    
    output$SelectPlotType = renderUI({
      
      data = dataframe()
      if(is.null(data))
        return(NULL)
      
      if(input$number == 1) {
        print(input)
        if(is.numeric(data[,input$first])) {
          return(selectInput("choosePlot", "Choose a Plot",
                             choices = c("Histogram","Bar Plot")))
        }
      }else{return(NULL)}
    })
    
    output$SelectCategory1 = renderUI({
      
      
      data = dataframe()
      if(is.null(data))
        return(NULL)
      vec = c()
      col = colnames(data)
      for(i in 1:length(colnames(data))) {
        if(!is.Date(data[,i])) {
          vec = c(vec, col[i])
        }
      }
      if(input$number == 1) {
      return(selectInput("first", "Variable 1",
                  choices = vec))
      }else{
      return(selectInput("first", "Variable 1",
                           choices = col))
      }
    })
    output$SelectCategory2 = renderUI({
      
      data = dataframe()
      if(is.null(data))
        return(NULL)
      if(input$number < 2)
        return(NULL)
      selectInput("second", "Variable 2",
                  choices = colnames(data)) 
    }) 
    output$SelectCategory3 = renderUI({
      
      data = dataframe()
      if(is.null(data))
        return(NULL)
      if(input$number < 3)
        return(NULL)
      selectInput("third", "Variable 3",
                  choices = colnames(data)) 
    }) 
    
    #3. Data Transformation: DON'T HAVE A CLUE HOW TO DO THIS
    #4.Plotting Part.I feel that We should change everything here.But I should have start from somewhere 
    #to have a small thing. 
    
    #PLOTING
    
    
    output$plot = renderPlot({
      
      dataset = dataframe()
      if(is.null(dataset)) return(NULL)
      
      if(input$number == 1) {
        plot_one_dimensional(dataset, input$first, input$choosePlot)
      }
      else if(input$number == 2) {
        plot_two_dimensional(dataset, input$first, input$second)  
      }
      else{
        plot_three_dimensional(dataset, input$first, input$second, input$third)
      }
      
    })
    
  }
  
  shinyApp(ui, server)
  
}