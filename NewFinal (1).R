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
      column(4,uiOutput('SelectCategory2')),
      column(4,uiOutput('SelectCategory3')))
    )
  
  
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
  
  
  
  
#DATA CLEANING ---------------------------------------------------------------------------------------------------------------------------------------------
  
  calc_mode <- function(vec){
    str_to_lower(vec)
    distinct_values <- unique(vec[!is.na(vec)])
    distinct_tabulate <- tabulate(match(vec, distinct_values))
    distinct_values[which(distinct_tabulate == max(distinct_tabulate))]
  }
  remove_outliers <- function(column){
    Q1 <- quantile(column, .25)
    Q3 <- quantile(column, .74)
    IQR <- IQR(column)
    filtered <- subset(column, column > (Q1 - 1.5*IQR) & column< (Q3 + 1.5*IQR))
    return(filtered)}
  
  find_factors = function(dataset)
  {
    for (col in 1:ncol(dataset)){
      if(is.character(dataset[,col])){
        count = length(unique(dataset[,col]))
        lengthOfCol = nrow(dataset)
        prc = count / lengthOfCol * 100
        if(prc < 80) 
        {
          dataset[,col] = as.factor(dataset[,col]) 
        }
      }
    }
    return(dataset)
  }
  fill_missing_values <- function(dataset,select,outlier){
    
    dataset = find_factors(dataset)
    for( i in 1:length(dataset)){
      if(is.numeric(dataset[[i]])){
        if(select == "mean"){
          col_mean = sum(dataset[[i]], na.rm = T)/sum(!is.na(dataset[[i]]))
          for(j in 1:length(dataset[[i]])){
            if(is.na(dataset[j,i])){
              dataset[j,i] <- round(col_mean,digits = 4)
            }}
        }
        else if(select == "median"){
          col_median = median(dataset[[i]], na.rm = T)
          for(j in 1:length(dataset[[i]])){
            if(is.na(dataset[j,i])){
              dataset[j,i] <- round(col_median,digits = 4)
            }
          }}
        if(outlier == TRUE){
          remove_outliers(dataset[[i]])
        }
      }else{
        for(m in 1:length(dataset[[i]])){
          str_to_lower(dataset[m,i])
          if(is.na(dataset[m,i]) || dataset[m,i] == "" || is.null(dataset[m,i]) ){
            if(length(calc_mode(dataset[[i]])) > 1){
              dataset[m,i] <- calc_mode(dataset[[i]])[1]
            }
            else {
              dataset[m,i] <- calc_mode(dataset[[i]])
            }
          }
        }
      } }
    return (dataset)
  }
  
  formate_data = function(date,dataset)
  {
    if(!is.null(dataset)){
    vec = colnames(dataset)
    all_dates <- str_split(date, ",", simplify = T)
    all_dates = as.vector(all_dates)
    if(length(all_dates) != 0){
      for(i in 1:length(all_dates)){
        for(j in 1:length(vec))
        {
          if(vec[j] == all_dates[i])
          {
            mdy <- mdy(dataset[,all_dates[i]])
            dmy <- dmy(dataset[,all_dates[i]])
            ymd <- ymd(dataset[,all_dates[i]])
            ydm <- ydm(dataset[,all_dates[i]])
            mdy[is.na(mdy)] <- dmy[is.na(mdy)]
            mdy[is.na(mdy)] <- ydm[is.na(mdy)]
            mdy[is.na(mdy)] <- ymd[is.na(mdy)]
            dataset[,all_dates[i]] <- mdy
            break
          }
        }
      }
    }
    return(dataset)
    }
  }
  
  #THE END OF DATA CLEANING ---------------------------------------------------------------------------------------------------------------------------------------------

  
  
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
      print(str(data))
      return(data)
    })
    
    
    output$SelectCategory1 <-renderUI({
      
      
      data = dataframe()
      if(is.null(data))
        return(NULL)
      
      selectInput("first", "Variable 1",
                  choices = colnames(data)) 
    })
    output$SelectCategory2 <-renderUI({
      
      data = dataframe()
      if(is.null(data))
        return(NULL)
      if(input$number < 2)
        return(NULL)
      selectInput("second", "Variable 2",
                  choices = colnames(data)) 
    }) 
    output$SelectCategory3 <-renderUI({
      
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
    
  
    output$plot <- renderPlot({
      
      data1 = dataframe()
      if(is.null(data1))
        return(NULL)
      if(input$number == 1)
      {
          if(is.numeric(data1[,input$first]))
          {
            ggplot(data1, aes_string(x = input$first)) + geom_histogram(bins = 50,fill = "#FC8D62")
          }
          else if(is.factor(data1[,input$first]))
          {
            lengthLevels = length(levels(data1[,input$first]))
            
            ggplot(data1, aes_string(x = input$first, fill = input$first)) +
              geom_bar() +scale_fill_brewer(palette="Set2")
          }
          else{
            NULL
          }
      }
      else if(input$number == 2)
      {
        if(is.numeric(data1[,input$first]) & is.numeric(data1[,input$second]))
        {
          ggplot(data1, aes_string(x=input$first, y=input$second)) +
            geom_point()+ scale_fill_brewer(palette="Set2")
        
          
        }
        else if(is.numeric(data1[,input$first]) & is.factor(data1[,input$second]))
        {
          ggplot(data1, aes_string(x=input$first, fill=input$second)) +
            geom_bar() + scale_fill_brewer(palette="Set2")
        
        }
        else if(is.factor(data1[,input$first]) & is.numeric(data1[,input$second]))
        {
          ggplot(data1, aes_string(x=input$second, fill=input$first)) +
            geom_bar() + scale_fill_brewer(palette="Set2")
        
        }
        else if(is.factor(data1[,input$first]) & is.factor(data1[,input$second]))
        {
          ggplot(data1, aes_string(x=input$second, fill=input$first)) +
            geom_bar() + scale_fill_brewer(palette="Set2")
        
        }
      }else{
        plotThreeVariables()
      }
      
    })
    
  }

  shinyApp(ui, server)
 
}