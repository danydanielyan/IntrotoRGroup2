library(ggplot2)
library(shiny)

if (interactive()) {
  
  ui <- fluidPage(
    
    theme = shinythemes::shinytheme("superhero"),
    titlePanel("Data Explorer"),
    
    sidebarLayout(
      sidebarPanel(
        #1. Browsing data
        fileInput("file", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        
        #MAYBE WE SHOULD ADD MORE FILE EXTENTIONS. FOR EXAMPLE: .tsv.
        
        uiOutput('SelectCategory1'),
        uiOutput('SelectCategory2')),
        mainPanel(plotOutput("plot")
)
    )
  )
  
  server <- function(input, output) {
    
    dataframe <- reactive({
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      
      data = read.csv(inFile$datapath)
      
      #2. PREPROCESS AND CLEAR THE DATA HERE!!!!!!!!!!!!!!!!!!!!!
      for (col in 1:ncol(data)){
        if(is.character(data[,col])){
          count = length(unique(data[,col]))
          lengthOfCol = nrow(data)
          prc = count / lengthOfCol * 100
          if(prc < 90) #Remember to ask if we should decrease this number
          {
            data[,col] = as.factor(data[,col]) 
          }
        }
      }
      data
    })
    
    output$SelectCategory1 <-renderUI({
     
      
      data = dataframe()
      if(is.null(data))
        return(NULL)
      
      selectInput("first", "X axes",
                  choices = colnames(data)) 
    })
    output$SelectCategory2 <-renderUI({
      
      data = dataframe()
      if(is.null(data))
        return(NULL)
      
      selectInput("second", "Y axes",
                  choices = colnames(data)) 
    }) 
    #3. Data Transformation: DON'T HAVE A CLUE HOW TO DO THIS
    #4.Plotting Part.I feel that We should change everything here.But I should have start from somewhere 
    #to have a small thing. 
    output$plot <- renderPlot({
      
      data1 = dataframe()
      if(is.null(data1))
        return(NULL)
      if(is.numeric(data1[,input$first]) & is.numeric(data1[,input$second]))
      {
        ggplot(data=data1, aes_string(x=input$first, y=input$second)) +
        geom_point() + theme_classic()

      }
      else if(is.numeric(data1[,input$first]) & is.factor(data1[,input$second]))
      {
        ggplot(data=data1, aes_string(x=input$first, fill=input$second)) +
          geom_bar() + theme_classic() 
      }
      else if(is.factor(data1[,input$first]) & is.numeric(data1[,input$second]))
      {
        ggplot(data=data1, aes_string(x=input$second, fill=input$first)) +
          geom_bar() + theme_classic() 
      }
      else if(is.factor(data1[,input$first]) & is.factor(data1[,input$second]))
      {
        ggplot(data=data1, aes_string(x=input$second, fill=input$first)) +
          geom_bar() + theme_classic() 
      }
    })
    
  }
  
  shinyApp(ui, server)
}