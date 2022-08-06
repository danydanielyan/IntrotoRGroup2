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
      data = formate_date(isolate(input$date),dataNotDate)
    print(head(data,n =20))
    print(str(data))
    return(data)
  })
  
  output$SelectPlotType = renderUI({
    
    data = dataframe()
    if(is.null(data))
      return(NULL)
    
    if(input$number == 1) {
      if(is.numeric(data[,input$first])) {
        return(selectInput("choosePlot", "Choose a Plot",
                           choices = c("Histogram","Bar Plot")))
      }else{return(NULL)}
    }
    else if(input$number == 2) {
      if(is.numeric(data[,input$first]) & is.numeric(data[,input$second])){
        return(selectInput("choosePlot", "Choose a Plot",
                           choices = c("Simple Scatter Plot","Scatter plot|regression line")))           
      }
      else if((is.factor(data[,input$first]) & is.numeric(data[,input$second])) | 
              (is.numeric(data[,input$first]) & is.factor(data[,input$second]))) {
        return(selectInput("choosePlot", "Choose a Plot",
                           choices = c("Bar Plot","Bar Plot Facet","Histogram","Box Plot")))
      }
      else if(is.factor(data[,input$first]) & is.factor(data[,input$second])) {
        return(selectInput("choosePlot", "Choose a Plot",
                           choices = c("Bar Plot","Mosaic Plot")))
      }
      else if((is.Date(data[,input$first]) & is.numeric(data[,input$second])) | 
              (is.numeric(data[,input$first]) & is.Date(data[,input$second]))) {
        return(selectInput("choosePlot", "Choose a Plot",
                           choices = c("Scatter Plot","Line Plot")))
      }
    }
    else if(input$number == 3) {
      if((is.numeric(data[,input$first]) & is.numeric(data[,input$second])
         & is.factor(data[,input$third])) | (is.numeric(data[,input$first]) 
         & is.factor(data[,input$second]) & is.numeric(data[,input$third])) | 
         (is.factor(data[,input$first]) & is.numeric(data[,input$second]) 
         & is.numeric(data[,input$third]))) {
        return(selectInput("choosePlot", "Choose a Plot",
                           choices = c("Simple Scatter Plot","Scatter Plot|Cat. colour","Facet Scatter Plot","Smooth Method Scatter Plot")))
      }
    }
    else{return(NULL)}
  })
  
  output$SelectCategory1 = renderUI({
    
    data = dataframe()
    if(is.null(data))
      return(NULL)
    vec = c()
    col = colnames(data)
    if(input$number == 1) {
      for(i in 1:length(colnames(data))) {
        if(!is.Date(data[,i])) {
          vec = c(vec, col[i])
        }
      }
      return(selectInput("first", "Variable 1",
                         choices = vec))
    }else if(input$number == 2) {
      for(i in 1:length(colnames(data))) {
        if(!is.character(data[,i])) {
          vec = c(vec, col[i])
        }
      }
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
    vec = c()
    col = colnames(data)
    if(input$number == 2){
      for(i in 1:length(colnames(data))) {
        if(!is.character(data[,i])) {
          vec = c(vec, col[i])
        }
      }
      return(selectInput("second", "Variable 2",
                         choices = vec))
    }
    selectInput("second", "Variable 2",
                choices = col) 
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
  
  output$plot = renderPlot({
    
    dataset = dataframe()
    if(is.null(dataset)) return(NULL)
    
    if(input$number == 1) {
      plot_one_dimensional(dataset, input$first, input$choosePlot)
    }
    else if(input$number == 2) {
      plot_two_dimensional(dataset, input$first, input$second, input$choosePlot)  
    }
    else{
      plot_three_dimensional(dataset, input$first, input$second, input$third, input$choosePlot)
    } 
  })
}