options(shiny.maxRequestSize = 40*1024^2)
server <- function(input, output) {
  
  observeEvent(input$about, {
    shinyalert("About", "This is group 2 Data Visualization R shiny project. Accepted input file types are .csv, .txt.", type = "info")
  })
  
  dataframe <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    data = read.csv(file= inFile$datapath, sep=input$sep)
    dataNotCleared = data
    
    show_modal_spinner(
      spin = "cube-grid",
      color = "firebrick",
      text = "Cleaning up Data, Please wait..."
    )
    
    data = fill_missing_values(dataNotCleared,input$clean, input$outlier)
    dataNotDate = data
    if(input$submit > 0)
      data = formate_date(isolate(input$date),dataNotDate)
    remove_modal_spinner()
    return(data)
  })
  
  output$SelectPlotType = renderUI({
    
    data = dataframe()
    if(is.null(data))
      return(NULL)
    if(input$corr == TRUE)
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
    if(input$corr == TRUE)
      return(NULL)
    vec = c()
    col = colnames(data)
    if(input$number == 1) {
      for(i in 1:length(col)) {
        if(!is.Date(data[,i])) {
          vec = c(vec, col[i])
        }
      }
      return(selectInput("first", "Variable 1",
                         choices = sort(vec)))
    }else{
      for(i in 1:length(col)) {
        if(!is.character(data[,i])) {
          vec = c(vec, col[i])
        }
      }
      return(selectInput("first", "Variable 1",
                         choices = sort(vec)))
    }
  })
  output$SelectCategory2 = renderUI({
    
    data = dataframe()
    if(is.null(data))
      return(NULL)
    if(input$number < 2)
      return(NULL)
    if(input$corr == TRUE)
      return(NULL)
    vec = c()
    col = colnames(data)
    if(input$number == 2){
      for(i in 1:length(col)) {
        if(!is.character(data[,i])) {
          vec = c(vec, col[i])
        }
      }
      return(selectInput("second", "Variable 2",
                         choices = sort(vec)))
    }
    else{
      for(i in 1:length(col)) {
        if(!is.character(data[,i]) & !is.Date(data[,i])) {
          vec = c(vec, col[i])
        }
      }
      return(selectInput("second", "Variable 2",
                         choices = sort(vec)))
    }
  }) 
  output$SelectCategory3 = renderUI({
    
    data = dataframe()
    if(is.null(data))
      return(NULL)
    if(input$number < 3)
      return(NULL)
    if(input$corr == TRUE)
      return(NULL)
    vec = c()
    col = colnames(data)
    for(i in 1:length(colnames(data))) {
      if(!is.character(data[,i]) & !is.Date(data[,i])) {
        vec = c(vec, col[i])
      }}
    selectInput("third", "Variable 3",
                choices = sort(vec)) 
  }) 
  
  output$SelectCategorical = renderUI({
    data = dataframe()
    if(is.null(data))
      return(NULL)
    if(input$number == 0)
      return(NULL)
    if(input$corr == TRUE)
      return(NULL)
    vec = c()
    col = colnames(data)
    for(i in 1:length(colnames(data))) {
      if(is.factor(data[,i])) {
        vec = c(vec, col[i])
      }}
    selectInput("categorical", "Group By",
                choices = sort(vec))
  }) 

  output$plot = renderPlot({
    
    dataset = dataframe()
    if(is.null(dataset)) return(NULL)
    
    if(input$corr == TRUE) {
     return(plot_heatmap(dataset))
    }
    if(input$number == 1 & input$corr == FALSE) {
      plot_one_dimensional(dataset, input$first, input$choosePlot)
    }
    else if(input$number == 2 & input$corr == FALSE) {
      plot_two_dimensional(dataset, input$first, input$second, input$choosePlot)  
    }
    else if(input$number == 3 & input$corr == FALSE) {
      plot_three_dimensional(dataset, input$first, input$second, input$third, input$choosePlot)
    }else{ return(NULL)}
  })
}