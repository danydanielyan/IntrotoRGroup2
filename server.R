server = function(input, output) 
{   
    dataframe = reactive({
      inFile = input$file
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
      }
      else{return(NULL)}
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
          return(selectInput("first", "Variable 1", choices = vec))
      }
      else{
          return(selectInput("first", "Variable 1", choices = col))
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
