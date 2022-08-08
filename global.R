plot_heatmap = function(data)
{
  dat = select_if(data, is.numeric)
  corr_mat = round(cor(dat),2)
  melted_corr_mat = melt(corr_mat)
  return(ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) + 
           geom_tile()+
           scale_fill_gradient2(low = "#009E73", high = "#E69F00", mid = "#2D3741", midpoint = 0, limit = c(-1,1))+
           theme(axis.text.x = element_text(angle = 45,size = 12,hjust = 1), axis.text.y = element_text(size = 12),
                 axis.title.x = element_blank(), axis.title.y = element_blank())+
           coord_fixed())
}
plot_one_dimensional = function(data, first, plot, group,slider) 
{
  if(is.numeric(data[,first]) & group == "") {
    if(plot == "Histogram") {
      return(ggplot(data, aes_string(x = first)) + 
               geom_histogram(bins = as.numeric(slider),fill = "#FDB462"))
    }
    else if(plot == "Bar Plot") {
      return(ggplot(data, aes_string(x = first,fill = first)) + 
               geom_bar(fill = "#FDB462"))
    }
  } 
  else if(is.numeric(data[,first]) & group != "") {
    df = data %>% 
      group_by( !!!rlang::syms(group)) %>% 
      summarize(min = min( !!!rlang::syms(first)), max = max( !!!rlang::syms(first)))
    print(head(df))
    vec = colnames(df)
    return(ggplot(df, aes_string(x = "min" , y = "max",color = group)) +
             geom_point(position = "jitter",width = .2) + geom_smooth(method = "lm"))
  }
  else if(is.factor(data[,first])) {
    return(ggplot(data, aes_string(x = first, fill = first)) +
             geom_bar())
  }
  else if(is.character(data[,first])) {
    corpus <- Corpus(VectorSource(data[,first]))
    corpus <- tm_map(corpus,  removeNumbers)
    corpus <- tm_map(corpus, tolower)
    tdm <-TermDocumentMatrix(corpus, control=list(wordLengths=c(1,Inf)))
    freq <- slam::row_sums(tdm)
    words <- names(freq)    
    wordcloud(words, freq, min.freq = 1,max.words=200,random.order=FALSE, colors=brewer.pal(8, "Set2"))
  }else { NULL }
}

plot_two_dimensional = function(data, first, second, plot) 
{
  if(is.numeric(data[,first]) & is.numeric(data[,second])) {
    if(plot == "Simple Scatter Plot") {
      return(ggplot(data, aes_string(x=first, y=second)) +
               geom_point(position = "jitter",width = .2,color = "#FDB462"))
    }
    else if(plot == "Scatter plot|regression line") {
      return(ggplot(data, aes_string(x=first, y=second)) +
               geom_point(position = "jitter",width = .2,color = "#FDB462")+
               geom_smooth(method = "lm"))
    }
  }
  else if(is.numeric(data[,first]) & is.factor(data[,second])) {
    if(plot == "Bar Plot") {
      return(ggplot(data, aes_string(x=first, fill=second)) +
               geom_bar())
    }
    else if(plot == "Bar Plot Facet") {
      return(ggplot(data, aes_string(x=first, fill=second)) +
               geom_bar() + facet_grid(second))
    }
    else if(plot == "Histogram") {
      return(ggplot(data, aes_string(x=first, fill=second)) +
               geom_histogram())
    }
    else if(plot == "Box Plot") {
      return(ggplot(data, aes_string(x=first, fill=second)) +
               geom_boxplot())
    }
  }
  else if(is.factor(data[,first]) & is.numeric(data[,second])) {
    if(plot == "Bar Plot") {
      return(ggplot(data, aes_string(x=second, fill=first)) +
               geom_bar())
    }
    else if(plot == "Bar Plot Facet") {
      return(ggplot(data, aes_string(x=second, fill=first)) +
               geom_bar() + facet_grid(first))
    }
    else if(plot == "Histogram") {
      return(ggplot(data, aes_string(x=second, fill=first)) +
               geom_histogram())
    }
    else if(plot == "Box Plot") {
      return(ggplot(data, aes_string(x= second, fill= first)) +
               geom_boxplot())
    }   
  }
  else if(is.factor(data[,first]) & is.factor(data[,second])) {
    if(plot == "Bar Plot") {
      return(ggplot(data, aes_string(x=second, fill=first)) +
               geom_bar())
    }
    else if(plot == "Mosaic Plot"){
      counts = table(data[,first],data[,second])
      return(mosaicplot(counts, col="#FDB462"))
    }
  }
  else if(is.numeric(data[,first]) & is.Date(data[,second])) {
    
    return(ggplot(data, aes_string(x = second, y = first)) +
             geom_point(position = "jitter",width = .2,color = "#FDB462"))
  }
  else if(is.Date(data[,first]) & is.numeric(data[,second])) {
    return(ggplot(data, aes_string(x = first, y = second)) +
             geom_line(color = "#FDB462"))
  }else{NULL}
}

plot_three_dimensional = function(data, first, second, third, plot)
{
  if(is.numeric(data[,first]) & is.numeric(data[,second]) & is.numeric(data[,third])) {
    ggplot(data, aes_string(x = first , y = second, color = third)) +
      geom_point(position = "jitter",width = .2,color = "#FDB462") 
  }
  else if(is.numeric(data[,first]) & is.numeric(data[,second]) & is.factor(data[,third])) {
    if(plot == "Simple Scatter Plot") {
      return(ggplot(data, aes_string(x = third , y = first, color = second)) +
               geom_point(position = "jitter",width = .2) )
    }
    else if(plot == "Scatter Plot|Cat. colour") {
      return(ggplot(data, aes_string(x = first , y = second, color = third)) +
               geom_point(position = "jitter",width = .2) )
    }
    else if(plot == "Facet Scatter Plot") {
      return(ggplot(data, aes_string(x = first , y = second,color = third)) +
               geom_point(position = "jitter",width = .2) + facet_wrap(third))
    }
    else if(plot == "Smooth Method Scatter Plot") {
      return(ggplot(data, aes_string(x = first , y = second,color = third, shape = third)) +
               geom_point(position = "jitter",width = .2) + geom_smooth(method = "lm"))
    }
  }
  else if(is.numeric(data[,first]) & is.factor(data[,second]) & is.numeric(data[,third])) {
    if(plot == "Simple Scatter Plot") {
      return(ggplot(data, aes_string(x = second , y = first, color = third)) +
               geom_point(position = "jitter",width = .2) )
    }
    else if(plot == "Scatter Plot|Cat. colour") {
      return(ggplot(data, aes_string(x = first , y = third, color = second)) +
               geom_point(position = "jitter",width = .2) )
    }
    else if(plot == "Facet Scatter Plot") {
      return(ggplot(data, aes_string(x = first , y = third,color = second)) +
               geom_point(position = "jitter",width = .2) + facet_wrap(second))
    }
    else if(plot == "Smooth Method Scatter Plot") {
      return(ggplot(data, aes_string(x = first , y = third, color = second, shape = second)) +
               geom_point(position = "jitter",width = .2) + geom_smooth(method = "lm"))
    } 
  }
  else if(is.factor(data[,first]) & is.numeric(data[,second]) & is.numeric(data[,third])) {
    if(plot == "Simple Scatter Plot") {
      return(ggplot(data, aes_string(x = first , y = second, color = third)) +
               geom_point(position = "jitter",width = .2) )
    }
    else if(plot == "Scatter Plot|Cat. colour") {
      return(ggplot(data, aes_string(x = second , y = third, color = first)) +
               geom_point(position = "jitter",width = .2) )
    }
    else if(plot == "Facet Scatter Plot") {
      return(ggplot(data, aes_string(x = second , y = third,color = first)) +
               geom_point(position = "jitter",width = .2) + facet_wrap(first))
    }
    else if(plot == "Smooth Method Scatter Plot") {
      return(ggplot(data, aes_string(x = second , y = third,color = first, shape = first)) +
               geom_point(position = "jitter",width = .2) + geom_smooth(method = "lm"))
    } 
  }
  else if(is.factor(data[,first]) & is.factor(data[,second]) & is.numeric(data[,third])){ 
    return(ggplot(data, aes_string(x=first, y= third, fill=second)) + geom_boxplot())
    
  }
  else if(is.factor(data[,first]) & is.numeric(data[,second]) & is.factor(data[,third])) {
    return(ggplot(data, aes_string(x=first, y= second, fill= third)) + geom_boxplot())
  }
  else if(is.numeric(data[,first]) & is.factor(data[,second]) & is.factor(data[,third])) {
    return(ggplot(data, aes_string(x=second, y= first, fill=third)) + geom_boxplot())
  }
  else if(is.factor(data[,first]) & is.factor(data[,second]) & is.factor(data[,third])) {
    if(first == second | second == third | first == third)
      return(NULL)
    ggplot(data,aes_string(x=first, fill=second)) + geom_bar(position = "dodge") + facet_wrap(third)
  }
}
plot_heatmap_inter = function(data)
{
  dat = select_if(data, is.numeric)
  corr_mat = round(cor(dat),2)
  melted_corr_mat = melt(corr_mat)
  return(ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) + 
           geom_tile()+
           scale_fill_gradient2(low = "#009E73", high = "#E69F00", mid = "#2D3741", midpoint = 0, limit = c(-1,1))+
           theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
                 axis.title.x = element_blank(), axis.title.y = element_blank())+
           coord_fixed())
}
plot_one_dimensional_inter = function(data, first, plot) 
{ 
  if(is.numeric(data[,first])) {
    if(plot == "Histogram") {
      print(" In Histogram ")
      yaxis <- paste("Number of occurances of  ", first)
      title <- paste("Histogram of ", first)
      e_plot <- e_charts(data) %>% 
        e_histogram_(first, color = "#FDB462") %>% 
        e_tooltip() %>%
        e_axis_labels(x = first, y = yaxis) %>%
        e_title(title) %>%
        e_legend(fontSize = 100,orinet = "vertical", right = "5", top = '15%', textAlign = 'center') %>%
        e_animation(duration = 3000) %>% 
        e_toolbox_feature("dataZoom") %>%
        e_toolbox_feature(feature = "reset") %>% 
        e_toolbox_feature("dataView") %>%
        e_toolbox_feature("saveAzImage") %>% 
        e_theme("chalk")
      return(e_plot)
    }
    else if(plot == "Bar Plot") {
      print(" In Barplot ")
      yaxis <- paste("Number of occurances of  ", first)
      title <- paste("Histogram of ", first)
      e_plot <- e_charts(data) %>% 
        e_bar_(first, color = "#FDB462") %>% 
        e_tooltip() %>%
        e_axis_labels(x = first, y = yaxis) %>%
        e_title(title) %>%
        e_legend(fontSize = 100,orinet = "vertical", right = "5", top = '15%', textAlign = 'center') %>%
        e_animation(duration = 3000) %>% 
        e_toolbox_feature("dataZoom") %>%
        e_toolbox_feature(feature = "reset") %>% 
        e_toolbox_feature("dataView") %>%
        e_toolbox_feature("saveAzImage") %>% 
        e_theme("chalk")
      return(e_plot)
    }
  } 
  #_______________________________________________________________
  #_______________________________________________________________
  else if(is.factor(data[,first])) {
    if(plot == "Bar Plot")  {
      print("In categorical barplot")
      x <- data %>% group_by(!!!rlang::syms(first)) %>% count()
      x$new <- get(first, x)
      yaxis <- paste("Number of occurances of  ", first)
      title <- paste("Histogram of ", first)
      e_plot <-  x %>%
        e_charts(new) %>%
        e_bar(n) %>% 
        e_tooltip(trigger = "axis") %>%
        e_axis_labels(x = first, y = yaxis) %>%
        e_title(title) %>%
        e_legend(fontSize = 100,orinet = "vertical", right = "5", top = '15%', textAlign = 'center',
                 selector = list(
                   list(type = "inverse", title = "Invert"),
                   list(type = "all", title = "Reset")
                 )) %>%
        e_animation(duration = 3000) %>% 
        e_toolbox_feature("dataZoom") %>%
        e_toolbox_feature(feature = "reset") %>% 
        e_toolbox_feature("dataView") %>%
        e_toolbox_feature("saveAzImage") %>% 
        e_theme("chalk")
      return(e_plot)
    }
  }
  else if(is.character(data[,first])) {
    corpus <- Corpus(VectorSource(data[,first]))
    corpus <- tm_map(corpus,  removeNumbers)
    tdm <-TermDocumentMatrix(corpus, control=list(wordLengths=c(1,Inf), removePunctuation=T, stopwords=T, stemming=T))
    freq <- slam::row_sums(tdm)
    words <- names(freq)    
    wordcloud(words, freq, min.freq = 1,max.words=200,random.order=FALSE, colors=brewer.pal(8, "Set3"))
  }
  else { NULL }
}

plot_two_dimensional_inter = function(data, first, second, plot) 
{
  if(is.numeric(data[,first]) & is.numeric(data[,second])) {
    #______________________________________________________
    if(plot == "Simple Scatter Plot"){
      print(" In Scatter ")
      title <- paste("Scatterplot of variables", first, " and ", second)
      e_plot <- e_charts_(data,first) %>% 
        e_scatter_(second) %>% 
        e_axis_labels(x = first, y = second) %>%
        e_tooltip(trigger = "axis") %>%
        e_title(title) %>%
        e_legend(fontSize = 100,orinet = "vertical", right = "5", top = '15%', textAlign = 'center',
                 selector = list(
                   list(type = "inverse", title = "Invert"),
                   list(type = "all", title = "Reset")
                 )) %>%
        e_animation(duration = 3000) %>% 
        e_toolbox_feature("dataZoom") %>%
        e_toolbox_feature(feature = "reset") %>% 
        e_toolbox_feature("dataView") %>%
        e_toolbox_feature("saveAzImage") %>% 
        e_theme("chalk")
      return(e_plot)
    }
    if(plot == "Scatter plot|regression line"){
      print(" In Scatter reg")
      title <- paste("Scatterplot of variables", first, " and ", second)
      data$first <- get(first, data)
      data$second <- get(second, data)
      e_plot <- e_charts_(data,first) %>% 
        e_scatter_(second) %>% 
        e_lm(second ~ first, name = "Linear model") %>%
        e_tooltip(trigger = "axis") %>%
        e_axis_labels(x = first, y = second) %>%
        e_title(title) %>%
        e_legend(fontSize = 100,orinet = "vertical", right = "5", top = '15%', textAlign = 'center',
                 selector = list(
                   list(type = "inverse", title = "Invert"),
                   list(type = "all", title = "Reset")
                 )) %>%
        e_animation(duration = 3000) %>% 
        e_toolbox_feature("dataZoom") %>%
        e_toolbox_feature(feature = "reset") %>% 
        e_toolbox_feature("dataView") %>%
        e_toolbox_feature("saveAzImage") %>% 
        e_theme("chalk")
      return(e_plot)
    }
    #___________________________________________________
  }
  if(is.numeric(data[,first]) & is.factor(data[,second])) {
    print("numeric and factor")
    print("In categorical barplot")
    data$new <- get(second, data)
    data$num <- get(first, data)
    x <- data %>% group_by(new)
    yaxis <- paste("Number of occurances of  ", first)
    title <- paste("Histogram of ", first)
    e_plot <- x %>%
      e_charts(num) %>%
      e_bar(new, stack = "grp") %>% 
      e_tooltip(trigger = "axis") %>%
      e_axis_labels(x = first, y = yaxis) %>%
      e_title(title) %>%
      e_legend(fontSize = 100,orinet = "vertical", right = "5", top = '15%', textAlign = 'center',
               selector = list(
                 list(type = "inverse", title = "Invert"),
                 list(type = "all", title = "Reset")
               )) %>%
      e_animation(duration = 3000) %>% 
      e_toolbox_feature("dataZoom") %>%
      e_toolbox_feature(feature = "reset") %>% 
      e_toolbox_feature("dataView") %>%
      e_toolbox_feature("saveAzImage") %>% 
      e_theme("chalk")
    return(e_plot)
  }
  if(is.factor(data[,first]) & is.numeric(data[,second])) {
    print("factor and numeric")
    data$new <- get(second, data)
    data$num <- get(first, data)
    x <- data %>% group_by(new)
    yaxis <- paste("Number of occurances of  ", first)
    title <- paste("Histogram of ", first)
    e_plot <- x %>%
      e_charts(new) %>%
      e_bar(num, stack = "grp") %>% 
      e_tooltip(trigger = "axis") %>%
      e_axis_labels(x = first, y = yaxis) %>%
      e_title(title) %>%
      e_legend(fontSize = 100,orinet = "vertical", right = "5", top = '15%', textAlign = 'center',
               selector = list(
                 list(type = "inverse", title = "Invert"),
                 list(type = "all", title = "Reset")
               )) %>%
      e_animation(duration = 3000) %>% 
      e_toolbox_feature("dataZoom") %>%
      e_toolbox_feature(feature = "reset") %>% 
      e_toolbox_feature("dataView") %>%
      e_toolbox_feature("saveAzImage") %>% 
      e_theme("chalk")
    return(e_plot)
  }
  if(is.factor(data[,first]) & is.factor(data[,second])) {
    print("factor and factor")
    data$new <- get(second, data)
    data$num <- get(first, data)
    x <- data %>% group_by(new)
    yaxis <- paste("Number of occurances of  ", first)
    title <- paste("Histogram of ", first)
    e_plot <- x %>%
      e_charts(new) %>%
      e_bar(num, stack = "grp") %>% 
      e_tooltip(trigger = "axis") %>%
      e_axis_labels(x = first, y = yaxis) %>%
      e_title(title) %>%
      e_legend(fontSize = 100,orinet = "vertical", right = "5", top = '15%', textAlign = 'center',
               selector = list(
                 list(type = "inverse", title = "Invert"),
                 list(type = "all", title = "Reset")
               )) %>%
      e_animation(duration = 3000) %>% 
      e_toolbox_feature("dataZoom") %>%
      e_toolbox_feature(feature = "reset") %>% 
      e_toolbox_feature("dataView") %>%
      e_toolbox_feature("saveAzImage") %>% 
      e_theme("chalk")
    return(e_plot)
  }
  else { NULL }
}

plot_three_dimensional_inter = function(data, first, second, third, plot_type)
{
  if(is.numeric(data[,first]) & is.numeric(data[,second]) & is.numeric(data[,third])) {
    print("in three numeric")
    yaxis <- paste(second)
    title <- paste(third, " and ", second, " dependency from ", first)
    e_plot <- data %>% e_charts_(first) %>% 
      e_line_(second, color = "#FDB462") %>% 
      e_area_(third) %>%
      e_tooltip() %>%
      e_axis_labels(x = first, y = yaxis) %>%
      e_title(title) %>%
      e_legend(fontSize = 100,orinet = "vertical", right = "5", top = '15%', textAlign = 'center') %>%
      e_animation(duration = 3000) %>% 
      e_toolbox_feature("dataZoom") %>%
      e_toolbox_feature(feature = "reset") %>% 
      e_toolbox_feature("dataView") %>%
      e_toolbox_feature("saveAzImage") %>% 
      e_theme("chalk")
    return(e_plot)   
  }
  else if(is.numeric(data[,first]) & is.numeric(data[,second]) & is.factor(data[,third])) {
    print("in two numeric and a categorical")
    #_______________________________
    yaxis <- paste(second)
    title <- paste(third, " and ", second, " dependency from ", first)
    data$add <- get(third, data)
    e_plot <- data %>% 
      group_by(add) %>% 
      e_charts_(first) %>% 
      e_scatter_(second, color = "#FDB462") %>%
      e_tooltip() %>%
      e_axis_labels(x = first, y = yaxis) %>%
      e_title(title) %>%
      e_legend(fontSize = 100,orinet = "vertical", right = "5", top = '15%', textAlign = 'center') %>%
      e_animation(duration = 3000) %>% 
      e_toolbox_feature("dataZoom") %>%
      e_toolbox_feature(feature = "reset") %>% 
      e_toolbox_feature("dataView") %>%
      e_toolbox_feature("saveAzImage") %>% 
      e_theme("chalk")
    
    #_______________________________
    ggplot(data, aes_string(x = first , y = second, color = third)) +
      geom_point(position = "jitter",width = .2, color ="#FDB462") 
  }
  else if(is.numeric(data[,first]) & is.factor(data[,second]) & is.numeric(data[,third])) {
    ggplot(data, aes_string(x = first , y = third, color = second)) +
      geom_point(position = "jitter",width = .2, color ="#FDB462") 
  }
  else if(is.factor(data[,first]) & is.numeric(data[,second]) & is.numeric(data[,third])) {
    ggplot(data, aes_string(x = second , y = third, color = first)) +
      geom_point(position = "jitter",width = .2, color ="#FDB462") 
  }
  else if(is.factor(data[,first]) & is.factor(data[,second]) & is.numeric(data[,third])) {
    ggplot(data, aes_string(x=first, y= third, fill=second)) + geom_boxplot()
  }
  else if(is.factor(data[,first]) & is.numeric(data[,second]) & is.factor(data[,third])) {
    ggplot(data, aes_string(x=first, y= second, fill=third)) + geom_boxplot()
  }
  else if(is.numeric(data[,first]) & is.factor(data[,second]) & is.factor(data[,third])) {
    ggplot(data, aes_string(x=second, y= first, fill=third)) + geom_boxplot()
  }
  else if(is.factor(data[,first]) & is.factor(data[,second]) & is.factor(data[,third])) {
    if(first == second | second == third | first == third)
      return(NULL)
    ggplot(data,aes_string(x=first, fill=second)) + geom_bar(position = "dodge") + facet_wrap(~third)
  }
  
}
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
  return(filtered)
}

find_factors = function(dataset)
{
  for (col in 1:ncol(dataset)){
    if(is.character(dataset[,col])){
      count = length(unique(dataset[,col]))
      lengthOfCol = nrow(dataset)
      prc = count / lengthOfCol * 100
      if(prc < 50) 
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

formate_date = function(date,dataset)
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