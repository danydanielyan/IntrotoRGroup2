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