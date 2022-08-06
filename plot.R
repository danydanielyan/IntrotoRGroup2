plot_one_dimensional = function(data, first) 
{
  if(is.numeric(data[,first])) {
    ggplot(data, aes_string(x = first)) + 
      geom_histogram(bins = 50,fill = "#FDB462")
  } 
  else if(is.factor(data[,first])) {
    lengthLevels = length(levels(data[,first]))    
    ggplot(data, aes_string(x = first, fill = first)) +
      geom_bar() + scale_fill_brewer(palette="Set3")
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

plot_two_dimensional = function(data, first, second) 
{
  if(is.numeric(data[,first]) & is.numeric(data[,second])) {
    ggplot(data, aes_string(x=first, y=second)) +
      geom_point(position = "jitter",width = .2,color = "#FDB462")
  }
  else if(is.numeric(data[,first]) & is.factor(data[,second])) {
    ggplot(data, aes_string(x=first, fill=second)) +
      geom_bar() + scale_fill_brewer(palette="Set3")
    
  }
  else if(is.factor(data[,first]) & is.numeric(data[,second])) {
    ggplot(data, aes_string(x=second, fill=first)) +
      geom_bar() + scale_fill_brewer(palette="Set3")    
  }
  else if(is.factor(data[,first]) & is.factor(data[,second])) {
    ggplot(data, aes_string(x=second, fill=first)) +
      geom_bar() + scale_fill_brewer(palette="Set3")
  }
  else { NULL }
}

plot_three_dimensional = function(data, first, second, third)
{
  if(is.numeric(data[,first]) & is.numeric(data[,second]) & is.numeric(data[,third])) {
    ggplot(data, aes_string(x = first , y = second, color = third)) +
      geom_point(position = "jitter",width = .2, color ="#FDB462") 
  }
  else if(is.numeric(data[,first]) & is.numeric(data[,second]) & is.factor(data[,third])) {
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