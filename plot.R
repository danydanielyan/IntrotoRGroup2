plot_one_dimensional = function(data, first) 
{
    if(is.numeric(data[,first])) {
        ggplot(data, aes_string(x = first)) + 
            geom_histogram(bins = 50,fill = "#FC8D62")
    } 
    else if(is.factor(data[,first])) {
        lengthLevels = length(levels(data[,first]))    
        ggplot(data, aes_string(x = first, fill = first)) +
            geom_bar() + scale_fill_brewer(palette="Set2")
    }
    else { NULL }
}

plot_two_dimensional = function(data, first, second) 
{
    if(is.numeric(data[,first]) & is.numeric(data[,second])) {
        ggplot(data, aes_string(x=first, y=second)) +
            geom_point()+ scale_fill_brewer(palette="Set2")
    }
    else if(is.numeric(data[,first]) & is.factor(data[,second])) {
        ggplot(data, aes_string(x=first, fill=second)) +
            geom_bar() + scale_fill_brewer(palette="Set2")
        
    }
    else if(is.factor(data[,first]) & is.numeric(data[,second])) {
        ggplot(data, aes_string(x=second, fill=first)) +
            geom_bar() + scale_fill_brewer(palette="Set2")    
    }
    else if(is.factor(data[,first]) & is.factor(data[,second])) {
        ggplot(data, aes_string(x=second, fill=first)) +
            geom_bar() + scale_fill_brewer(palette="Set2")
    }
    else { NULL }
}
