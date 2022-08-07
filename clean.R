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