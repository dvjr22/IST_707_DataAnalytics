# Diego Valdes
# IST 707
# Apr 14, 2019
# Functions


# =============================================================================================================
# get number of na in each column
#
naInData = function(dataset) {
  
  nas = c()
  for (i in 1:length(dataset)) {
    nas = cbind(nas, sum(nas, is.na(dataset[,i])))   
  }
  nas = data.frame(nas)
  colnames(nas) = colnames(dataset)
  return(nas)
}


# =============================================================================================================
# converts cols in a dataset to factor
#
toFactor = function(dataset, columns) {
  
  for (i in 1:length(columns)) {
    
    dataset[,columns[i]] = as.factor(dataset[,columns[i]])
    
  }
  return(dataset)
}

# =============================================================================================================
# converts cols in a dataset to numeric
#
toNumeric = function(dataset, columns) {
  
  for (i in 1:length(columns)) {
    
    dataset[,columns[i]] = as.numeric(dataset[,columns[i]])
    
  }
  return(dataset)
}
