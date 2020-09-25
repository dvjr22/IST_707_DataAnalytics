


# =============================================================================================================
# 
#
scaleDFCol = function(dataset, colsToScale, colsNotScale) {
  
  max = apply(dataset[,colsToScale], 2, max)
  min = apply(dataset[,colsToScale], 2, min)
  
  scaledDF = as.data.frame(cbind(dataset[,colsNotScale], scale(dataset[,colsToScale], center = min, scale = max-min)))
  return(scaledDF)
}



# =============================================================================================================
# 
#
scaleDFRow = function(dataset, colsToScale) {
  
  scaledRows = c()
  for (i in 1:nrow(dataset)) {
    
    newRow = t(dataset[i, colsToScale])
    max = max(dataset[i, colsToScale])
    min = min(dataset[i, colsToScale])
    newRow = scale(newRow, center = min, scale = max - min)
    
    dataset[i,colsToScale] = t(newRow)
  }
  return(dataset)
}






