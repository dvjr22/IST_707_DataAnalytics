
wordPercentage = function(df, frequency){
  count = 0
  index = c()
  for (c in 1:ncol(df)) {

    colCount = 0

    for (r in 1:nrow(df)) {
      if(df[r,c] > 0) {
        colCount=colCount+1
      }
    }
    if(colCount/nrow(df) > frequency)
      index = c(index,c)
  }
  return(index)
}

cols_to_rem = wordPercentage(Fed_DF, .7)

newFed = Fed_DF[,cols_to_rem] #105 variables
