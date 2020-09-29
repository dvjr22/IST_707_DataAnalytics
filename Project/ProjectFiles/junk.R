(statePlotData = aggregate(x=cbind(
  x10.Earnins.median = data.csc$X10_yrs_after_entry.median),
  by = list(State = data.csc$state),
  FUN = mean))

statePlotData$StateName = tolower(abbr2state(statePlotData$State))


#StatePlotData = as.data.frame(tapply(data.csc$X10_yrs_after_entry.median, data.csc$state, median))
colnames(statePlotData) = c("state", "median", "stateName")


colnames(data.csc)
Fin.Filtered = data.csc[,c(1:3,7:8,14:15,22:24,37,38,33,34)]
private.profit = Fin.Filtered[Fin.Filtered$ownership == "Private for-profit", ]
private.nonprofit = Fin.Filtered[Fin.Filtered$ownership == "Private nonprofit", ]
public = Fin.Filtered[Fin.Filtered$ownership == "Public", ]

colnames(Fin.Filtered)
fin.filt.v2 = Fin.Filtered[,c(3,11:14)]
colnames(fin.filt.v2) = c('Ownership','x6.mean','x6.median','x10.mean','x10.median')
test = melt(fin.filt.v2, id.vars = 'ownership')
