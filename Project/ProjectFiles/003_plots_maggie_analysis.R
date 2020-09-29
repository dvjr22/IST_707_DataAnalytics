#plots
dev.off(dev.list()["RStudioGD"]) # clear plots

us = map_data("state") # get map data
byState$StateName = tolower(abbr2state(byState$State))

mapPlot = ggplot(byState, aes(map_id = StateName)) 



scorecard



(Financial.Maggie = aggregate(x=cbind(
  SAT.Reading = scorecard$sat_scores.midpoint.critical_reading, 
  SAT.Math = scorecard$sat_scores.midpoint.math,
  Loan.Principal = scorecard$loan_principal, # 
  PellGrant.Debt = scorecard$median_debt.pell_grant,
  NoPellGrant.Debt = scorecard$median_debt.no_pell_grant,
  x6.Earnings.mean = scorecard$X6_yrs_after_entry.working_not_enrolled.mean_earnings,
  x6.Earnings.median = scorecard$X6_yrs_after_entry.median,
  x10.Earnings.mean = scorecard$X10_yrs_after_entry.working_not_enrolled.mean_earnings,
  x10.Earnins.median = scorecard$X10_yrs_after_entry.median),
  by = list(Ownership = scorecard$during.cluster),
  FUN = mean))


SAT.data = data.frame(rbind(Financial[,c(1,2,3)], Financial.Maggie[,c(1,2,3)]))
SAT.data$SAT.Reading = round(SAT.data$SAT.Reading)
SAT.data$SAT.Math = round(SAT.data$SAT.Math)
plotTable(SAT.data)


dept.M = Financial.Maggie[,c(1,5,6)]
deptMelt.M = melt(dept.M, id.vars = 'Ownership')
debtPlot.M = ggplot(deptMelt.M, aes(Ownership, value, fill=variable)) + geom_bar(stat = 'identity', position = 'dodge') + 
  ggtitle("Student Dept") + scale_fill_manual(values=c("#999999", "#E69F00"), name="Legend", labels=c('Pell Grant','No Pell Grant')) 



colnames(scorecard)
box.data.M = scorecard[,c(39,37,33)]
box.data.melt.M = melt(box.data.M)

# box plots, and outliers, and quartiles, oh my!
(debtPlotBox = ggplot(box.data.melt.M, aes(during.cluster, value, fill=variable)) + geom_boxplot() + 
    ggtitle("Average Earnings") + scale_fill_manual(values=c("#999999", "#E69F00"), name="Legend", labels=c('6 Years','10 Years')) +
    xlab(label = '') + ylab(label = 'Income') )


model.data.M = scorecard[,c(39,2,7,8,22,37,38,33,34)]


lm.data = model.data.M[,c(-2,-7,-9)]
colnames(lm.data)

lm.model = lm(X10_yrs_after_entry.working_not_enrolled.mean_earnings ~., lm.data)

lm.test.data = lm.data
(colNames = colnames(lm.test.data))
lm.test.data$X6_yrs_after_entry.working_not_enrolled.mean_earnings = lm.test.data$X10_yrs_after_entry.working_not_enrolled.mean_earnings

lm.test.data = lm.test.data[,-6]

lm.pred = predict(lm.model, lm.test.data)

lm.model.data = cbind(lm.data[,c(1,5:6)],lm.pred)

(pred.data = aggregate(x=cbind(x6 = lm.model.data$X6_yrs_after_entry.working_not_enrolled.mean_earnings,
                               x10 = lm.model.data$X10_yrs_after_entry.working_not_enrolled.mean_earnings,
                               pred = lm.model.data$lm.pred), 
                       by = list(Ownership=lm.model.data$during.cluster),
                       FUN = mean))



test = melt(pred.data)
years = c(6,6,6,6,10,10,10,10,18,18,18,18)
test$variable = as.factor(years)

test$log = log(test$value)
test$years = years
levels(test$during.cluster)

predEarningsPlot.M = ggplot(test, aes(Ownership, value, fill=variable)) + geom_bar(stat = 'identity', position = 'dodge') + 
  ggtitle("Earnings After College") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Legend", labels=c('After 6 Years','After 10 Years','After ~20 Years')) 


fin.data.M = Financial.Maggie[,c(1,4,7,9)]

colnames(fin.data.M) = c('Ownership','Loan Principal','Earnings 6yrs', 'Earnings 10yrs')


plotTable(fin.data.M)

md.mag = model.data.M

model.data.list.M = testingSample(md.mag)
trng.data.m = model.data.list.M[[1]]
test.data.m = model.data.list.M[[2]]

# randomize and create trng and test sets
# write them for future import to keep results consistant



rf.model.m = randomForest(during.cluster ~., trng.data.m[,-2])
# summary(rf.model)
# importance(rf.model.m)
rf.pred.m = predict(rf.model.m, test.data.m[,-2], type='class')
rf.table.m = table(rf.pred.m, test.data.m$during.cluster )
(rf.matrix.m = confusionMatrix(rf.table.m))



pfnp = test[test$Ownership == "Entry Level",]
pfp = test[test$Ownership == "Stable",]
pub = test[test$Ownership == "Reachers" ,]
pub1 = test[test$Ownership == "Privileged" ,]


pfnp = pfnp[,c(3,5)]
colnames(pfnp) = c('Entry Level','pfnp_years')
pfnp$pfnp_log = log(pfnp$`Entry Level`)

pfp = pfp[,c(3,5)]
colnames(pfp) = c('Stable','pfp_years')
pfp$pfp_log = log(pfp$Stable)

pub = pub[,c(3,5)]
colnames(pub) = c('Reachers','pub_years')
pub$pub_log = log(pub$Reachers)

pub1 = pub1[,c(3,5)]
colnames(pub1) = c('Privileged','pub1_years')
pub1$pub_log1 = log(pub1$Privileged)

log.data.m = cbind(pfp,pfnp,pub,pub1)


(rateChangePlot = ggplot(log.data.m, aes(x= pub_years)) + 
    geom_line(aes(y = pub_log), color = 'darkgreen') + 
    geom_line(aes(y = pfnp_log), color = 'darkorange') + 
    geom_line(aes(y = pfp_log), color = 'darkred') +
    geom_line(aes(y = pub_log1), color = 'darkblue') + xlab(label = 'Years') + ylab(label = 'Rate of Change'))


predPlotDF = pred.data
colnames(predPlotDF) = c('Ownership','Earnings 6yrs', 'Earnings 10yrs','Earnings ~20yrs')
plotTable(predPlotDF)

#---------------------------------------------------------------------------------------------------------------

#plots
dev.off(dev.list()["RStudioGD"]) # clear plots
# it's all about the benjamins
plotTable(forPres)


# what do you mean the school matters?
(EarningsPlot = ggplot(earningsMelt, aes(Ownership, value, fill=variable)) + geom_bar(stat = 'identity', position = 'dodge') + 
    ggtitle("Earnings After College") + xlab(label = '') + ylab(label = 'Income') +
    scale_fill_manual(values=c("#999999", "#E69F00"), name="Legend", labels=c('After 6 Years','After 10 Years')))


# change is good
plotTable(fin.data[,-5])
(rateChangePlot = ggplot(log.data, aes(x= pub_years)) + 
    geom_line(aes(y = pub_log), color = 'darkgreen') + 
    geom_line(aes(y = pfnp_log), color = 'darkorange') + 
    geom_line(aes(y = pfp_log), color = 'darkred') + xlab(label = 'Years') + ylab(label = 'Rate of Change'))

# success
plotTable(fin.data[,c(1,5)])
plotTable(fin.data[,-2])
(predEarningsPlot = ggplot(test, aes(Ownership, value, fill=variable)) + geom_bar(stat = 'identity', position = 'dodge') + 
    ggtitle("Earnings After College") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Legend", labels=c('After 6 Years','After 10 Years','After ~20 Years')))



(nb.matrix = confusionMatrix(nb.table))
(nb.matrix1 = confusionMatrix(table(nb.pred1, test.data$ownership))) 

(svm.matrix = confusionMatrix(svm.table))
(svm.matrix1 = confusionMatrix(table(svm.pred1, test.data$ownership)))

draw_confusion_matrix(rf.matrix, 'Private nonprofit', 'Public', 'Random Forest')
(rf.matrix = confusionMatrix(rf.table))

#------------------------------------------------------------------------------------------------------------------------








write.csv(fin.data, file = 'finplot_data.csv', row.names = F)
write.csv(forPres, file = 'incomeplot_data.csv', row.names = F)
