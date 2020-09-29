#plots
dev.off(dev.list()["RStudioGD"]) # clear plots
# it's all about the benjamins
plotTable(forPres)

(loanPrincipalPlot = mapPlot + geom_map(map = us, aes(fill = Loan.Principal), color = "black") +
    expand_limits(x = us$long, y = us$lat) + 
    coord_map() + ggtitle("Avg College Dept") + scale_fill_gradient(low='white', high='darkblue', name="School Debt") +
    xlab(label = '') + ylab(label = ''))

# free money
(debtPlot = ggplot(deptMelt, aes(Ownership, value, fill=variable)) + geom_bar(stat = 'identity', position = 'dodge') + 
    xlab(label = '') + ylab(label = 'Income') + ggtitle("Student Dept") + 
    scale_fill_manual(values=c("#999999", "#E69F00"), name="Legend", labels=c('Pell Grant','No Pell Grant')) )

# show me the money
(x10MedianIncomePlot = mapPlot + geom_map(map = us, aes(fill = x10.Earnings.mean), color = "black") +
    expand_limits(x = us$long, y = us$lat) + 
    coord_map() + ggtitle("Average Earnings 10 Yrs") + scale_fill_gradient(low='white', high='darkblue', name="Legend") + 
    xlab(label = '') + ylab(label = ''))


(expPlot = mapPlot + geom_map(map = us, aes(fill = ration), color = "black") +
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + ggtitle("College Dept") + scale_fill_gradient(low='white', high='darkblue', name="Legend") + 
  xlab(label = '') + ylab(label = ''))


# box plots, and outliers, and quartiles, oh my!
(debtPlotBox = ggplot(box.data.melt, aes(ownership, value, fill=variable)) + geom_boxplot() + 
    ggtitle("Average Earnings") + scale_fill_manual(values=c("#999999", "#E69F00"), name="Legend", labels=c('6 Years','10 Years')) +
    xlab(label = '') + ylab(label = 'Income') )

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
draw_confusion_matrix(nb.matrix1, 'Private nonprofit', 'Public', 'Naive Bayes')

(svm.matrix = confusionMatrix(svm.table))
(svm.matrix1 = confusionMatrix(table(svm.pred1, test.data$ownership)))
draw_confusion_matrix(svm.matrix1, 'Private nonprofit', 'Public', 'Support Vector Machines')

draw_confusion_matrix(rf.matrix, 'Private nonprofit', 'Public', 'Random Forest')
(rf.matrix = confusionMatrix(rf.table))

#------------------------------------------------------------------------------------------------------------------------








write.csv(fin.data, file = 'finplot_data.csv', row.names = F)
write.csv(forPres, file = 'incomeplot_data.csv', row.names = F)
