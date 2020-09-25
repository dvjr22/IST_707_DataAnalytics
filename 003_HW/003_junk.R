rules = apriori(bank_data_rules, parameter = list(supp = 0.001, conf = 0.8))
length(rules) # 1,183,020 | 5 - 55,492

rules01 = sort(rules01, by=c("lift", "support", "confidence"), decreasing=TRUE)
inspect(rules01[1:10])

bank_cust = bank_data[bank_data$current_act == 'YES',]

savePlot2 = ggplot(bank_cust, aes(save_act)) + geom_bar(aes(fill = region)) + theme(legend.position = "none")
mortagePlot2 = ggplot(bank_cust, aes(mortgage)) + geom_bar(aes(fill = region)) + theme(legend.position = "none")
pepPlot2 = ggplot(bank_cust, aes(pep)) + geom_bar(aes(fill = region)) 

grid.arrange(savePlot2, mortagePlot2, pepPlot2, ncol=3)

plot(rules01, method='matrix', engine = 'htmlwidget', shading = 'confidence')
plot(rules02[1:1000], method='matrix', engine = 'htmlwidget', shading = 'confidence')

rules01 = apriori(bank_data_rules, parameter = list(supp = 0.01, conf = 1))
length(rules01) 

# {children=1} => {pep=YES}                             0.1833333     0.8148148     1.784266    110
# {married=YES,children=0,save_act=YES} => {pep=NO}     0.1783333     0.8991597     1.654895    107 
ggplot(bank_data, aes(children)) +  geom_bar(aes(fill = pep))
ggplot(bank_data[bank_data$married=='YES',], aes(children)) +  geom_bar(aes(fill = pep))
ggplot(bank_data[bank_data$married=='YES' & bank_data$save_act=='YES',], aes(children)) +  geom_bar(aes(fill = pep))



# {children=1,save_act=YES,ageF=fourties} => {pep=YES}  0.04333333    1             2.189781    26 
ggplot(bank_data[bank_data$childrenInt == 1 & bank_data$save_act=='YES',], aes(ageF) ) + geom_bar(aes(fill = pep))

# {married=YES,children=1,incomeF=TWENTY K}=> {pep=YES} 0.04666667    1             2.189781    28   
ggplot(bank_data[bank_data$married=='YES' & bank_data$save_act=='YES' & bank_data$childrenInt == 1,], aes(incomeF) ) + geom_bar(aes(fill = pep))

# leads to...
ggplot(bank_data[bank_data$married=='YES' & bank_data$save_act=='YES' & bank_data$childrenInt == 2,], aes(ageF) ) + geom_bar(aes(fill = pep))
#ggplot(bank_data[bank_data$married=='YES' & bank_data$save_act=='YES' & bank_data$childrenInt == 2 & bank_data$pep=='YES',], aes(ageF) ) + geom_bar(aes(fill = incomeF))

