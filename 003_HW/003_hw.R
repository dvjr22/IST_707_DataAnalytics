# Diego Valdes
# IST 707
# Apr 14, 2019
# HW 3

# =============================================================================================================
# RESET WORKSPACE
# =============================================================================================================

rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

# =============================================================================================================
# LIBRARIES
# =============================================================================================================

#library(tidyr)
#library(reshape)
library(gridExtra)
library(grid)
library(ggplot2)
#library(lattice)
library(arules)
library(arulesViz)
library(datasets)

originalWD = getwd() # get original wd for saving to .r file
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_707/HW/003_HW")

# =============================================================================================================
# IMPORT AND CLEAN DATA
# =============================================================================================================

bankData_og = read.csv2("bankdata_csv_all.csv", sep = ',', stringsAsFactors = FALSE)
naInData(bankData_og)# no NA

bank_data = bankData_og[,-1] # remove id col
str(bank_data) # to factor: 2,3:11
colToFactor = c(2,3,5:11)
colToNum = c(4)

colNames = colnames(bank_data)
colNames

colNames[c(2,3,5:11)]
colNames[4]

bank_data = toFactor(bank_data, colToFactor) # change defined cols to factors
bank_data = toNumeric(bank_data, colToNum) # change defined cols to numeric
str(bank_data)

# adding cols for factorizing income and age
summary(bank_data$income)
bank_data$incomeF = cut(bank_data$income, breaks = c(0,10000,20000,30000,40000,50000,60000,Inf), 
                        labels = c("ZERO","TEN K","TWENTY K","THIRTY K","FOURTY K","FIFTY K","SIXTY K"))

summary(bank_data$age)
bank_data$ageF = cut(bank_data$age, breaks = c(0,10,20,30,40,50,60,Inf), 
                     labels = c("child","teens","twenties","thirties","fourties","fifties","sixties"))

# adding col for int of children
bank_data$childrenInt=as.integer(bank_data$children) 
summary(bank_data)
str(bank_data)

summary(bank_data$income)
summary(bank_data[bank_data$sex=='FEMALE',]$income)
summary(bank_data[bank_data$sex=='MALE',]$income)

# =============================================================================================================
# TO EXPLORE STRANGE NEW DATASETS, TO SEEK OUT NEW ANSWERS TO OLD SOLUTIONS,  
# TO BOLDY DISCOVER WHAT NO ONE HAS DISCOVERD BEFORE!
# =============================================================================================================


regionsPlot = ggplot(bank_data, aes(region)) + geom_bar(aes(fill = sex))+ scale_fill_manual( values = c('red', 'blue')) + ggtitle("Gender by Region") # 4 regions
#incomeRegionsPlot = ggplot(bank_data, aes(region, income, fill = region)) + geom_boxplot() # income by region
incomeRegionsPlot = ggplot(bank_data, aes(incomeF)) + geom_bar(aes(fill = region)) + ggtitle("Income by Region")
grid.arrange(regionsPlot, incomeRegionsPlot, ncol = 2)

sexPlot = ggplot(bank_data, aes(sex)) + geom_bar(aes(fill = region)) # equal sex count
incomeSexPlot = ggplot(bank_data, aes(sex, income, fill = region)) + geom_boxplot() + ggtitle("Income by Sex/Region")  # income by sex
grid.arrange(sexPlot, incomeSexPlot)

marriedPlot = ggplot(bank_data, aes(married)) + geom_bar(aes(fill = sex)) + scale_fill_manual( values = c('red', 'blue')) + ggtitle("Married by Gender")# more are married
marriedPlot01 = ggplot(bank_data, aes(married)) + geom_bar(aes(fill = region)) + theme(legend.position = "none") + ggtitle("Married by Region")# more are married
childrenPlot = ggplot(bank_data[bank_data$married=='YES',], aes(children)) + geom_bar(aes(fill = region)) + ggtitle("Married with Children")
grid.arrange(marriedPlot, marriedPlot01, childrenPlot, ncol = 3)

carPlot = ggplot(bank_data, aes(car)) + geom_bar()

savePlot = ggplot(bank_data, aes(save_act)) + geom_bar(aes(fill = region)) + theme(legend.position = "none") + ylim(0,500) + ggtitle("Savings Accounts")
currentPlot = ggplot(bank_data, aes(current_act)) + geom_bar(aes(fill = region)) + theme(legend.position = "none") + ylim(0,500) + ggtitle("Current Accounts")
mortagePlot = ggplot(bank_data, aes(mortgage)) + geom_bar(aes(fill = region)) + theme(legend.position = "none") + ylim(0,500) + ggtitle("Mortgages")
pepPlot = ggplot(bank_data, aes(pep)) + geom_bar(aes(fill = region))  + ylim(0,500) + ggtitle("PEP")
legend = getLegend(pepPlot)
pepPlot = pepPlot+ theme(legend.position = "none")
grid.arrange(savePlot, currentPlot, mortagePlot, pepPlot, legend, ncol=5)

#colorPalette = c("FEMALE" = "red", "MALE" = "blue")
xy_plot01 = ggplot(bank_data, aes(age, income)) + geom_point(aes(color = region, shape = pep, size = 1)) + scale_fill_manual( values = c('red', 'blue')) + ggtitle("Income vs Age")
xy_plot02 = ggplot(bank_data, aes(age, income)) + geom_point(aes(color = region , shape = pep )) + scale_fill_manual( values = c('yellow', 'green'))

grid.arrange(xy_plot01, xy_plot02)

ggplot(bank_data, aes(incomeF)) + geom_bar(aes(fill = region))



# =============================================================================================================
# Rule mining
# =============================================================================================================

bank_data_rules = bank_data[, c(-1,-4,-14)] # remove cols age and income
str(bank_data_rules)

# Get the rules
rules01 = apriori(bank_data_rules, parameter = list(supp = 0.15, conf = 0.8))
length(rules01) # 19

# top 5 rules by confidence
options(digits=2)
rules01 = sort(rules01, by="confidence", decreasing=TRUE)
inspect(rules01)

# top 5 rules by lift
rules01 = sort(rules01, by="lift", decreasing=TRUE)
inspect(rules01)

# top 5 rules by support
rules01 = sort(rules01, by="support", decreasing=TRUE)
inspect(rules01[1:10])

rules01 = sort(rules01, by="count", decreasing=TRUE)
inspect(rules01[1:10])

# set pep to rhs
rules02 = apriori(data = bank_data_rules, parameter=list(supp=0.04, conf = .9), 
               appearance = list(default="lhs",rhs="pep=YES"),
               control = list(verbose=FALSE, sort=TRUE))
length(rules02) #1,434
summary(rules02)

rules02 = sort(rules02, by="confidence", decreasing=TRUE)
inspect(rules02[1:10])

rules02 = sort(rules02, by="lift", decreasing=TRUE)
inspect(rules02)

rules02 = sort(rules02, by="support", decreasing=TRUE)
inspect(rules02[1:10])

# =============================================================================================================
# INTRESTING RULES
# =============================================================================================================

#                                                       support       confidence    lift        count
# {sex=FEMALE,region=INNER_CITY} => {current_act=YES}   0.1750000     0.8015267     1.056958    105  
# {children=1,save_act=YES,ageF=fourties} => {pep=YES}  0.04333333    1             2.189781    26 
# {married=YES,children=1,incomeF=TWENTY K}=> {pep=YES} 0.04666667    1             2.189781    28   


ggplot(bank_data_rules[bank_data_rules$sex=='FEMALE',], aes(region)) + geom_bar(aes(fill = current_act)) # equal sex count
ggplot(bank_data_rules, aes(ageF)) + geom_bar(aes(fill = save_act)) 
ggplot(bank_data_rules, aes(save_act)) + geom_bar(aes(fill = ageF)) 

# =============================================================================================================

plot(rules01, method='scatterplot', engine = 'interactive', shading = 'lift')
plot(rules02, method='scatterplot', engine = 'interactive', shading = 'lift')


# explain the rule
# why it is interesting
# what that means to the bank

# customers with 1 kid more likely to have a pep
#{children=1}                                      => {pep=YES}         0.18    0.81       1.8  110  
kidPlot = ggplot(bank_data, aes(pep)) + geom_bar(aes(fill=children)) + ggtitle("PEP Accounts with Children")

# children are a factor in pep, but so is income
#{children=1,incomeF=THIRTY K}                                     => {pep=YES} 0.043   0.96       2.1  26 
#{children=1,incomeF=TWENTY K}                                     => {pep=YES} 0.062   0.95       2.1  37 
incomePlot = ggplot(bank_data[bank_data$children==1,], aes(incomeF)) +  geom_bar(aes(fill = pep)) +ggtitle("PEP Accounts by Age with 1 Child")

grid.arrange(kidPlot, incomePlot, ncol=2)

# these are target customers
#{married=YES,children=1,save_act=YES,current_act=YES}             => {pep=YES} 0.073   0.92       2.0  44
# These are future potential pep customers
#{married=YES,children=0,save_act=YES}             => {pep=NO}          0.18    0.90       1.7  107 
ggplot(bank_data[bank_data$married=='YES' & bank_data$save_act=='YES',], aes(children)) +  geom_bar(aes(fill = pep)) + ggtitle("Current Customers with Children")


#{car=NO,save_act=YES,mortgage=NO}                 => {current_act=YES} 0.17    0.81       1.1  104 
ggplot(bank_data[bank_data$car=='NO' & bank_data$mortgage=='NO',], aes(incomeF)) + geom_bar(aes(fill=save_act))+ggtitle("Customers for Future Products")


