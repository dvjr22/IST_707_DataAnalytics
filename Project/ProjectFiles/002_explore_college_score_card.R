# Diego Valdes
# IST 707
# May 11, 2019
# Project

# What relationship does Financial Aid have with student success before, during and after college?
# I got 'after'

# =============================================================================================================
# RESET WORKSPACE
# =============================================================================================================

rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

# =============================================================================================================
# LIBRARIES
# =============================================================================================================

loadLibraries()

(originalWD = getwd()) # get original wd for saving to .r file
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_707/Project")

# =============================================================================================================
# FUNCTIONS
# =============================================================================================================


# =============================================================================================================
# IMPORT AND CLEAN DATA
# =============================================================================================================

C.SCORE.CARD.DATA.OG = read.csv2("collegescorecard-3.csv", sep = ',', stringsAsFactors = FALSE, numerals = 'no.loss')

data.csc = C.SCORE.CARD.DATA.OG
#summary(data.csc)
#str(data.csc)
#colnames(data.csc)

#naInData(data.csc)
data.csc = toFactor(data.csc, c(1:5))
#summary(data.csc[,1:5])

#str(data.csc[,6:12])
data.csc = toInteger(data.csc, c(7:12)) # nulls turned to NA
data.csc = toNumeric(data.csc, c(6))
#str(data.csc[,6:12])

#str(data.csc[,12:20])
data.csc = toNumeric(data.csc, c(17:20))

#str(data.csc[,21:30])
data.csc = toNumeric(data.csc, c(21:30))
#str(data.csc$loan_principal)

#str(data.csc[,31:39])
data.csc = toNumeric(data.csc, c(31:39))

str(data.csc)
naInData(data.csc)

sat.writing.data = data.csc$sat_scores.midpoint.writing
data.csc = data.csc[,-9]
data.csc = noNas(data.csc)

naInData(data.csc)

colnames(data.csc[,1:10])


# =============================================================================================================
# MAGGIE'S CLUSTER DATA

#scorecard <- read.csv("scorecard-3.csv", stringsAsFactors = FALSE)
scorecard = C.SCORE.CARD.DATA.OG

# There are 2 "emmanuel colleges", so I added theie states to them so that we can use name as a unique identifier
scorecard$name.id <- ifelse(scorecard$name == "Emmanuel College" & scorecard$state=="GA", "Emmanuel College (GA)", 
                            ifelse(scorecard$name == "Emmanuel College" & scorecard$state == "MA", "Emmanuel College (MA)", scorecard$name))


scorecard = toFactor(scorecard, c(1:5))
#summary(data.csc[,1:5])

#str(data.csc[,6:12])
scorecard = toInteger(scorecard, c(7:12)) # nulls turned to NA
scorecard = toNumeric(scorecard, c(6))
#str(data.csc[,6:12])

#str(data.csc[,12:20])
scorecard = toNumeric(scorecard, c(17:20))

#str(data.csc[,21:30])
scorecard = toNumeric(scorecard, c(21:30))
#str(data.csc$loan_principal)

#str(data.csc[,31:39])
scorecard = toNumeric(scorecard, c(31:39))

str(scorecard)
naInData(scorecard)

#sat.writing.data = data.csc$sat_scores.midpoint.writing
scorecard = scorecard[,-9]
scorecard = noNas(scorecard)

naInData(scorecard)

colnames(scorecard[,1:10])

# read in cluster assignments
clusters <- read.csv("during.clusters.csv")

# join to scorecard dataset
library(dplyr)
scorecard <- scorecard %>%
  left_join(clusters, by = c("name.id"="name"))
str(scorecard)
scorecard$during.cluster <- factor(scorecard$during.cluster)


# if you want to remove the name.id column:
scorecard <- scorecard %>% select(-name.id)
str(scorecard)

test.revalue = revalue(scorecard$during.cluster, c('1'='Entry Level', '2'='Stable', '3'='Reachers', '4'='Privileged'))
scorecard$during.cluster = test.revalue

# =============================================================================================================
# TO EXPLORE STRANGE NEW DATASETS, TO SEEK OUT NEW ANSWERS TO OLD SOLUTIONS,  
# TO BOLDY DISCOVER WHAT NO ONE HAS DISCOVERD BEFORE!
# =============================================================================================================

ggplot(data.csc, aes(ownership, X10_yrs_after_entry.median)) + geom_dotplot(binaxis = 'y', stackdir = 'center')
ggplot(data.csc, aes(ownership, X10_yrs_after_entry.median)) + geom_boxplot()

colnames(data.csc)
box.data = data.csc[,c(3,37,33)]
box.data.melt = melt(box.data)

debtPlotBox = ggplot(box.data.melt, aes(ownership, value, fill=variable)) + geom_boxplot() + 
  ggtitle("Average Earnings") + scale_fill_manual(values=c("#999999", "#E69F00"), name="Legend", labels=c('6 Years','10 Years')) 

ggplot(data.csc, aes(ownership, X10_yrs_after_entry.median)) + geom_bar(stat = 'identity')


(byOwnership = aggregate(x=cbind(AdminRate = data.csc$admission_rate.overall, 
                                 SAT.Reading = data.csc$sat_scores.midpoint.critical_reading, 
                                 SAT.Math = data.csc$sat_scores.midpoint.math,
                                 Avg.Price = data.csc$avg_net_price,
                                 Tuition.InState = data.csc$tuition.in_state,
                                 Tuition.OutState = data.csc$tuition.out_of_state,
                                 Yearly.Attendance = data.csc$attendance.academic_year,
                                 Retention = data.csc$retention_rate.four_year.full_time,
                                 Completion = data.csc$completion_rate_4yr_150nt,
                                 Pell.Grant = data.csc$pell_grant_rate, # 
                                 Fed.Loan = data.csc$federal_loan_rate, # 
                                 Loan.Principal = data.csc$loan_principal, # 
                                 PellGrant.Debt = data.csc$median_debt.pell_grant,
                                 NoPellGrant.Debt = data.csc$median_debt.no_pell_grant,
                                 x6.Earnings.mean = data.csc$X6_yrs_after_entry.working_not_enrolled.mean_earnings,
                                 x6.Earnings.median = data.csc$X6_yrs_after_entry.median,
                                 x10.Earnings.mean = data.csc$X10_yrs_after_entry.working_not_enrolled.mean_earnings,
                                 x10.Earnins.median = data.csc$X10_yrs_after_entry.median),
                         by = list(Ownership = data.csc$ownership),
                         FUN = mean))



ggplot(byOwnership, aes(Ownership, x10.Earnins.median)) + geom_bar(stat = 'identity')
ggplot(byOwnership, aes(Ownership, Loan.Principal)) + geom_bar(stat = 'identity')

colnames(data.csc[,22:24])

# 2018 U.S. Census Bureau
# HS - $35,256
# some college - $38,376
# AA - $41,496
# BA/S - $59,124
# MS - $89,960
# PHd - $84,396

Education = c('High School', 'Some College', 'Associates', 'Bachelors', 'Masters', 'PHd')
Income = c('$35,256','$38,376', '$41,496', '$59,124', '$89,960', '$84,396')
forPres = as.data.frame(cbind(Education, Income))
plotTable(forPres)

meanIncomeCollegeUS = signif(mean(c(rep(41496, 65),rep(59124,35),rep(89906, 9),84396)), 7)
meanIncomeCollegex10 = signif(mean(data.csc$X10_yrs_after_entry.working_not_enrolled.mean_earnings), 7)
meanIncomeCollegex6 = signif(mean(data.csc$X6_yrs_after_entry.working_not_enrolled.mean_earnings), 7)


(byState = aggregate(x=cbind(AdminRate = data.csc$admission_rate.overall, 
# (byState.Mode = aggregate(x=cbind(AdminRate = data.csc$admission_rate.overall,                             
                                 SAT.Reading = data.csc$sat_scores.midpoint.critical_reading, 
                                 SAT.Math = data.csc$sat_scores.midpoint.math,
                                 Avg.Price = data.csc$avg_net_price,
                                 Tuition.InState = data.csc$tuition.in_state,
                                 Tuition.OutState = data.csc$tuition.out_of_state,
                                 Yearly.Attendance = data.csc$attendance.academic_year,
                                 Retention = data.csc$retention_rate.four_year.full_time,
                                 Completion = data.csc$completion_rate_4yr_150nt,
                                 Pell.Grant = data.csc$pell_grant_rate,
                                 Fed.Loan = data.csc$federal_loan_rate,
                                 Loan.Principal = data.csc$loan_principal,
                                 PellGrant.Debt = data.csc$median_debt.pell_grant,
                                 NoPellGrant.Debt = data.csc$median_debt.no_pell_grant,
                                 x6.Earnings.mean = data.csc$X6_yrs_after_entry.working_not_enrolled.mean_earnings,
                                 x6.Earnings.median = data.csc$X6_yrs_after_entry.median,
                                 x10.Earnings.mean = data.csc$X10_yrs_after_entry.working_not_enrolled.mean_earnings,
                                 x10.Earnins.median = data.csc$X10_yrs_after_entry.median),
                         by = list(State = data.csc$state),
                         FUN = mean)) 
                        # FUN = Mode)



byState$ration = byState[,13]/ byState[,18]

highRatio = order(byState$ration)

byState[highRatio, c(1,20,21) ]

us = map_data("state") # get map data
byState$StateName = tolower(abbr2state(byState$State))

mapPlot = ggplot(byState, aes(map_id = StateName)) 


x10MedianIncomePlot = mapPlot + geom_map(map = us, aes(fill = x10.Earnings.mean), color = "black") +
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + ggtitle("Average Earnings") + scale_fill_gradient(low='white', high='darkblue', name="Income\n10 Years") 

loanPrincipalPlot = mapPlot + geom_map(map = us, aes(fill = Loan.Principal), color = "black") +
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + ggtitle("Avg College Dept") + scale_fill_gradient(low='white', high='darkblue', name="School Debt") 

expPlot = mapPlot + geom_map(map = us, aes(fill = ration), color = "black") +
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + ggtitle("Avg College Dept") + scale_fill_gradient(low='white', high='darkblue', name="School Debt") 

(byCarnegie = aggregate(x=cbind(AdminRate = data.csc$admission_rate.overall, 
                             Avg.Price = data.csc$avg_net_price,
                             Tuition.InState = data.csc$tuition.in_state,
                             Tuition.OutState = data.csc$tuition.out_of_state,
                             Pell.Grant = data.csc$pell_grant_rate,
                             Fed.Loan = data.csc$federal_loan_rate,
                             Loan.Principal = data.csc$loan_principal,
                             PellGrant.Debt = data.csc$median_debt.pell_grant,
                             NoPellGrant.Debt = data.csc$median_debt.no_pell_grant,
                             x6.Earnings.mean = data.csc$X6_yrs_after_entry.working_not_enrolled.mean_earnings,
                             x6.Earnings.median = data.csc$X6_yrs_after_entry.median,
                             x10.Earnings.mean = data.csc$X10_yrs_after_entry.working_not_enrolled.mean_earnings,
                             x10.Earnins.median = data.csc$X10_yrs_after_entry.median),
                     by = list(Car.Ugrd = data.csc$carnegie.ugrd, Ownership = data.csc$ownership),
                     FUN = mean)) 
# FUN = Mode)


########################################################################################################

(Financial = aggregate(x=cbind(
                                 SAT.Reading = data.csc$sat_scores.midpoint.critical_reading, 
                                 SAT.Math = data.csc$sat_scores.midpoint.math,
                                 Loan.Principal = data.csc$loan_principal, # 
                                 PellGrant.Debt = data.csc$median_debt.pell_grant,
                                 NoPellGrant.Debt = data.csc$median_debt.no_pell_grant,
                                 x6.Earnings.mean = data.csc$X6_yrs_after_entry.working_not_enrolled.mean_earnings,
                                 x6.Earnings.median = data.csc$X6_yrs_after_entry.median,
                                 x10.Earnings.mean = data.csc$X10_yrs_after_entry.working_not_enrolled.mean_earnings,
                                 x10.Earnins.median = data.csc$X10_yrs_after_entry.median),
                         by = list(Ownership = data.csc$ownership),
                         FUN = mean))


dept = Financial[,c(1,5,6)]
deptMelt = melt(dept, id.vars = 'Ownership')
debtPlot = ggplot(deptMelt, aes(Ownership, value, fill=variable)) + geom_bar(stat = 'identity', position = 'dodge') + 
  ggtitle("Student Dept") + scale_fill_manual(values=c("#999999", "#E69F00"), name="Legend", labels=c('Pell Grant','No Pell Grant')) 



levels(deptMelt$Ownership)


earnings = Financial[,c(1,7,9)]
earningsMelt = melt(earnings, id.vars = 'Ownership')
EarningsPlot = ggplot(earningsMelt, aes(Ownership, value, fill=variable)) + geom_bar(stat = 'identity', position = 'dodge') + 
  ggtitle("Earnings After College") + scale_fill_manual(values=c("#999999", "#E69F00"), name="Legend", labels=c('After 6 Years','After 10 Years')) 

colnames(data.csc)
Fin.Filtered = data.csc[,c(1:3,7:8,14:15,22:24,37,38,33,34)]



# =============================================================================================================
# MODELS
# =============================================================================================================

model.data = data.csc[,c(3,2,7,8,22,37,38,33,34)]
str(model.data)

md = model.data[model.data$ownership != "Private for-profit",] # removing private for profit
md$ownership = factor(md$ownership)
str(md$ownership)

# randomize and create trng and test sets
# write them for future import to keep results consistant
# model.data.list = testingSample(md)
# trng.data = model.data.list[[1]]
# test.data = model.data.list[[2]]

# write.csv(trng.data, file = 'trng_data.csv', row.names = F)
# write.csv(test.data, file = 'test_data.csv', row.names = F)
# paste('trng_data_','001', sep = '')


trng.data = read.csv('trng_data.csv')
test.data = read.csv('test_data.csv')
str(trng.data)
str(test.data)

# trng.data = trng.data[,-2]
# test.data = test.data[,-2]

# =============================================================================================================
# NAIVE BAYES
# =============================================================================================================

nb.model = naiveBayes(trng.data$ownership ~. , data = trng.data, laplace = 1, na.action = na.pass)
# summary(nb.model)
nb.pred = predict(nb.model, test.data[,-1], type = 'class')
nb.table = table(nb.pred, test.data$ownership)
(nb.matrix = confusionMatrix(nb.table))

# =============================================================================================================
# removing the state variable

nb.model1 = naiveBayes(trng.data$ownership ~. , data = trng.data[,-2], laplace = 1, na.action = na.pass)
# summary(nb.model)
nb.pred1 = predict(nb.model1, test.data[,-1,-2], type = 'class')
(nb.matrix1 = confusionMatrix(table(nb.pred1, test.data$ownership)))

# =============================================================================================================
# SVM
# =============================================================================================================

svm.model = svm(ownership ~. , data = trng.data[,-2], scale = F, cost = 1, type = 'C'
                 # , kernel = 'polynomial'
                  , kernel = 'linear'
                 # , kernel = 'radial'
                # , kernel = 'sigmoid'
)

# summary(svm.model)
svm.pred = predict(svm.model, test.data[,-1], type = 'class')
svm.table = table(svm.pred, test.data$ownership)
# length(svm.pred)
# length(test.data$ownership)
(svm.matrix = confusionMatrix(svm.table))

# =============================================================================================================
# removing the state variable

svm.model1 = svm(ownership ~. , data = trng.data[,-2], scale = F, cost = 1, type = 'C'
                 # , kernel = 'polynomial'
                 , kernel = 'linear'
                 # , kernel = 'radial'
                 # , kernel = 'sigmoid'
)
svm.pred1 = predict(svm.model1, test.data[,-1,-2], type = 'class')
(svm.matrix1 = confusionMatrix(table(svm.pred1, test.data$ownership)))


# =============================================================================================================
# RANDOM FOREST
# =============================================================================================================

rf.model = randomForest(ownership ~., trng.data[,-2])
# summary(rf.model)
# importance(rf.model)
rf.pred = predict(rf.model, test.data[,-2], type='class')
rf.table = table(rf.pred, test.data$ownership )
(rf.matrix = confusionMatrix(rf.table))


# =============================================================================================================
# LINEAR
# =============================================================================================================

colnames(md)
lm.data = model.data[,c(-2,-7,-9)]
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
                       by = list(Ownership=lm.model.data$ownership),
                       FUN = mean))


test = melt(pred.data)
years = c(6,6,6,10,10,10,18,18,18)
test$variable = as.factor(years)
levels(test$variable)

test$log = log(test$value)
test$years = years
levels(test$Ownership)

predEarningsPlot = ggplot(test, aes(Ownership, value, fill=variable)) + geom_bar(stat = 'identity', position = 'dodge') + 
  ggtitle("Earnings After College") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), name="Legend", labels=c('After 6 Years','After 10 Years','After ~20 Years')) 

ggplot(test, aes(Ownership, log, fill=variable)) + geom_bar(stat = 'identity', position = 'dodge')

pfnp = test[test$Ownership == "Private nonprofit",]
pfp = test[test$Ownership == "Private for-profit",]
pub = test[test$Ownership == "Public" ,]

pfnp = pfnp[,c(3,5)]
colnames(pfnp) = c('pfnp_earnings','pfnp_years')
pfnp$pfnp_log = log(pfnp$pfnp_earnings)

pfp = pfp[,c(3,5)]
colnames(pfp) = c('pfp_earnings','pfp_years')
pfp$pfp_log = log(pfp$pfp_earnings)

pub = pub[,c(3,5)]
colnames(pub) = c('pub_earnings','pub_years')
pub$pub_log = log(pub$pub_earnings)

log.data = cbind(pfp,pfnp,pub)


(rateChangePlot = ggplot(log.data, aes(x= pub_years)) + 
  geom_line(aes(y = pub_log), color = 'darkgreen') + 
  geom_line(aes(y = pfnp_log), color = 'darkorange') + 
  geom_line(aes(y = pfp_log), color = 'darkred') + xlab(label = 'Years') + ylab(label = 'Rate of Change'))
  

fin.data = Financial[,c(1,4,7,9)]
fin.data[2,2] - fin.data[3,2] # difference in loan amount
fin.data[2,3] - fin.data[3,3] # earnings after 6 years
fin.data[2,4] - fin.data[3,4] # earnings after 10 years
fin.data[2,5] - fin.data[3,5] # projected earnings 20 years
fin.data$projection = c(54374.91, 72951.13, 66502.71)

v = c(fin.data[2,3] - fin.data[3,3] ,fin.data[2,4] - fin.data[3,4],fin.data[2,5] - fin.data[3,5])

vl = log(v)
log.data$vl = vl

x + geom_line(aes(y=log.data$vl), color = 'darkblue')  

log.data$vl= log.data$vl+1
x + geom_line(aes(y=vl), color = 'darkblue')  


colnames(fin.data) = c('Ownership','Loan Principal','Earnings 6yrs', 'Earnings 10yrs','Earnings ~20yrs')

plotTable(fin.data)




