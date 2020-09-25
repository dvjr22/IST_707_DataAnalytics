# Diego Valdes
# IST 707
# Apr 6, 2019
# HW 2

# librarys
library(tidyr)
library(reshape)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)


rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

originalWD = getwd() # get original wd for saving to .r file
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_707/HW/002_HW")





# =============================================================================================================
# IMPORT AND CLEAN DATA
# =============================================================================================================

# import data and take a look
schools = read.csv("002_hw_data_storyteller.csv", stringsAsFactors = FALSE) 
colsNames = colnames(schools)
colsNames

# change the col names
colnames(schools) = c("School", "Section", "VeryAhead", "Middling", "Behind", "MoreBehind", "VeryBehind", "Completed")
summary(schools)
str(schools)

# change data types for cols School and Section
schools$School = as.factor(schools$School)
schools$Section = as.factor(schools$Section)
summary(schools)
str(schools)

# check for NA/NaNs
sum(is.na(schools))

# =============================================================================================================
# TO EXPLORE STRANGE NEW DATASETS, TO SEEK OUT NEW ANSWERS TO OLD SOLUTIONS,  
# TO BOLDY DISCOVER WHAT NO ONE HAS DISCOVERD BEFORE!
# =============================================================================================================

# Table to take a quick look
table(schools$School) # school variation leans heavy toward A and B
table(schools$Section) # The division of sections seems a little better

a = grid.table(schools)

# set a color palette
colorPalette = c("A" = "red", "B" = "blue", "C" = "green", "D" = "yellow", "E" = "orange")

# expore with box plots
A = ggplot(schools, aes(School, Middling)) + geom_boxplot(fill = colorPalette)
B = ggplot(schools, aes(School, Behind)) + geom_boxplot(fill = colorPalette)
C = ggplot(schools, aes(School, MoreBehind)) + geom_boxplot(fill = colorPalette)
D = ggplot(schools, aes(School, VeryBehind)) + geom_boxplot(fill = colorPalette)
E = ggplot(schools, aes(School, Completed)) + geom_boxplot(fill = colorPalette)

gh =  textGrob("Performance Box Plots")
grid.arrange(gh,A,B,C,D,E, ncol=3) 

A = ggplot(bySchools,aes(School, Middling)) + geom_bar(stat = 'identity', fill = colorPalette) + ggtitle("Middling")
B = ggplot(bySchools,aes(School, Behind)) + geom_bar(stat = 'identity', fill = colorPalette)  + ggtitle("Behind")
c = ggplot(bySchools,aes(School, MoreBehind)) + geom_bar(stat = 'identity', fill = colorPalette)  + ggtitle("MoreBehind")
D = ggplot(bySchools,aes(School, VeryBehind)) + geom_bar(stat = 'identity', fill = colorPalette)  + ggtitle("VeryBehind")
E = ggplot(bySchools,aes(School, Completed)) + geom_bar(stat = 'identity', fill = colorPalette)  + ggtitle("Completed")

grid.arrange(A,B,C,D,E, ncol = 3) 

# data frame for number of sections
a = nrow(schools[schools$School == 'A',])
b = nrow(schools[schools$School == 'B',])
c = nrow(schools[schools$School == 'C',])
d = nrow(schools[schools$School == 'D',])
e = nrow(schools[schools$School == 'E',])
Sections = c(a,b,c,d,e)
School = as.factor(bySchools$School)
cbind
SecCount = data.frame(cbind(School,Sections))
SecCount$School = as.factor(bySchools$School)

grid.table(SecCount)

# nope!
plot(schools$Middling, schools$Behind)
plot(schools$Middling, schools$MoreBehind)
plot(schools$Middling, schools$VeryBehind)
plot(schools$Middling, schools$Completed)

Z = ggplot(scaledSchool, aes(Middling, Behind)) + geom_point()
X = ggplot(scaledSchool, aes(Middling, MoreBehind)) + geom_point()
Y = ggplot(scaledSchool, aes(Middling, VeryBehind)) + geom_point()
W = ggplot(scaledSchool, aes(Middling, Completed)) + geom_point()

grid.arrange(Z, X, Y, W, ncol = 2) 

# aggregate by schools
bySchools = aggregate(x = cbind(VA = schools$VeryAhead, M =  schools$Middling, B = schools$Behind, 
                                MB = schools$MoreBehind, VB = schools$VeryBehind, C = schools$Completed ),
                      by = list(School = schools$School),
                      FUN = sum)

grid.table(bySchools)

colnames(bySchools) = c("School", "VeryAhead", "Middling", "Behind", "MoreBehind", "VeryBehind", "Completed")
bySchools$Total = rowSums(bySchools[,2:7]) # add totals
bySchools # order by total: A, B, E, C, D
str(bySchools)

# histograms didn't work with data organized in this way
# ggplot(bySchools, aes(School)) + geom_bar(aes(Behind))

# getting the percents of students to see how that looks
bySchools.Per = bySchools
bySchools.Per[,2:7] = signif(bySchools[,2:7]/rowSums(bySchools[,2:7])*100, digits = 3)  
bySchools.Per$Total = signif(bySchools.Per$Total/sum(bySchools.Per$Total)*100, digits = 3) 

bySchools$Per = bySchools.Per$Total
bySchools.Per

grid.table(bySchools.Per)

# put it in a pie chart to represent student size
pieChart = ggplot(bySchools, aes("", Total, fill = School)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") + 
  geom_text(aes(label = paste0(round(Per), "%")), position = position_stack(vjust = 0.5),size = 2) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Student Count") +
  scale_fill_manual(values = colorPalette) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

# function to reformat data to get the histograms to look the way I want them
reformat = function(schools, code) {
    a = cbind( rep( code, sum(schools[schools$School == code, ]$Middling)), rep( "Middling", sum(schools[schools$School == code, ]$Middling))) 
    b = cbind( rep( code, sum(schools[schools$School == code, ]$Behind)), rep( "Behind", sum(schools[schools$School == code, ]$Behind))) 
    c = cbind( rep( code, sum(schools[schools$School == code, ]$MoreBehind)), rep( "MoreBehind", sum(schools[schools$School == code, ]$MoreBehind))) 
    d = cbind( rep( code, sum(schools[schools$School == code, ]$VeryBehind)), rep( "VeryBehind", sum(schools[schools$School == code, ]$VeryBehind))) 
    e = cbind( rep( code, sum(schools[schools$School == code, ]$Completed)), rep( "Completed", sum(schools[schools$School == code, ]$Completed))) 
    
    NewTest = rbind(a,b,c,d,e)
    NewTest = as.data.frame(NewTest)
    colnames(NewTest) = c("School", "Status")
    NewTest[,2] = as.factor(NewTest[,2])

    NewTest[,2] = factor(NewTest[,2], levels(NewTest[,2])[c(3,1,4,5,2)])
    
    return(NewTest)
}

# reformat and make new df ala brute force
A = reformat(schools, "A")
B = reformat(schools, "B")
C = reformat(schools, "C")
D = reformat(schools, "D")
E = reformat(schools, "E")
school.hist = rbind(A,B,C,D,E)

a1 = reformat(bySchools.Per, "A")
b2 = reformat(bySchools.Per, "B")
c3 = reformat(bySchools.Per, "C")
d4 = reformat(bySchools.Per, "D")
e5 = reformat(bySchools.Per, "E")
school.hist.per = rbind(a1,b2,c3,d4,e5)

# plot the histograms
histPlot = ggplot(school.hist, aes(Status)) + geom_bar(aes (fill = School)) + 
  ggtitle("Status Counts") + scale_fill_manual( values = colorPalette) 
histPlot

histPlot.v2 = ggplot(school.hist.per, aes(Status)) + geom_bar(aes(fill = School)) + ggtitle("Status Percents") + scale_fill_manual( values = colorPalette)
histPlot.v2

# histograms show that the disparity in the percentages of students isn't as great.
histPlot = histPlot + theme(legend.position = "none")
grid.arrange(histPlot, histPlot.v2, ncol = 2)

# =============================================================================================================
# =============================================================================================================

# aggregate by sections.  I think this might be the way to go
bySections = aggregate(x = cbind(VA = schools$VeryAhead, M =  schools$Middling, B = schools$Behind, 
                                 MB = schools$MoreBehind, VB = schools$VeryBehind, C = schools$Completed ),
                       by = list(schools$School, schools$Section),
                       FUN = sum)
bySections

# combine section and school to create a new variable
# The idea is a unique name for each class
schools.sections = unite(bySections, Section, c(Group.1, Group.2), remove = TRUE, sep = "")
# keeping same col names to use the reformat function
colnames(schools.sections) = c("School", "VeryAhead", "Middling", "Behind", "MoreBehind", "VeryBehind", "Completed")

# brute force for percentages
schools.sections$Totals = rowSums(schools.sections[, 2:7])
schools.sections$VeryAheadPer = signif(schools.sections$VeryAhead/schools.sections$Totals*100, digits = 3)
schools.sections$MiddlingPer = signif(schools.sections$Middling/schools.sections$Totals*100, digits = 3)
schools.sections$BehindPer = signif(schools.sections$Behind/schools.sections$Totals*100, digits = 3)
schools.sections$MoreBehindPer = signif(schools.sections$MoreBehind/schools.sections$Totals*100, digits = 3)
schools.sections$VeryBehindPer = signif(schools.sections$VeryBehind/schools.sections$Totals*100, digits = 3)
schools.sections$CompletedPer = signif(schools.sections$Completed/schools.sections$Totals*100, digits = 3)

# order the data for ease of visuality
schools.sections = schools.sections[order(schools.sections$School) , ]
schools.sections

# make a new df for the percentages
schools.sections.per = schools.sections[ , c(1,9:14)]
schools.sections.per
colnames(schools.sections.per) = c("School", "VeryAhead", "Middling", "Behind", "MoreBehind", "VeryBehind", "Completed")

# empty df to fill 
section.hist = data.frame()
# no brute force this time, we loop through these
for (i in 1:nrow(schools.sections)) {
  section.hist = rbind(section.hist, reformat(schools.sections, schools.sections[i,1]))
}

# ended up not needing this idea... or better said, not being able to assign color to each section
# based on school
# section.hist$RealSchool = substring(section.hist[,1],1,1)

# plot them by section
histPlotSec = ggplot(section.hist, aes(Status)) + 
  geom_bar(aes (fill = School), position = 'dodge', colour = "black") + 
  ggtitle("Section Status Counts")
histPlotSec

section.hist.per = data.frame()
for (i in 1:nrow(schools.sections.per)) {
  section.hist.per = rbind(section.hist.per, reformat(schools.sections.per, schools.sections.per[i,1]))
}

histPlotSecV2 = ggplot(section.hist.per, aes(Status)) + 
  geom_bar(aes (fill = School), position = 'dodge', colour = "black") + 
  ggtitle("Section Status Percentages")
histPlotSecV2

histPlotSec = histPlotSec + theme(legend.position = "none")
grid.arrange(histPlotSec, histPlotSecV2, ncol = 2)
# =============================================================================================================
# =============================================================================================================

# going to try scaling the data and see what I get
NewSchools = schools
NewSchoolsSec = schools.sections[ , 1:8]

summary(NewSchoolsSec)
NewSchoolsSec$School = as.factor(NewSchoolsSec$School)

# got this method of applying min/max from a friend 
# it works just like:
# min_max_ageT = (NewSchoolsSec$Middling - min(NewSchoolsSec$Middling , na.rm = TRUE))
# / (max(NewSchoolsSec$Middling , na.rm = TRUE) - min(NewSchoolsSec$Middling , na.rm = TRUE))
max = apply(NewSchoolsSec[,3:7] , 2 , max)
min = apply(NewSchoolsSec[,3:7], 2 , min)
scaledSchool = as.data.frame(cbind(NewSchoolsSec$School, NewSchoolsSec$VeryAhead, 
                                      scale(NewSchoolsSec[,3:7], center = min, scale = max - min)))

colnames(scaledSchool) = c("Section", "VeryAhead", "Middling", "Behind", "MoreBehind", "VeryBehind", "Completed")

# some renaming of cols and adding School as its own col 
scaledSchool$Section = NewSchoolsSec$School
School = substring(scaledSchool$Section,1,1)
scaledSchool$School = School

# look at some box plots
A = ggplot(scaledSchool, aes(School, Middling)) + geom_boxplot(fill = colorPalette) 
#  + geom_point(aes(fill = 'black'), size = 1, shape = 1, position = position_jitterdodge())
B = ggplot(scaledSchool, aes(School, Behind)) + geom_boxplot(fill = colorPalette)
C = ggplot(scaledSchool, aes(School, MoreBehind)) + geom_boxplot(fill = colorPalette)
D = ggplot(scaledSchool, aes(School, VeryBehind)) + geom_boxplot(fill = colorPalette)
E = ggplot(scaledSchool, aes(School, Completed)) + geom_boxplot(fill = colorPalette)

gh =  textGrob("Scaled Box Plots")

# look exactly like the ones without scaled data
grid.arrange(gh,A,B,C,D,E, ncol=3) 

ggplot(A) + geom_boxplot(aes(School, Middling))

# box blots for each category per school
A = scaledSchool[School == "A", ]
B = scaledSchool[School == "B", ]
C = scaledSchool[School == "C", ]
D = scaledSchool[School == "D", ]
E = scaledSchool[School == "E", ]
schoolAB = as.data.frame(rbind(A,B))
ggplot(schoolAB, aes(School, Middling)) + geom_boxplot()
schoolCDE = as.data.frame(rbind(C,D,E))
ggplot(schoolCDE, aes(School, Middling)) + geom_boxplot()

boxplot(A$Middling, A$Behind, A$MoreBehind, A$VeryBehind, A$Completed)
boxplot(B$Middling, B$Behind, B$MoreBehind, B$VeryBehind, B$Completed)
boxplot(C$Middling, C$Behind, C$MoreBehind, C$VeryBehind, C$Completed)
boxplot(D$Middling, D$Behind, D$MoreBehind, D$VeryBehind, D$Completed)
boxplot(E$Middling, E$Behind, E$MoreBehind, E$VeryBehind, E$Completed)

# =============================================================================================================
# =============================================================================================================

# hypothesis test
bySchools
# dividing data into two categories
AtLvl = bySchools$Middling + bySchools$Completed
AtRisk = bySchools$Behind + bySchools$VeryBehind + bySchools$MoreBehind

hypoTest = data.frame(School = bySchools$School, AtLvl, AtRisk)
str(hypoTest)
hypoTest$PerAtLvl = hypoTest$AtLvl/(hypoTest$AtLvl+hypoTest$AtRisk) # most of the schools kids are behind
hypoTest$PerAtRisk = hypoTest$AtRisk/(hypoTest$AtLvl+hypoTest$AtRisk) # most of the schools kids are behind

grid.table(hypoTest)

# plot new reformated data to show how schools are doing
AtL = ggplot(hypoTest,aes(School, PerAtLvl)) + geom_bar(stat = 'identity', fill = colorPalette) + ggtitle("At Level Students") + coord_cartesian(ylim = c(0, 1)) 
AtR = ggplot(hypoTest,aes(School, PerAtRisk)) + geom_bar(stat = 'identity', fill = colorPalette) + ggtitle("At Risk Students") + coord_cartesian(ylim = c(0, 1)) 

grid.arrange(AtL, AtR, ncol = 2) 

# same as above but by section
AtLvl = NewSchoolsSec$Middling + NewSchoolsSec$Completed
AtRisk = NewSchoolsSec$Behind + NewSchoolsSec$VeryBehind + NewSchoolsSec$MoreBehind

hypoTestSec = data.frame(Section = NewSchoolsSec$School, AtLvl, AtRisk)
hypoTestSec$PerAtLvl = hypoTestSec$AtLvl/(hypoTestSec$AtLvl+hypoTestSec$AtRisk)
hypoTest # only sections with students above 50% are in School B
summary(hypoTestSec)

# function to get p values to place in for loop
twoSample = function(val01, val02, Col01,  Col02, Col03, Col04, test = 'two.sided' ) {
  
  vec01 = c(rep(val01, Col01), rep(val02, Col02)) # atlvl/atrisk school 1
  vec02 = c(rep(val01, Col03), rep(val02, Col04)) # atlvl/atrisk school 2
  
  result = t.test(vec01, vec02, alternative = test)
  
  return (result)
}


# do a comparison of each school and get the pvalue.
# comparing both samples and testing if equal
# if p is low, h0 must go, reject the null hypo, 
# which in this case is the samples are equal
# which may mean something about the school is affecting performance

hypoTest = hypoTestSec
count = 1
resultList = list()
resultDF = data.frame()
for (i in 1:nrow(hypoTest)){
  for(j in 1:nrow(hypoTest)){
    if(i != j){
      result =  twoSample(1,0, hypoTest[i,2], hypoTest[i,3],hypoTest[j,2], hypoTest[j,3])
      resultList[paste(hypoTest[i,1], hypoTest[j,1], sep = '_')] = result$p.value
      count = count+1
      results = cbind( paste(hypoTest[i,1], hypoTest[j,1], sep = '_'), signif(result$p.value, digits = 5), result$p.value < 0.05)
      resultDF =  rbind(resultTest, results)
    }  
  }
}

colnames(resultDF) = c("Schools", "Pvalue", "RejectNull")
GreatSchool = resultDF[resultDF$RejectNull == TRUE, ]
GreatSchool = GreatSchool[c(-1,-5,-6),]

grid.table(GreatSchool)
grid.table(hypoTest[,-4])


#SQL testing capability of the process.
hypoTest
sqlVal = c()
for (i in 1:nrow(hypoTest)) {
  #D = 1 defect per unit, student falls behind is a defect
  #U = hypoTest[i,2] + hypoTest[i,3] AtLvl+AtRisk
  #A = hypoTest[i,3] AtRisk
  #DPMO = A/DU = DPO
  #SQL = DPO * 1,000,000
  sqlVal =  c(sqlVal, (hypoTest[i,3]/(1*hypoTest[i,2] + hypoTest[i,3]) ) *1000000 )
}

# something is wrong. 
# either the program or the school
# but b/c 1 school is doing better, but still ass
# tend to lean that the program is bogus!
sqlVal
sqlLvl = c(0.9, 1.4, 1.1, 0.9, 1.1 )
Yield = c(28,46,35,28,35)
SQL = data.frame(School = hypoTest$School, sqlVal, sqlLvl, Yield)
grid.table(SQL)

# lets look at sections just for s&g
sqlValSec = c()
for (i in 1:nrow(hypoTestSec)) {
  sqlValSec =  c(sqlValSec, (hypoTestSec[i,3]/(1*hypoTestSec[i,2] + hypoTestSec[i,3]) ) *1000000 )
}

# best sql is 1.9, 65%
min(sqlValSec)
hypoTestSec[which.min(sqlValSec),] # B6 is best section

SecSQL = data.frame(hypoTestSec$Section, sqlValSec)

SecSQL[sqlValSec < 500000,]

grid.table(data.frame(hypoTest[,-4],SQL[,-1]))

