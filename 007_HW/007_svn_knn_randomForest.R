# Diego Valdes
# IST 707
# May 11, 2019
# HW 7

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
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_707/HW/007_HW")

# =============================================================================================================
# FUNCTIONS
# =============================================================================================================

# =============================================================================================================
#
gridify = function(RowToGridify) {
  
  test.df = data.frame()
  y=28
  x=1
  for (i in 1:ncol(RowToGridify)) {
    
    r1 = RowToGridify[1,c(x:y)]
    colnames(r1) = c(1:28)
    x=x+28
    y=y+28
    
    test.df = rbind(test.df,r1)
    
    if(y>ncol(RowToGridify)) break
    
  }
  test.df
}

# =============================================================================================================
#
rotate = function(x) {
  x = as.matrix(x)
  t(apply(x, 2, rev))
} 

# =============================================================================================================
#
display = function(x) {
  par(mar=c(0, 0, 0, 0))
  #image(m, useRaster=TRUE, axes=FALSE)
  image(x, axes = FALSE, col = grey(seq(0, 1, length = 256)))
}

# =============================================================================================================
#
plotIt = function(x) {
  x = gridify(x[1,-1])
  x = rotate(x)
  return(display(x))
}

# =============================================================================================================
#
testingSample = function(dataSet) {
  
  returnList = list()
  
  trainSet = dataSet[1:ceiling(nrow(dataSet)*.7),] # 70% for train
  testSet = dataSet[(ceiling(nrow(dataSet)*.7)+1):nrow(dataSet),] # 30% for test
  
  returnList[[1]] = trainSet
  returnList[[2]] = testSet
  
  return(returnList)
  
}

# =============================================================================================================
# converts cols in a dataset to numeric
#
toInteger = function(dataset, columns) {
  
  for (i in 1:length(columns)) {
    
    dataset[,columns[i]] = as.integer(dataset[,columns[i]])
    
  }
  return(dataset)
}


# =============================================================================================================
# IMPORT AND CLEAN DATA
# =============================================================================================================
# https://www.kaggle.com/c/digit-recognizer/data
DIGIT_OG = read.csv("Kaggle-digit-train-sample-small-1400.csv")
str(DIGIT_OG)

digit.df = DIGIT_OG
digit.df$label = factor(digit.df$label)
str(digit.df$label)

# train and testing dataset
digit.df = digit.df[sample(nrow(digit.df)), ] # randomize

trainSet_list = testingSample(digit.df)
trainSet = trainSet_list[[1]]
testSet = trainSet_list[[2]]


# values as 1 or 0
digitSet.1 = digit.df
for (i in 2:ncol(digitSet.1)) {
  
  digitSet.1[,i] = ifelse(digitSet.1[,i] > 0, 1,0)
}

# train and test sets for datasets 1|0
trainset_list = testingSample(digitSet.1)
trainSet.1 = trainset_list[[1]]
testSet.1 = trainset_list[[2]]


# final test set
DIGIT_TEST_OG = read.csv("Kaggle-digit-test-sample1000.csv")
digit.test = DIGIT_TEST_OG
digit.test$label = factor(digit.test$label)


# =============================================================================================================
# TO EXPLORE STRANGE NEW DATASETS, TO SEEK OUT NEW ANSWERS TO OLD SOLUTIONS,  
# TO BOLDY DISCOVER WHAT NO ONE HAS DISCOVERD BEFORE!
# =============================================================================================================

# plot the numbers
plotIt(digit.df[digit.df$label==0,])
plotIt(digit.df[digit.df$label==1,])
plotIt(digit.df[digit.df$label==2,])
plotIt(digit.df[digit.df$label==3,])
plotIt(digit.df[digit.df$label==4,])
plotIt(digit.df[digit.df$label==5,])
plotIt(digit.df[digit.df$label==6,])
plotIt(digit.df[digit.df$label==7,])
plotIt(digit.df[digit.df$label==8,])
plotIt(digit.df[digit.df$label==9,])

plotIt(digitSet.1[digitSet.1$label==0,])
plotIt(digitSet.1[digitSet.1$label==1,])
plotIt(digitSet.1[digitSet.1$label==2,])
plotIt(digitSet.1[digitSet.1$label==3,])
plotIt(digitSet.1[digitSet.1$label==4,])
plotIt(digitSet.1[digitSet.1$label==5,])
plotIt(digitSet.1[digitSet.1$label==6,])
plotIt(digitSet.1[digitSet.1$label==7,])
plotIt(digitSet.1[digitSet.1$label==8,])
plotIt(digitSet.1[digitSet.1$label==9,])

# =============================================================================================================
# MODELS
# =============================================================================================================

# =============================================================================================================
# KNN
# =============================================================================================================

str(knn.train[1:10])
train_label = trainSet$label
model.knn = knn(train = trainSet, test = testSet, cl = train_label, k = 5)
knn.matrix = table(model.knn, testSet$label)
(knn.matrix.stats = confusionMatrix(knn.matrix))

grid.table(knn.matrix)
# Overall Statistics
# Accuracy : 0.881           
# 95% CI : (0.8461, 0.9103)
# No Information Rate : 0.1143          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.8675          
# Mcnemar's Test P-Value : NA   

# stolen from site to create viz
#https://stats.stackexchange.com/questions/21572/how-to-plot-decision-boundary-of-a-k-nearest-neighbor-classifier-from-elements-o
mod15 <- knn(train = trainSet, test = testSet, cl = train_label, k=5, prob=TRUE)
prob <- attr(mod15, "prob")
prob <- ifelse(mod15=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
prob15 <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
          "5-nearest neighbour", axes=FALSE)
points(testSet$label, col=ifelse(train_label==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()


# =============================================================================================================
# SVM
# =============================================================================================================


model.svm = svm(label ~., data = trainSet, scale = FALSE)
pred.svm = predict(model.svm, testSet, type = 'class')
svm.matrix = table(pred.svm, testSet$label)
(svm.matrix.stats = confusionMatrix(svm.matrix))

# Overall Statistics
# 
# Accuracy : 0.1071          
# 95% CI : (0.0792, 0.1407)
# No Information Rate : 0.1143          
# P-Value [Acc > NIR] : 0.6994          
# 
# Kappa : 0               
# Mcnemar's Test P-Value : NA  

model.svm.v1 = svm(label ~., data = trainSet, scale = FALSE, cost = .5, kernel = 'polynomial')
model.svm.v1
pred.svm.v1 = predict(model.svm.v1, testSet, type = 'class')
svm.matrix.v1 = table(pred.svm.v1, testSet$label)
(svm.matrix.stats = confusionMatrix(svm.matrix.v1))

grid.table(svm.matrix.v1)
# Overall Statistics
# 
# Accuracy : 0.8881         
# 95% CI : (0.854, 0.9166)
# No Information Rate : 0.1143         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.8755         
# Mcnemar's Test P-Value : NA 

# =============================================================================================================
# RANDOM FOREST
# =============================================================================================================

model.rf.og = randomForest(label~., data=trainSet)
pred.rf.og = predict(model.rf.og, testSet, type = 'class')
rf.matrix.og = table(pred.rf.og, testSet$label)
(rf.matrix.stats.og = confusionMatrix(rf.matrix.og))

# Overall Statistics
# 
# Accuracy : 0.9143          
# 95% CI : (0.8833, 0.9392)
# No Information Rate : 0.1143          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9046          
# Mcnemar's Test P-Value : NA              

# testing accuracy in reducing trees from 100 - 400
# accuracy was always less on test set
model.rf = randomForest(label~., data=trainSet, ntree = 400)
model.rf
summary(model.rf)
pred.rf = predict(model.rf, testSet, type = 'class')
rf.matrix = table(pred.rf, testSet$label)
(rf.matrix.stats = confusionMatrix(rf.matrix))

grid.table(rf.matrix)


