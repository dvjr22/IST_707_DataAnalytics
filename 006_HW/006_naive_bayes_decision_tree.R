# Diego Valdes
# IST 707
# May 11, 2019
# HW 6

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
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_707/HW/006_HW")

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
trainSet = digit.df[1:ceiling(nrow(digit.df)*.7),] # 70% for train
testSet = digit.df[(ceiling(nrow(digit.df)*.7)+1):nrow(digit.df),] # 30% for test

# values as 1 or 0
digitSet.1 = digit.df
for (i in 2:ncol(digitSet.1)) {
  
  digitSet.1[,i] = ifelse(digitSet.1[,i] > 0, 1,0)
}

# compare the two plots
plotIt(digit.df)
plotIt(digitSet.1)

# train and test sets for datasets 1|0
trainSet.1 = digitSet.1[1:ceiling(nrow(digitSet.1)*.7),] # 70% for train
testSet.1 = digitSet.1[(ceiling(nrow(digitSet.1)*.7)+1):nrow(digitSet.1),] # 30% for test

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
# TREE
# =============================================================================================================


set.seed(144)

# RAW data
model_tree.A = rpart(trainSet$label ~., data = trainSet, method = "class", 
                   control=rpart.control(minsplit=9, cp=0, maxdepth = 10))
summary(model_tree.A)
pred_tree.A = predict(model_tree.A, type = "class") # get model run on train set
conMatrix.A = table(pred_tree.A, trainSet$label)  
(matrixStats.A = confusionMatrix(conMatrix.A))

# plots
grid.table(matrixStats.A)
fancyRpartPlot(model_tree.A)

# RAW test data
pred_tree.test.A = predict(model_tree.A, testSet[,-1], type = "class")
conMatrix.test.A = table(pred_tree.test.A, testSet$label)
(matrixStats.test.A = confusionMatrix(conMatrix.test.A))

# plots
grid.table(conMatrix.test.A)

# 1|0 data
model_tree.B = rpart(trainSet.1$label ~., data = trainSet.1, method = "class", 
                     control=rpart.control(minsplit=9, cp=0, maxdepth = 10))
summary(model_tree.B)
pred_tree.B = predict(model_tree.B, type = "class")
conMatrix.B = table(pred_tree.B, trainSet.1$label)  # model run on train set
(matrixStats.B = confusionMatrix(conMatrix.B))

grid.table(conMatrix.B)
fancyRpartPlot(model_tree.B)


pred_tree.test.B = predict(model_tree.B, testSet.1[,-1], type = "class")
conMatrix.test.B = table(pred_tree.test.B, testSet.1$label)
(matrixStats.test.B = confusionMatrix(conMatrix.test.B))


# plots
grid.table(conMatrix.test.B)

# =============================================================================================================
# NAIVE BAYES
# =============================================================================================================

model_naive.A = naiveBayes(trainSet$label ~., data = trainSet, laplace = 1, na.action = na.pass)
summary(model_naive.A)
pred_naive.test.A = predict(model_naive.A, testSet[,-1], type = "class")

test.A = predict(model_naive.A, trainSet[,-1], type = "class")
test.A = table(test.A, trainSet$label)
(test.nb.A = confusionMatrix(test.A))

conMat_NB.A = table(pred_naive.test.A, testSet$label)
(matstat.nb.A = confusionMatrix(conMat_NB.A))

#plot
grid.table(conMat_NB.A)

model_naive.B = naiveBayes(trainSet.1$label ~., data = trainSet.1, laplace = 1, na.action = na.pass)
summary(model_naive.B)
pred_naive.test.B = predict(model_naive.B, testSet.1[,-1], type = "class")

test.B = predict(model_naive.B, trainSet.1[,-1], type = "class")
test.B = table(test.B, trainSet.1$label)
(test.nb.B = confusionMatrix(test.B))

conMat_NB.B = table(pred_naive.test.B, testSet.1$label)
(matstat.nb.B = confusionMatrix(conMat_NB.B))

#plot
grid.table(conMat_NB.B)

# =============================================================================================================
# FINAL TESTS
# =============================================================================================================


FINAL.TREE.A = predict(model_tree.A, digit.test[,-1], type = "class")
FINAL.TREE.B = predict(model_tree.B, digit.test[,-1], type = "class")

FINAL.NB.A = predict(model_naive.A, digit.test[,-1], type = "class")
FINAL.NB.B = predict(model_naive.B, digit.test[,-1], type = "class")
table(FINAL.TREE.A)
table(FINAL.TREE.B)
x = data.frame(table(FINAL.TREE.A),table(FINAL.TREE.B),table(FINAL.NB.A),table(FINAL.NB.B))
x = x[,c(-1,-3,-5,-7)]
FINAL.RESULTS = x
colnames(FINAL.RESULTS) = c('Tree A','Tree B','NB A', 'NB B')
grid.table(FINAL.RESULTS)

for (i in 1:ncol(FINAL.RESULTS)) {
  FINAL.RESULTS[,i] = percent(FINAL.RESULTS[,i]/sum(FINAL.RESULTS[,i]))
  
}

?naive_bayes

#model_naive.B = naiveBayes(trainSet.1$label ~., data = trainSet.1, laplace = 1, na.action = na.pass)
test.nb = naive_bayes(trainSet.1$label ~., data = trainSet.1, laplace = 1, na.action = na.pass)
summary(test.nb)
model = predict(test.nb, testSet.1[,-1], type = "class")

test_cm = table(model, testSet.1$label)
(test_nb = confusionMatrix(test_cm))



