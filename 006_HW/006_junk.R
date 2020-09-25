
test = scaleDFRow(digit.df, c(2:785))
testA = test[1:ceiling(nrow(test)*.7),]
testB = test[(ceiling(nrow(test)*.7)+1):nrow(test),]
m = rpart(label ~., data = testA, method = "class", 
          control=rpart.control(minsplit=9, cp=0, maxdepth = 10))
p = predict(m, type = 'class')
c = table(p, testA$label)
(s = confusionMatrix(c))

pt = predict(m, testB[,-1], type = "class")
ct = table(pt, testB$label)
(st = confusionMatrix(ct))

scale(FINAL.RESULTS$`Tree A`)
percent(FINAL.RESULTS$`Tree A`)
FINAL.RESULTS/sum(FINAL.RESULTS$`Tree A`)