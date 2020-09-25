# Diego Valdes
# IST 707
# May 11, 2019
# HW 8

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
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_707/HW/008_HW")

# =============================================================================================================
# FUNCTIONS
# =============================================================================================================

#'determine the cols where a word appears less than the desired frequency
#' @param   df data frame
#' @param   frequency percent want col to meet
#' wordPercentage()
wordPercentage = function(df, frequency){
  count = 0
  index = c()
  for (c in 1:ncol(df)) {
    
    colCount = 0
    
    for (r in 1:nrow(df)) {
      if(df[r,c] > 0) {
        colCount=colCount+1
      }
    }
    if(colCount/nrow(df) > frequency)
      index = c(index,c)
  }
  return(index)
}


# =============================================================================================================
# IMPORT AND CLEAN DATA
# =============================================================================================================

DECEPTION.OG = read.csv("deception_data_converted_final_edited.csv", stringsAsFactors = F)
deception.df = DECEPTION.OG

# titles for reviews
titles = c()
for (i in 1:nrow(deception.df)) {
  
  Title = paste('review_',i, sep = '')
  titles = append(titles, Title)
  
}

deception.df$title = titles

deception.df = toFactor(deception.df, c(1,2))
summary(deception.df)
str(deception.df)

# deception.S = deception.df[,c(2,3,4)]
# deception.L = deception.df[,c(1,3,4)]

# CORPUS
corpus.deception = Corpus(VectorSource(deception.df$review))
summary(corpus.deception)

dtm.deception = DocumentTermMatrix(corpus.deception)

decption.rmv.stp = remove_stopwords(dtm.deception, stopwords = stopwords('english') )
dec.mat = as.matrix(decption.rmv.stp)



dec.df = as.data.frame(dec.mat)
test.5 = 



deception.T = data.frame(t(apply(dec.df, 1, function(i) round(i/sum(i),3))))

reviewLength = rowSums(dec.df[,1:1337])

deception.T$title = titles
deception.T$lie = deception.df[,1]
deception.T$sentiment = deception.df[,2]
deception.T$reviewLength = reviewLength
deception.TSL = deception.T[,c(1338,1339,1340,1:1337,1341)]

#colnames(deception.TSL[,1337:1341])
deception.L = deception.TSL[,c(1,2,4:1341)]
deception.S = deception.TSL[,c(1,3:1341)]


# =============================================================================================================
# MODELS - LIE
# =============================================================================================================

deception.L = deception.L[sample(nrow(deception.L)), ] # randomize

model.data.1 = testingSample(deception.L[,-1])  
lie.data.trng = model.data.1[[1]]
lie.data.test = model.data.1[[2]]

model_naive.lie = naiveBayes(lie.data.trng$lie ~. , data = lie.data.trng, laplace = 1, na.action = na.pass)
summary(model_naive.lie)
pred_naive.lie = predict(model_naive.lie, lie.data.test[,-1], type = 'class')
# length(pred_naive.lie)
# length(lie.data.test$lie)

model_naive.lie.table = table(pred_naive.lie,lie.data.test$lie)
(conmat.naive.lie = confusionMatrix(model_naive.lie.table))

#plot
grid.table(conmat.naive.lie)

model_svm.lie = svm(lie ~., data = lie.data.trng, scale = F)
summary(model_svm.lie)
pred_svm.lie = predict(model_svm.lie, lie.data.test[,-1], type = 'class')
# length(pred_svm.lie)
# length(lie.data.test$lie)

model_svm.lie.table = table(pred_svm.lie, lie.data.test$lie)
(conmat.svm.lie = confusionMatrix(model_svm.lie.table))

#plot
grid.table(conmat.svm.lie)

# =============================================================================================================
# MODELS - SENTIMENT
# =============================================================================================================

deception.S = deception.S[sample(nrow(deception.S)), ] # randomize

model.data.2 = testingSample(deception.S[,-1])  
lie.data.trng.2 = model.data.2[[1]]
lie.data.test.2 = model.data.2[[2]]

model_naive.sent = naiveBayes(lie.data.trng.2$sentiment ~. , data = lie.data.trng.2, laplace = 1, na.action = na.pass)
summary(model_naive.sent)
pred_naive.sent = predict(model_naive.sent, lie.data.test.2[,-1], type = 'class')
# length(pred_naive.lie)
# length(lie.data.test$lie)

model_naive.sent.table = table(pred_naive.sent,lie.data.test.2$sentiment)
(conmat.naive.sent = confusionMatrix(model_naive.sent.table))

#plot
grid.table(conmat.naive.sent)

model_svm.sent = svm(sentiment ~., data = lie.data.trng.2, scale = F)
summary(model_svm.sent)
pred_svm.sent = predict(model_svm.sent, lie.data.test.2[,-1], type = 'class')
# length(pred_svm.lie)
# length(lie.data.test$lie)

model_svm.sent.table = table(pred_svm.sent, lie.data.test.2$sentiment)
(conmat.svm.sent = confusionMatrix(model_svm.sent.table))

