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


# number of words appearing percentage of data
dec.df = as.data.frame(dec.mat)
# test.5 = wordPercentage(dec.df, .5) #2
# test.4 = wordPercentage(dec.df, .4) #2
# test.3 = wordPercentage(dec.df, .3) #3
# test.2 = wordPercentage(dec.df, .2) #10
test.1 = wordPercentage(dec.df, .1) #42
test.05 = wordPercentage(dec.df, .05) #121

plot.df.1 = dec.df[,test.1]
plot.df.2 = dec.df[,test.05]

# plot those words
wordcloud(colnames(plot.df.1), as.matrix(colSums(plot.df.1)), max.words = 200, random.order = F,rot.per = .35,colors=brewer.pal(8, "Dark2"))
wordcloud(colnames(plot.df.2), as.matrix(colSums(plot.df.2)), max.words = 200, random.order = F, rot.per = .35,colors=brewer.pal(8, "Dark2"))

deception.T = data.frame(t(apply(dec.df, 1, function(i) round(i/sum(i),3))))
reviewLength = rowSums(dec.df[,1:1337])


P1 = plot.df.1
P1$title = titles
P1$lie = deception.df[,1]
P1$sentiment = deception.df[,2]
P1$reviewLength = reviewLength
P1.TSL = P1[,c(43:46,1:42)]

P2 = plot.df.2
P2$title = titles
P2$lie = deception.df[,1]
P2$sentiment = deception.df[,2]
P2$reviewLength = reviewLength
P2.TSL = P2[,c(122:125,1:121)]


deception.T$title = titles
deception.T$lie = deception.df[,1]
deception.T$sentiment = deception.df[,2]
deception.T$reviewLength = reviewLength
deception.TSL = deception.T[,c(1338,1339,1340,1:1337,1341)]

#colnames(deception.TSL[,1337:1341])
deception.L = deception.TSL[,c(1,2,4:1341)]
deception.S = deception.TSL[,c(1,3:1341)]

P1.TSL.L = P1.TSL[,-3]
P1.TSL.S = P1.TSL[,-2]
P2.TSL.L = P2.TSL[,-3]
P2.TSL.S = P2.TSL[,-2]


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

# Accuracy : 0.6667          
# 95% CI : (0.4604, 0.8348)
# No Information Rate : 0.5185          
# P-Value [Acc > NIR] : 0.08796         
# 
# Kappa : 0.3379          
# Mcnemar's Test P-Value : 0.50499         


#plot
grid.table(conmat.naive.lie)

model_svm.lie = svm(lie ~., data = lie.data.trng, scale = F, cost = 1, 
                    kernel = 'polynomial', 
                    # kernel = 'linear',
                    # kernel = 'radial',
                    # kernel = 'sigmoid',
                    type = 'C')
summary(model_svm.lie)
pred_svm.lie = predict(model_svm.lie, lie.data.test[,-1], type = 'class')
# length(pred_svm.lie)
# length(lie.data.test$lie)

model_svm.lie.table = table(pred_svm.lie, lie.data.test$lie)
(conmat.svm.lie = confusionMatrix(model_svm.lie.table))

# Accuracy : 0.5185          
# 95% CI : (0.3195, 0.7133)
# No Information Rate : 0.5185          
# P-Value [Acc > NIR] : 0.5770322       
# 
# Kappa : 0               
# Mcnemar's Test P-Value : 0.0008741    

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

# Accuracy : 0.7778          
# 95% CI : (0.5774, 0.9138)
# No Information Rate : 0.5185          
# P-Value [Acc > NIR] : 0.005195        
# 
# Kappa : 0.55            
# Mcnemar's Test P-Value : 0.220671  

#plot
grid.table(conmat.naive.sent)

model_svm.sent = svm(sentiment ~., data = lie.data.trng.2, scale = F)
summary(model_svm.sent)
pred_svm.sent = predict(model_svm.sent, lie.data.test.2[,-1], type = 'class')
# length(pred_svm.lie)
# length(lie.data.test$lie)

model_svm.sent.table = table(pred_svm.sent, lie.data.test.2$sentiment)
(conmat.svm.sent = confusionMatrix(model_svm.sent.table))

# Accuracy : 0.5185          
# 95% CI : (0.3195, 0.7133)
# No Information Rate : 0.5926          
# P-Value [Acc > NIR] : 0.8364          
# 
# Kappa : 0.1159          
# Mcnemar's Test P-Value : 0.0265    

# =============================================================================================================
# MODELS - LIE smaller datasets
# =============================================================================================================

P1.TSL.L = P2.TSL.L # run model with longer df
# P1.TSL.L = P1.TSL[,-3] # return to original
P1.TSL.L = P1.TSL.L[sample(nrow(P1.TSL.L)), ] # randomize

model.data.p1 = testingSample(P1.TSL.L[,-1])  
lie.data.trng.p1 = model.data.p1[[1]]
lie.data.test.p1 = model.data.p1[[2]]

lie.p1.mod = naiveBayes(lie.data.trng.p1$lie ~. , data = lie.data.trng.p1, laplace = 1, na.action = na.pass)
summary(lie.p1.mod)
lie.p1.pred = predict(lie.p1.mod, lie.data.test.p1[,-1], type = 'class')
# length(pred_naive.lie)
# length(lie.data.test$lie)

lt1 = table(lie.p1.pred, lie.data.test.p1$lie)
(conmat.lie.p1 = confusionMatrix(lt1))

# P1
# Accuracy : 0.4074         
# 95% CI : (0.2239, 0.612)
# No Information Rate : 0.5556         
# P-Value [Acc > NIR] : 0.959          
# 
# Kappa : -0.2           
# Mcnemar's Test P-Value : 1.000 

# P2
# Accuracy : 0.4815          
# 95% CI : (0.2867, 0.6805)
# No Information Rate : 0.5556          
# P-Value [Acc > NIR] : 0.8336          
# 
# Kappa : -0.0328         
# Mcnemar's Test P-Value : 0.7893   

#plot
grid.table(lt1)

model_svm.lie.p1 = svm(lie ~., data = lie.data.trng.p1, scale = F)
summary(model_svm.lie.p1)
pred_svm.lie.p1 = predict(model_svm.lie.p1, lie.data.test.p1[,-1], type = 'class')
# length(pred_svm.lie)
# length(lie.data.test$lie)

model_svm.lie.table.p1 = table(pred_svm.lie.p1, lie.data.test.p1$lie)
(conmat.svm.lie = confusionMatrix(model_svm.lie.table.p1))

# P1
# Accuracy : 0.3704         
# 95% CI : (0.194, 0.5763)
# No Information Rate : 0.6296         
# P-Value [Acc > NIR] : 0.99830        
# 
# Kappa : -0.1007        
# Mcnemar's Test P-Value : 0.01529    

# P2
# Accuracy : 0.4444          
# 95% CI : (0.2548, 0.6467)
# No Information Rate : 0.5185          
# P-Value [Acc > NIR] : 0.8322          
# 
# Kappa : -0.1219         
# Mcnemar's Test P-Value : 0.6056   

#plot
grid.table(conmat.svm.lie)

# =============================================================================================================
# MODELS - SENTIMENT smaller datasets
# =============================================================================================================

P1.TSL.S = P2.TSL.S # run model with longer df
# P1.TSL.S = P1.TSL[,-2] # return to original
P1.TSL.S = P1.TSL.S[sample(nrow(P1.TSL.S)), ] # randomize


model.data.p2 = testingSample(P1.TSL.S[,-1])  
lie.data.trng.p2 = model.data.p2[[1]]
lie.data.test.p2 = model.data.p2[[2]]

model_naive.sentp = naiveBayes(lie.data.trng.2$sentiment ~. , data = lie.data.trng.p2, laplace = 1, na.action = na.pass)
summary(model_naive.sentp)
pred_naive.sentp = predict(model_naive.sentp, lie.data.test.p2[,-1], type = 'class')
# length(pred_naive.lie)
# length(lie.data.test$lie)

model_naive.sent.tablep = table(pred_naive.sentp,lie.data.test.p2$sentiment)
(conmat.naive.sentp = confusionMatrix(model_naive.sent.tablep))

# P1
# Accuracy : 0.3704         
# 95% CI : (0.194, 0.5763)
# No Information Rate : 0.5185         
# P-Value [Acc > NIR] : 0.959          
# 
# Kappa : -0.2439        
# Mcnemar's Test P-Value : 0.332    

# P2
# Accuracy : 0.7407          
# 95% CI : (0.5372, 0.8889)
# No Information Rate : 0.5926          
# P-Value [Acc > NIR] : 0.08283         
# 
# Kappa : 0.4553          
# Mcnemar's Test P-Value : 1.00000  


#plot
grid.table(conmat.naive.sentp)

model_svm.sentp = svm(sentiment ~., data = lie.data.trng.p2, scale = F)
summary(model_svm.sent)
pred_svm.sentp = predict(model_svm.sentp, lie.data.test.p2[,-1], type = 'class')
# length(pred_svm.lie)
# length(lie.data.test$lie)

model_svm.sent.tablep = table(pred_svm.sentp, lie.data.test.p2$sentiment)
(conmat.svm.sent = confusionMatrix(model_svm.sent.tablep))

# P1
# Accuracy : 0.3333          
# 95% CI : (0.1652, 0.5396)
# No Information Rate : 0.5185          
# P-Value [Acc > NIR] : 0.9836          
# 
# Kappa : -0.3352         
# Mcnemar's Test P-Value : 1.0000    

# P2
# Accuracy : 0.4444          
# 95% CI : (0.2548, 0.6467)
# No Information Rate : 0.5926          
# P-Value [Acc > NIR] : 0.95984         
# 
# Kappa : -0.0202         
# Mcnemar's Test P-Value : 0.03887   

