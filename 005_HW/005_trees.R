# Diego Valdes
# IST 707
# Apr 26, 2019
# HW 5

# =============================================================================================================
# RESET WORKSPACE
# =============================================================================================================

#rm(list=ls()) # clear work space
#dev.off(dev.list()["RStudioGD"]) # clear plots

# =============================================================================================================
# LIBRARIES
# =============================================================================================================

loadLibraries()

library(ISLR)
library(MASS)
library(randomForest)
library(caret)

(originalWD = getwd()) # get original wd for saving to .r file
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_707/HW/005_HW")



# =============================================================================================================
# FUNCTIONS
# =============================================================================================================

# =============================================================================================================
#' transform data types
cleanThis = function(DF_FED){
  DF_FED = toNumeric(DF_FED, c(2:ncol(DF_FED))) # decimals to numeric
  DF_FED = toFactor(DF_FED, 1) # author and file names to factor
  return(DF_FED)
}

# =============================================================================================================
#' create the heat maps
plotThis = function(DF_FED, title = c('plot1','plots')) {
  
  FED_melt = melt(DF_FED) # melt for plotting
  
  ploHeat_Author = ggplot(FED_melt, aes(FED_melt[,1], variable, fill = value)) + geom_tile(color='white')  + scale_fill_gradient(low = "yellow", high = "red") + 
    ggtitle(title[1]) + theme_minimal() + xlab("Author")
  
  
  plotAuthorship = ggplot(DF_FED, aes(DF_FED[,1])) + geom_bar()+ xlab("Author") + ggtitle("Authorship")# number of essays each writer has
  # heat map of authors to mean of words
  
  FED_AM = aggregate(DF_FED[,-1], by=list(Writer = DF_FED[,1]), FUN=mean)
  FED_melt_am = melt(FED_AM)
  plotHeat_Author2 = ggplot(FED_melt_am, aes(FED_melt_am[,1], variable, fill = value)) + geom_tile(color='white')  + scale_fill_gradient(low = "yellow", high = "red") + 
    ggtitle(title[2]) + theme_minimal() + xlab("Author")
  
  plots = list(ploHeat_Author, plotHeat_Author2,plotAuthorship )
}

# =============================================================================================================
#' create tree models
treeThis = function(DF_FED) {
  
  FED_DISPT = DF_FED[DF_FED[,1]=='dispt',] # disputed authorship
  FED_AUTH = DF_FED[DF_FED[,1]!='dispt',] # known authorship
  
  #reset factors and check
  FED_AUTH[,1] = factor(FED_AUTH[,1])
  #levels(FED_AUTH$essay_writer)
  #str(FED_AUTH$essay_writer)
  
  FED_TREE = FED_AUTH[sample(nrow(FED_AUTH)),] # randomize dataset
  
  FED_TRNG = FED_TREE[1:ceiling(nrow(FED_TREE)*.7),] # 70% trng data
  FED_TEST = FED_TREE[(ceiling(nrow(FED_TREE)*.7)+1):nrow(FED_TREE),] # 30% test data
  
  Model.RF = randomForest(FED_TRNG[,-1], FED_TRNG[,1])
  summary(Model.RF)
  
  impWords = data.frame(round(importance(Model.RF), 2)) # importance of variables, high is better
  imp_words = rownames(impWords)
  imp_words = rowvar[order(impWords[,1], decreasing = T)]
  
  # make predictions
  results = predict(Model.RF, FED_TEST[ ,  -1])
  
  baseAcc = mean(results==FED_TEST[,1])
  
  # look at results w/ confusion matrix
  table(results)
  table(FED_TEST[,1])
  (matrix = table(results, FED_TEST[,1]))
  
  matrixStats = confusionMatrix(matrix)
  
  results.disput = predict(Model.RF, FED_DISPT[,-1])
  
  theList = list(Model.RF,impWords,imp_words,results,matrix,matrixStats, results.disput,baseAcc)
  
}

# =============================================================================================================
#' plot the confustion matrix
plotThemResults = function(CON_MATRIX, title){
  con_mat_v1 = data.frame(CON_MATRIX)
  colnames(con_mat_v1) = c("Results","Actual","Freq")
  plotConMat_v1 = ggplot(con_mat_v1, aes(Results,Actual)) +  geom_tile(aes(fill=Freq), color='black') +
    scale_fill_gradient(low = "white", high = "steelblue") + geom_text(aes(label = sprintf("%1.0f",Freq)), vjust = 1) +
    ggtitle(title) + scale_x_discrete(position = 'top')# + scale_y_discrete(limits = rev(con_mat_v1$Actual))
  #return(plotConMat_v1) scale_x_discrete(limits = rev(levels(the_factor)))
}

plot_v1_conmat = plotThemResults(fed_v1_results[5], "Confusion Matrix: Scale Essay Length" )
# =============================================================================================================
# IMPORT AND CLEAN DATA
# =============================================================================================================

# scaled based on essay length
fed_data_og_v1 = read.csv2("fed_papers_scaled_v1.csv", sep = ',', stringsAsFactors = FALSE, numerals = 'no.loss')
# scaled based on max/min
fed_data_og_v2 = read.csv2("fed_papers_scaled_v2.csv", sep = ',', stringsAsFactors = FALSE, numerals = 'no.loss')
# original dataset
fed_data_og_v3 = read.csv2("fedPapers85.csv", sep = ',', stringsAsFactors = FALSE, numerals = 'no.loss')

# take a look at new datasets
naInData(fed_data_og_v1)# no NA
summary(fed_data_og_v1)
str(fed_data_og_v1)

naInData(fed_data_og_v2)# no NA
summary(fed_data_og_v2)
str(fed_data_og_v2)

# create df that will be altered
FED_V1 = fed_data_og_v1
FED_V2 = fed_data_og_v2
FED_V3 = fed_data_og_v3[,-2]

# change data types
FED_V1 = cleanThis(FED_V1)
FED_V2 = cleanThis(FED_V2)
FED_V3 = cleanThis(FED_V3)

grid.table(FED_V1[10:13,1:10])
grid.table(FED_V2[10:13,1:10])
grid.table(FED_V3[10:13,1:10])
# =============================================================================================================
# TO EXPLORE STRANGE NEW DATASETS, TO SEEK OUT NEW ANSWERS TO OLD SOLUTIONS,  
# TO BOLDY DISCOVER WHAT NO ONE HAS DISCOVERD BEFORE!
# =============================================================================================================


plots_v1 = plotThis(FED_V1, c("Author: Scale Essay Length","Author: Scale Essay Length | Mean") )
plots_v2 = plotThis(FED_V2, c("Author: Scale Max/Min","Author: Scale Max/Min | Mean") )
plots_v3 = plotThis(FED_V3, c("Author: Stop Words","Author: Function Words | Mean") )

par(mfrow=c(1,3))
wordcloud(colnames(FED_V1[,-1]), as.matrix(colSums(FED_V1[,-1]))*15, max.words = 100, random.order = F, rot.per = .35,colors=brewer.pal(8, "Dark2"))
wordcloud(colnames(FED_V2[,-1]), as.matrix(colSums(FED_V2[,-1]))*15, max.words = 100, random.order = F, rot.per = .35,colors=brewer.pal(8, "Dark2"))
wordcloud(colnames(FED_V3[,-1]), as.matrix(colSums(FED_V3[,-1]))*15, max.words = 100, random.order = F, rot.per = .35,colors=brewer.pal(8, "Dark2"))

grid.arrange(plots_v1[[2]], plots_v2[[2]], plots_v3[[2]], nrow = 1)
# =============================================================================================================
# TREES!!!
# =============================================================================================================
set.seed(123)

#FED_DISPT = FED_V1[FED_V1[,1]=='dispt',] # disputed authorship

fed_v1_results = treeThis(FED_V1[,c(-2,-30)])
fed_v2_results = treeThis(FED_V2[,c(-2,-30)])
fed_v3_results = treeThis(FED_V3)

fed_v1_results[1] # RF model  25%
fed_v1_results[2] # importance
fed_v1_results[[3]][1:5] # important words
fed_v1_results[4] # test prediction
fed_v1_results[5] # confustion matrix
fed_v1_results[6] # matrix stats Accuracy : 86% | 95% CI : (0.5463, 0.9218) Kappa : 0.3529    
fed_v1_results[7] # disputed prediction

fed_v2_results[1] # RF model 11.54%
fed_v2_results[2] # importance
fed_v2_results[[3]][1:5] # important words
fed_v2_results[4] # test prediction
fed_v2_results[5] # confustion matrix
fed_v2_results[6] # matrix stats Accuracy : 59% | 95% CI : (0.4513, 0.8614) Kappa : 0.1149       
fed_v2_results[7] # disputed prediction


fed_v3_results[1] # RF model 13.46%
fed_v3_results[2] # importance
fed_v3_results[[3]][1:5] # important words
fed_v3_results[4] # test prediction
fed_v3_results[5] # confustion matrix
fed_v3_results[6] # matrix stats Accuracy : 0.8636 | 95% CI : (0.6509, 0.9709) Kappa : 0.5829 
fed_v3_results[7] # disputed prediction


IMP_WORDS = data.frame(fed_v1_results[[3]][1:5],fed_v2_results[[3]][1:5],fed_v3_results[[3]][1:5])
colnames(IMP_WORDS) = c("Model 1","Model 2","Model 3")
rownames(IMP_WORDS) = c(1,2,3,4,5)
grid.table(IMP_WORDS)

plot_v1_conmat = plotThemResults(fed_v1_results[5], "Confusion Matrix: Scale Essay Length" )
plot_v2_conmat = plotThemResults(fed_v2_results[5], "Confusion Matrix: Scale Max/Min")
plot_v3_conmat = plotThemResults(fed_v3_results[5], "Confusion Matrix: Stop Words")

grid.arrange(plot_v1_conmat, plot_v2_conmat, plot_v3_conmat, nrow=1)

Error = c("25%","25%","13%")
TestACC = c('86%','59%','86%')
Kappa = c('0.35','0.11','0.58')
CI_95 = c('(0.5463, 0.9218)', '(0.4513, 0.8614)','(0.6509, 0.9709)')
StatDF = data.frame(Error,TestACC,Kappa,CI_95)
rownames(StatDF) = c("Model 1","Model 2","Model 3")
grid.table(StatDF)

Predictions = data.frame(fed_data_og_v3[fed_data_og_v3$author=='dispt',2],fed_v1_results[7],fed_v2_results[7],fed_v3_results[7])
colnames(Predictions) = c("Essay","Model 1","Model 2","Model 3")
grid.table(Predictions)

# lets see what happens with this?
nrow(FED_V1[FED_V1$essay_writer == 'HM' | FED_V1$essay_writer == 'Jay',])
testDF = FED_V1[FED_V1$essay_writer == 'HM' | FED_V1$essay_writer == 'Jay'|FED_V1$essay_writer=='dispt',]
testDf_v2 = FED_V1[FED_V1$essay_writer == 'Hamilton' | FED_V1$essay_writer == 'Madison' |FED_V1$essay_writer=='dispt',]

testdf_v3 = testDf_v2[59:66,]

balanced = data.frame(rbind(testDF,testdf_v3))

test_1 = treeThis(testDF)
test_2 = treeThis(testDf_v2)
balanced_results = treeThis(balanced)

test_1[1]
test_1[6]

test_2[1]
test_2[6]
test_2[7]

balanced_results[1]
balanced_results[6]
