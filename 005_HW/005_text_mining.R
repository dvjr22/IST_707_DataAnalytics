# Diego Valdes
# IST 707
# Apr 26, 2019
# HW 5

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
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_707/HW/005_HW")



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

FedCorpus =  Corpus(DirSource("FedPapersCorpus")) # corpus, a collection of docs
getTransformations() # list of attributes that can be in the last
(ndocs = length(FedCorpus)) # 85 docs


##The following will show you that you read in all the documents
summary(FedCorpus)

# show metadata for each element in corpus
(meta(FedCorpus[[1]]))
(meta(FedCorpus[[1]],1))

(meta(FedCorpus[[85]]))
(meta(FedCorpus[[85]],5))

# stolen from Gates
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
(minTermFreq = ndocs * 0.0001)
# ignore overly common words i.e. terms that appear in more than 50% of the documents
(maxTermFreq = ndocs * 1)

# document term matrix
# some of the commented out stuff isn't working right
FedPapers_dtm <- DocumentTermMatrix(FedCorpus,
                                 control = list(
                                   stopwords = TRUE,
                                   wordLengths=c(3, 15),
                                   removePunctuation = T,
                                   removeNumbers = T,
                                   tolower=T,
                                   stemming = T,
                                   remove_separators = T,
                                   #stopwords = MyStopwords,
                                   #removeWords(STOPS),
                                   #removeWords(MyStopwords),
                                   #words = STOPS,
                                   bounds = list(global = c(minTermFreq, maxTermFreq))
                                 ))

# create 2 ver of dtm, w/ and w/o stop words
FedPapers_dtm_stop = remove_stopwords(FedPapers_dtm, stopwords = stopwords('english') ) # remove stop words

# Have a look
#inspect(FedPapers_dtm)
DTM_mat = as.matrix(FedPapers_dtm) # matrix
DTM_mat[,3]
DTM_mat[1:13,1:5]

Fed_DF = as.data.frame(as.matrix(FedPapers_dtm))
(ncol(Fed_DF)) # 4900
Fed_DF_S = as.data.frame(as.matrix(FedPapers_dtm_stop))
(ncol(Fed_DF_S)) # 4897

EssayLen = rowSums(Fed_DF) #essay length

cols_to_keep.50 = wordPercentage(Fed_DF, .5) #240
cols_to_keepS.50 = wordPercentage(Fed_DF_S, .5) #238 - lets move with them in

cols_to_keep.60 = wordPercentage(Fed_DF, .6) #163
cols_to_keep.65 = wordPercentage(Fed_DF, .65) #124
cols_to_keep.70 = wordPercentage(Fed_DF, .70) #106
cols_to_keep.75 = wordPercentage(Fed_DF, .75) #82

gw = cols_to_keep.75

newFed = Fed_DF[,gw] #105 variables
newFed$essayLen = EssayLen

# scale
newFed = data.frame(t(apply(Fed_DF, 1, function(i) round(i/sum(i),3))))
newFed = newFed[,gw]

#scale w/ max min
newFedV2 = scaleDFRow(Fed_DF, c(1:ncol(Fed_DF)))
newFedV2 = newFedV2[,gw]

essay_writer = gsub('_.*','',rownames(newFed))

newFed$essay_writer = essay_writer
newFedV2$essay_writer = essay_writer

newFed = newFed[,c(ncol(newFed),1:(ncol(newFed)-1))]
rownames(newFed) = c()
newFedV2 = newFedV2[,c(ncol(newFedV2),1:(ncol(newFedV2)-1))]
rownames(newFedV2) = c()

write.csv(newFed, file = "fed_papers_scaled_v1.csv",row.names=F)
write.csv(newFedV2, file = "fed_papers_scaled_v2.csv",row.names=F)

# =============================================================================================================
# Plots
# =============================================================================================================


rownames(Fed_DF) = c()
DF_FED = Fed_DF[,cols_to_keep.50]
wordcloud(colnames(DF_FED), as.matrix(colSums(DF_FED))/10, max.words = 200, random.order = F, rot.per = .35,colors=brewer.pal(8, "Dark2"))

DF_FED = Fed_DF[,cols_to_keep.60]
wordcloud(colnames(DF_FED), as.matrix(colSums(DF_FED))/10, max.words = 300, random.order = F, rot.per = .35,colors=brewer.pal(8, "Dark2"))

DF_FED = Fed_DF[,cols_to_keep.65]
wordcloud(colnames(DF_FED), as.matrix(colSums(DF_FED))/10, max.words = 300, random.order = F, rot.per = .35,colors=brewer.pal(8, "Dark2"))

DF_FED = Fed_DF[,cols_to_keep.70]
wordcloud(colnames(DF_FED), as.matrix(colSums(DF_FED))/10, max.words = 300, random.order = F, rot.per = .35,colors=brewer.pal(8, "Dark2"))

DF_FED = Fed_DF[,cols_to_keep.75]
wordcloud(colnames(DF_FED), as.matrix(colSums(DF_FED))/10, max.words = 300, random.order = F, rot.per = .35,colors=brewer.pal(8, "Dark2"))

