(MyStopwords = c("and", "the", "as", "a", "be", "their","but"))
#stopwords))

# stop words in tm package
(STOPS = stopwords('english'))
length(STOPS) #174


#FedPapers_dtm = weightTfIdf(FedPapers_dtm, normalize = TRUE)
#FedPapers_dtm = weightTfIdf(FedPapers_dtm, normalize = FALSE)

## Look at word freuqncies
(WordFreq <- colSums(Fed_DF))
(WordFreq <- colSums(Fed_DF_S))


(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])
## Row Sums
(Row_Sum_Per_doc <- rowSums((as.matrix(FedPapers_dtm))))

DF_FED = FED_V1
DF_FED = cleanThis(DF_FED)
summary(DF_FED)
str(DF_FED)
colNames = colnames(DF_FED)

test = ctree(essay_writer ~ ., data = FED_V1[FED_V1[,1]!='dispt',])
plot(test)
confusionMatrix(test)
table(test)

test = apply(Fed_DF, 1, function(i) i/sum(i))
test = t(test)
test = test[, cols_to_keep.60]
test[3,2] #0.003250271
newFed[3,2]/EssayLen[3] #0.003250271

as.matrix(colSums(DF_FED[,-1]))

as.matrix(colSums(DF_FED[,-1]))*10
set.seed(1234)
wordcloud(colnames(DF_FED[,-1]), as.matrix(colSums(DF_FED[,-1]))*15, max.words = 100, random.order = F, rot.per = .35,colors=brewer.pal(8, "Dark2"))
(head(sort(as.matrix(Novels_dtm)[13,], decreasing = TRUE), n=20))


