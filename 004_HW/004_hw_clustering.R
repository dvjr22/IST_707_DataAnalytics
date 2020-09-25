# Diego Valdes
# IST 707
# Apr 20, 2019
# HW 4

# =============================================================================================================
# RESET WORKSPACE
# =============================================================================================================

rm(list=ls()) # clear work space
dev.off(dev.list()["RStudioGD"]) # clear plots

# =============================================================================================================
# LIBRARIES
# =============================================================================================================

loadLibraries()

originalWD = getwd() # get original wd for saving to .r file
setwd("C:/Users/dvjr2/Google Drive/Documents/Syracuse/IST_707/HW/004_HW")


# =============================================================================================================
# FUNCTIONS
# =============================================================================================================

# =============================================================================================================
# @dp df with mode set
# @ds df with all values and clusters
# calculate percentage right based on cluster assignments
getPercentages = function(dp, ds) {
  
  for (i in 1:nrow(dp)) {
    for (j in 3:ncol(ds)) {
      dp[i,(j-1)] = max(table(ds[ds$author == dp[i,1],][,j]))/sum(table(ds[ds$author == dp[i,1],][,j]))
    }
  }
  return(dp)
}


# =============================================================================================================
# @ds df of author cluster assignment: prediction.s[-1,]
# @dp df of essay predictions: essay_predict_sc[,c(-1,-2)]
# replaces cluster assignments with author
convertAuthor = function(ds,dp,colds,coldp) {
  
  for (i in 1:nrow(ds)) {
    
    for (j in 1:nrow(dp)) {
      if (dp[j,coldp] == ds[i,colds]) {
        dp[j,coldp] = as.character (ds[i,1])
      }
    }
  }
  return(dp)
}

# =============================================================================================================
# IMPORT AND CLEAN DATA
# =============================================================================================================

fed_data_og = read.csv2("fedPapers85.csv", sep = ',', stringsAsFactors = FALSE, numerals = 'no.loss')
naInData(fed_data_og)# no NA
summary(fed_data_og)
str(fed_data_og)

fed_data = fed_data_og
fed_data = toNumeric(fed_data, c(3:72)) # decimals to numeric
fed_data = toFactor(fed_data, c(1,2)) # author and file names to factor

summary(fed_data)
str(fed_data)
colNames = colnames(fed_data)


# =============================================================================================================
# TO EXPLORE STRANGE NEW DATASETS, TO SEEK OUT NEW ANSWERS TO OLD SOLUTIONS,  
# TO BOLDY DISCOVER WHAT NO ONE HAS DISCOVERD BEFORE!
# =============================================================================================================

# heat map raw
# heat map of authors to words
fed_data_melt = melt(fed_data[,-2]) # remove filename
plotHeatRaw = ggplot(fed_data_melt, aes(author, variable, fill = value)) + geom_tile(color='white') + scale_fill_gradient(low = "yellow", high = "red")

# heat map of essays to words
fed_data_essay_melt = melt(fed_data[,-1]) #remove author
plotHeatEssayRaw = ggplot(fed_data_essay_melt, aes(filename, variable, fill=value)) + geom_tile(color='white')  + scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Raw Data: Essays") + theme_minimal()+  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 6, hjust = 1))

# heat map of authors to mean of words
fed_data_am = aggregate(fed_data[,c(-1,-2)], by=list(Author = fed_data$author), FUN=mean)
fed_data_am_melt = melt(fed_data_am)
plotheatAuthorRaw = ggplot(fed_data_am_melt, aes(Author, variable, fill=value)) + geom_tile(color='white')  + scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Raw Data: Authors") + theme_minimal()


# heat map with cols scaled
# heat map of authors to words
scale_fed_data = scaleDFCol(fed_data, c(3:72), c(1,2))
scale_fd_hm = melt(scale_fed_data[,-2]) # remove essays
plotHeatMap = ggplot(scale_fd_hm, aes(author, variable, fill = value)) + geom_tile(color='white')+ scale_fill_gradient(low = "yellow", high = "red")

# heat map of essays to words
scale_essay = melt(scale_fed_data[,-1])
plotEssayHeatMap = ggplot(scale_essay, aes(filename, variable, fill = value)) + geom_tile(color='white')+ scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Scaled Data: Essays") + theme_minimal()+  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 6, hjust = 1))

# heat map of authors to mean of words
authors_df = aggregate(scale_fed_data [,c(-1,-2)], by=list(Author = scale_fed_data$author), FUN=mean)
author_melt = melt(authors_df)
authorHeatmap = ggplot(author_melt, aes(Author, variable, fill = value)) + geom_tile(color='white')+ scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Scaled Data: Authors") + theme_minimal()

# just to see
onePaper = scale_fed_data[-2:-11,]
onePaper_A = aggregate(onePaper [,c(-1,-2)], by=list(Author = onePaper$author), FUN=mean)
onePaper_A_melt = melt(onePaper_A)
onePaperPlot = ggplot(onePaper_A_melt, aes(Author, variable, fill = value)) + geom_tile(color='white')+ scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Scaled Data: Dispt 1") + theme_minimal()



# heat map with rows scaled
# heat map of authors to words
scale_fed_data_v2 = scaleDFRow(fed_data, c(3:72))
scale_fed_hm_v2 = melt(scale_fed_data_v2[,-2])
plotHeatMap_v2 = ggplot(scale_fed_hm_v2, aes(author, variable, fill = value)) + geom_tile()+ scale_fill_gradient(low = "yellow", high = "red")

# heat map of essays to words
scale_essay_v2 = melt(scale_fed_data_v2[,-1])
plotEssayHeatMap_v2 = ggplot(scale_essay_v2, aes(filename, variable, fill = value)) + geom_tile()+ scale_fill_gradient(low = "yellow", high = "red")

# heat map of authors to mean of words
authors_df_v2 = aggregate(scale_fed_data_v2 [,c(-1,-2)], by=list(scale_fed_data_v2$author), FUN=mean)
author_melt_v2 = melt(authors_df_v2)
authorHeatmap = ggplot(author_melt, aes(Group.1, variable, fill = value)) + geom_tile()+ scale_fill_gradient(low = "yellow", high = "red") 


grid.arrange(plotEssayHeatMap, plotHeatEssayRaw, ncol = 2)
grid.arrange(authorHeatmap, plotheatAuthorRaw, ncol = 2)

# =============================================================================================================
# kclustering
# =============================================================================================================


# =============================================================================================================
# SCALED
set.seed(123) # random number generator

# make unlabeled version of scaled data
unlabeledScale = scale_fed_data[,c(-2:-1)]

# suggested number of clusters
fviz_nbclust(unlabeledScale, kmeans, method="wss" )

# k means
model_s.k = kmeans(unlabeledScale, 4)
plotcluster(unlabeledScale, model_s.k$cluster)
clusplot(unlabeledScale, model_s.k$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
cluster_s.k = model_s.k$cluster # get cluster assignment vector

# hierarchical
distance = dist(unlabeledScale, method = 'euclidean')
model_s.h = hclust(distance, method = 'ward.D2')
plot(model_s.h)
cluster_s.h = cutree(model_s.h, k=4) #cluster assignments vector
rect.hclust(model_s.h, k=4, border = 'red')


#em
model_s.e = em.cluster.R(unlabeledScale, K=4, S=c(rep(TRUE, 70)))

slotNames(model_s.e)
cluster_s.e = model_s.e["mapClassification"] # cluster assignments vector

cluster_scaled = data.frame(scale_fed_data[,c(1,2)], cluster_s.k, cluster_s.h ,cluster_s.e ) #kmeans assignments
cluster_scaled$cluster_s.e = cluster_s.e+1 # cluster classifications match other results: range 0-3 -> 1-4


# =============================================================================================================
# UNSCALED

# unlabeled version of raw data
unlabeledUnscale = fed_data[,c(-2:-1)]

fviz_nbclust(unlabeledUnscale, kmeans, method ="wss") # 4 clusters

# kmeans
model_u.k = kmeans(unlabeledUnscale, 4)
plotcluster(unlabeledUnscale, model_u.k$cluster)
clusplot(unlabeledUnscale, model_u.k$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
cluster_u.k = model_u.k$cluster # cluster assignment vector

# hierarchical
distanceU = dist(unlabeledUnscale, method = 'euclidean')
model_u.h = hclust(distanceU, method = 'ward.D2')
plot(model_u.h)
cluster_u.h = cutree(model_u.h, k=4) #cluster assignments
rect.hclust(model_u.h, k=4, border = 'red')

#em
model_u.e = em.cluster.R(unlabeledUnscale, K=4, S=c(rep(TRUE, 70)))
slotNames(model_u.e)
cluster_u.e = model_u.e["mapClassification"] # cluster assignments

cluster_unscaled = data.frame(fed_data[,c(1,2)], cluster_u.k, cluster_u.h ,cluster_u.e ) #kmeans assignments
cluster_unscaled$cluster_u.e = cluster_u.e+1

# =============================================================================================================
# PREDICTION AND CLUSTER ASSIGNMENTS

# assign mode to an author
prediction.u = aggregate(cluster_unscaled[,-1:-2], by=list(cluster_unscaled$author), FUN=Mode)
prediction.s = aggregate(cluster_scaled[,-1:-2], by=list(cluster_scaled$author), FUN=Mode)
clusterNames_S = c("Author","cluster_s.k","cluster_s.h","cluster_s.e")
clusterNames_U = c("Author","cluster_u.k","cluster_u.h","cluster_u.e")
colnames(prediction.s) = clusterNames_S
colnames(prediction.u) = clusterNames_U

# graph cluster assignments
grid.table(prediction.u[-1,])
#Author cluster_s.k cluster_s.h cluster_s.e
#2 Hamilton           2           1           4
#3       HM           4           2           1
#4      Jay           1           4           1
#5  Madison           4           3           3

grid.table(prediction.s[-1,])
#Author cluster_s.k cluster_s.h cluster_s.e
#2 Hamilton           2           3           1
#3       HM           3           2           1
#4      Jay           1           4           2
#5  Madison           4           1           4

# percent of cluster correct
percent_un = getPercentages(prediction.u[-1,], cluster_unscaled)
percent_sc = getPercentages(prediction.s[-1,], cluster_scaled)

grid.table(percent_un)
grid.table(percent_sc)

# cluster predictions
essay_perdict_un = cluster_unscaled[cluster_unscaled$author=='dispt',]
essay_predict_sc = cluster_scaled[cluster_scaled$author=='dispt',]

# convert uncaled results to see authors
authors_un = essay_perdict_un[,c(-1,-2)]
authors_un = convertAuthor(prediction.u[-1,],authors_un,2,1 )
authors_un = convertAuthor(prediction.u[-1,],authors_un,3,2 )
authors_un = convertAuthor(prediction.u[-1,],authors_un,4,3 )

# convert scaled results to see authors
authors_sc = essay_predict_sc[,c(-1,-2)]
authors_sc = convertAuthor(prediction.s[-1,],authors_sc,2,1 )
authors_sc = convertAuthor(prediction.s[-1,],authors_sc,3,2 )
authors_sc = convertAuthor(prediction.s[-1,],authors_sc,4,3 )

# graph results
grid.table(authors_un)
grid.table(authors_sc)

nrow(fed_data[fed_data$author=='Madison',])/nrow(fed_data[fed_data$author!='dispt',])
nrow(fed_data[fed_data$author=='Jay',])
nrow(fed_data[fed_data$author=='Hamilton',])
nrow(fed_data[fed_data$author=='HM',])

nrow(fed_data[fed_data$author!='dispt',])

percent_works = as.data.frame(table(fed_data[fed_data$author!='dispt',]$author)/nrow(fed_data[fed_data$author!='dispt',]))
percent_works[-1,]
colnames(percent_works) = c("Author","Freq")
percent_works$Freq = percent(percent_works$Freq)
grid.table(percent_works[-1,]) # frequency of authorship


