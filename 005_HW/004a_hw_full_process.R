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


Fed_v1 = read.csv2("fed_papers_scaled_v1.csv", sep = ',', stringsAsFactors = FALSE, numerals = 'no.loss')
Fed_v2 = read.csv2("fed_papers_scaled_v2.csv", sep = ',', stringsAsFactors = FALSE, numerals = 'no.loss')

#Fed_v1 = Fed_v2 # so I don't need to change the variable all the way down

naInData(Fed_v1)# no NA
summary(Fed_v1)
str(Fed_v1)

fed_data_v1 = toNumeric(Fed_v1, c(2:ncol(Fed_v1))) # decimals to numeric
fed_data_v1 = toFactor(fed_data_v1, c(1)) # author and file names to factor

#ew = fed_data_v1$essay_writer
#fed_data_v1 = scaleDFCol(fed_data_v1, c(2:ncol(Fed_v1)), 1) # scale columns
#fed_data_v1$V1 = ew

summary(fed_data_v1)
str(fed_data_v1)


unlabeledScaleV2 = fed_data_v1[,-1]

# suggested number of clusters
fviz_nbclust(unlabeledScaleV2, kmeans, method="wss" )

# k means
model_s.kV2 = kmeans(unlabeledScaleV2, 4)
plotcluster(unlabeledScaleV2, model_s.kV2$cluster)
clusplot(unlabeledScaleV2, model_s.kV2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
cluster_s.kV2 = model_s.kV2$cluster # get cluster assignment vector

# hierarchical
distanceV2 = dist(unlabeledScaleV2, method = 'euclidean')
model_s.hV2 = hclust(distanceV2, method = 'ward.D2')
plot(model_s.hV2)
cluster_s.hV2 = cutree(model_s.hV2, k=4) #cluster assignments vector
rect.hclust(model_s.hV2, k=4, border = 'red')


#em
model_s.eV2 = em.cluster.R(unlabeledScaleV2, K=4, S=c(rep(TRUE, ncol(unlabeledScaleV2))))

cluster_s.eV2 = model_s.eV2["mapClassification"] # cluster assignments vector

cluster_scaledV2 = data.frame(fed_data_v1[,1], cluster_s.kV2, cluster_s.hV2 ,cluster_s.eV2 ) #kmeans assignments
cluster_scaledV2$cluster_s.eV2 = cluster_s.eV2+1 # cluster classifications match other results: range 0-3 -> 1-4



# assign mode to an author

prediction.sV2 = aggregate(cluster_scaledV2[,-1], by=list(cluster_scaledV2$fed_data_v1...1.), FUN=Mode)

clusterNames_SV2 = c("Author","cluster_k","cluster_h","cluster_e")

colnames(prediction.sV2) = clusterNames_SV2

grid.table(prediction.sV2[-1,])

essay_predict_scV2 = cluster_scaledV2[cluster_scaledV2$fed_data_v1...1.=='dispt',]

authors_scV2 = essay_predict_scV2[,-1]
authors_scV2 = convertAuthor(prediction.sV2[-1,],authors_scV2,2,1 )
authors_scV2 = convertAuthor(prediction.sV2[-1,],authors_scV2,3,2 )
authors_scV2 = convertAuthor(prediction.sV2[-1,],authors_scV2,4,3 )

grid.table(authors_scV2)

