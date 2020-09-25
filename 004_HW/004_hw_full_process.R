scale_fed_data_v2


unlabeledScaleV2 = newFed[,-1]

unlabeledScaleV2 = scale_fed_data_v2[,c(-2:-1)]

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
model_s.eV2 = em.cluster.R(unlabeledScaleV2, K=4, S=c(rep(TRUE, 163)))

slotNames(model_s.e)
cluster_s.eV2 = model_s.eV2["mapClassification"] # cluster assignments vector

cluster_scaledV2 = data.frame(unlabeledScaleV2[,1], cluster_s.kV2, cluster_s.hV2 ,cluster_s.eV2 ) #kmeans assignments
cluster_scaledV2$cluster_s.eV2 = cluster_s.eV2+1 # cluster classifications match other results: range 0-3 -> 1-4



# assign mode to an author

prediction.sV2 = aggregate(cluster_scaledV2[,-1], by=list(cluster_scaledV2$es), FUN=Mode)

clusterNames_SV2 = c("Author","cluster_k","cluster_h","cluster_e")

colnames(prediction.sV2) = clusterNames_SV2

grid.table(prediction.sV2[-1,])

essay_predict_scV2 = cluster_scaledV2[cluster_scaledV2$author=='dispt',]

authors_scV2 = essay_predict_scV2[,c(-1,-2)]
authors_scV2 = convertAuthor(prediction.sV2[-1,],authors_scV2,2,1 )
authors_scV2 = convertAuthor(prediction.sV2[-1,],authors_scV2,3,2 )
authors_scV2 = convertAuthor(prediction.sV2[-1,],authors_scV2,4,3 )

grid.table(authors_scV2)

