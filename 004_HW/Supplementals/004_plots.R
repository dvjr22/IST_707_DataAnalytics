#HW 4 plots

dev.off(dev.list()["RStudioGD"]) # clear plots


# scale vs unscale
grid.arrange(plotEssayHeatMap, plotHeatEssayRaw, ncol = 2)
grid.arrange(authorHeatmap, plotheatAuthorRaw, ncol = 2)

#scaled
fviz_nbclust(unlabeledScale, kmeans, method="wss" )

clusplot(unlabeledScale, model_s.k$cluster, main = "Scaled Cluster", color=TRUE, shade=TRUE, labels=2, lines=0) # kmeans

plot(model_s.h) #HAC
cluster_s.h = cutree(model_s.h, k=4) #cluster assignments vector
rect.hclust(model_s.h, k=4, border = 'red')


#unscaled
fviz_nbclust(unlabeledUnscale, kmeans, method ="wss") # 4 clusters

clusplot(unlabeledUnscale, model_u.k$cluster, main = "Raw Cluster", color=TRUE, shade=TRUE, labels=2, lines=0) # kmeans

plot(model_u.h) # HAC
cluster_u.h = cutree(model_u.h, k=4) #cluster assignments
rect.hclust(model_u.h, k=4, border = 'red')


#Analysis

dev.off(dev.list()["RStudioGD"]) # clear plots

grid.table(prediction.u[-1,]) # table unscaled
grid.table(prediction.s[-1,]) # table scaled

grid.table(percent_un) # % correct
grid.table(percent_sc) # % correct

# graph results
grid.table(authors_un)
grid.table(authors_sc)

grid.table(percent_works[-1,]) # frequency of authorship