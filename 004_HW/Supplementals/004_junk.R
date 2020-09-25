testA = melt(fed_dataA)
testA1 = ddply(testA, .(variable), transform, rescale=rescale(value))
ggplot(testA, aes(author, variable, fill = value)) + geom_tile()+ scale_fill_gradient(low = "yellow", high = "red")
ggplot(testA1, aes(author, variable, fill = rescale)) + geom_tile() + scale_fill_gradient(low = "yellow", high = "red")



dataset = fed_data
colsToScale = c(3:72)
colsNotScale = c(1,2)

testDFv1 = scaleDF(testDF, c(3:72), c(1,2))


# got this method of applying min/max from a friend 
# it works just like:
# min_max_ageT = (NewSchoolsSec$Middling - min(NewSchoolsSec$Middling , na.rm = TRUE))
# / (max(NewSchoolsSec$Middling , na.rm = TRUE) - min(NewSchoolsSec$Middling , na.rm = TRUE))
max = apply(NewSchoolsSec[,3:7] , 2 , max)
min = apply(NewSchoolsSec[,3:7], 2 , min)
scaledSchool = as.data.frame(cbind(NewSchoolsSec$School, NewSchoolsSec$VeryAhead, 
                                   scale(NewSchoolsSec[,3:7], center = min, scale = max - min)))

model_sHAC = pvclust(unlabeledScale, method.hclust = 'ward.D', method.dist = 'euclidean')
plot(model_sHAC)

pvrect(model_sHAC, alpha=.95)
groups1 = cutree(model_sHAC, k=4) # vector of clusters


scale(dataset[1,colsToScale])
test = cbind(dataset[1,colsToScale])
scale(test)
scale(dataset[,4])

test = t(dataset[1,colsToScale])
max = max(test)
min = min(test)
newRow = scale(test, center = min, scale = max - min)
newRow = t(test)

scaledRows = rbind(scaledRows, newRow)


fed_dataA = fed_data[,-2] # removing doc for aggregation
authorAvg = aggregate(fed_dataA, by=list(fed_data$author), FUN=mean) # mean



test = daisy(unlabeledScale)
atest = test^2
toPlot = silhouette(model_s$cluster,atest)
plot(toPlot)

plot(model_em[, c("levels", "proba")],
     col = "blue", xlab = "Dimension", ylab = "Log-likelihood")


model_em["N_levels"]

summary(cluster_unscaled)
str(cluster_unscaled)

Mode(cluster_unscaled$cluster_u.h)


table(cluster_unscaled[cluster_unscaled$author == 'Hamilton',]$cluster_u.k)
max(table(cluster_unscaled[cluster_unscaled$author == 'Hamilton',]$cluster_u.k))/sum(table(cluster_unscaled[cluster_unscaled$author == 'Hamilton',]$cluster_u.k))

max(table(cluster_unscaled[cluster_unscaled$author == 'Hamilton',]$cluster_u.k))/sum(table(cluster_unscaled[cluster_unscaled$author == 'Hamilton',]$cluster_u.k))
dp = prediction.s
ds = cluster_scaled

