#factorTrain = toFactor(trainSet, c(2:785))
#factorTrain.v2 = factorTrain[ , sapply(factorTrain, nlevels) > 1]
#svn.trainSet = toInteger(factorTrain.v2,c(2:608))

#model.svm.v1 = svm(label ~. , data = factorTrain.v2)
#model.svm.v2 = svm(label ~. , data = svn.trainSet)

#factorTest = toFactor(testSet, c(2:785))
#factorTest.v2 = factorTest[ , sapply(factorTest, nlevels) > 1]
#svn.testSet = toInteger(factorTest.v2, c(2:608))


#pred.svm.v2 = predict(model.svm.v2, svn.testSet, type = 'class')