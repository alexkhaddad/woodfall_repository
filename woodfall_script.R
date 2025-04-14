
library(vegan)

#data preparation
woodfall <- read.csv("woodfall.csv")
head(woodfall)
summary(woodfall)

#data exploration
woodfall.matrix <- woodfall[,-(1:8)]
woodfall.matrix[is.na(woodfall.matrix)]=0 #looks through the matrix to check if NA
rownames(woodfall.matrix) = woodfall$Log
plot(hclust(dist(woodfall.matrix))) #2 distinct cluster with one outlier
plot(hclust(dist(t(woodfall.matrix))))

plot(hclust(vegdist(woodfall.matrix))) #euclidean is informed by absolute abundance rather than relative     

#factor analysis
factanal(t(woodfall.matrix), factors=1)

#CCA
plot(cca(woodfall.matrix)$CA$v, cex=0)
text(cca(woodfall.matrix)$CA$v,labels=rownames(woodfall.matrix))

#principal coordinates analysis
cmdscale(vegdist(woodfall.matrix))
plot(cmdscale(vegdist(woodfall.matrix)), cex=sqrt(woodfall$Surface.Area..cm2.)/10)
plot(cmdscale(vegdist(woodfall.matrix)), cex=0)
text(cmdscale(vegdist(woodfall.matrix)), labels=rownames(woodfall.matrix))

woodfall[woodfall$Log %in% c("3","7","10","19"),]
