
library(vegan)

#data preparation
woodfall <- read.csv("data/woodfall.csv")
head(woodfall)
summary(woodfall)

#data exploration
woodfall.matrix <- woodfall[,-(1:8)]
woodfall.matrix[is.na(woodfall.matrix)]=0 #looks through the matrix to check if NA
rownames(woodfall.matrix) = woodfall$Log
plot(hclust(dist(woodfall.matrix))) #2 distinct cluster with one outlier
jpeg(filename="woodfall_hca.jpg", height = 480, width = 480) #turns plots into jpegs
par(cex=1)
par(mfrow=c(2,1)) #multiple plots
plot(hclust(dist(t(woodfall.matrix))))
plot(hclust(dist(woodfall.matrix)))
dev.off()


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

#testing Xylophaga influence
cmdscale(vegdist(t(woodfall.matrix)))
k <- cmdscale(vegdist(t(woodfall.matrix)))
plot(k)
points(k[1:3,], pch=19, col="brown") #xylophaga does not have much influence
which(k[,1]>0.4) #polychaetes 
which(k[,2]>0.4) #gastropods


#fishers alpha
library(vegan)
alpha <- array()
for (i in 1:ncol(woodfall.matrix)) {
  n <- woodfall.matrix[,i]
  alpha[i] <- fisher.alpha(n[n > 10])
}
alpha[alpha > 10] <- NA
alpha <- log(alpha)

logs_data <- woodfall[,1:6]
succession <- as.numeric(logs_data[,5])
weight <- unlist(logs_data[,4])


#generalized linear mixed model
install.packages("lmerTest")
library(lmerTest)
install.packages("MuMIn")
library(MuMIn)

glmm <- lmer(alpha ~ )
summary(glmm)
ranef(glmm)
rand(glmm)
r.squaredGLMM(glmm)