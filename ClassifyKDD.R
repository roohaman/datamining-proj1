
library(nnet)
#Load Data

FieldNames <-read.csv("Field Names.csv", header = FALSE,
                      stringsAsFactors = FALSE)

column.names <- FieldNames[,1]
Train<-read.csv(file = "KDDTrain+.csv")

#str(Train)
colnames(Train) <- column.names #Rename columns
Test<-read.csv(file="KDDTest+.csv")
colnames(Test) <- column.names #Rename columns
combi<-rbind(Train,Test)

#Data Standardization

norm = function(df) {
  names(df)[42] <- "outcome"
  df$outcome <- as.factor(df$outcome)
  df$outcome.response <- ifelse(df$outcome == 'normal',0,1)
  
  #Dealing with 3 Categorical Variables, 0/1, expanding ncols, replace into new.KDD.*
  service_<-as.data.frame(class.ind(df$service))
  protocol_type_<-as.data.frame(class.ind(df$protocol_type))
  flag_<-as.data.frame(class.ind(df$flag))
  new <- cbind(service_, protocol_type_, flag_)
  
  cat('Dummy features:',dim(new)[2],'\n')
  new.df = cbind(duration=df$duration, new, df[,5:41], outcome.response=df[,44])
  cat('New dim:',dim(new.df))
  return(new.df)
}

#Remove Missing Values
new.Train <- na.omit(Train)
new.Test<-na.omit(Test)

#Data Scaling
norm.Train<-norm(new.Train)
norm.Test<-norm(new.Test)

scale.Train <- scale(norm.Train[-1])
scale.Test<-scale(norm.Test)

#Impute missing values with median

scale.Train[is.na(scale.Train)] <- median(scale.Train, na.rm = TRUE)


prin_comp <- prcomp(scale.Train, scale. = T)

library(stats)
library(fpc)


#wssplot <- function(data, nc=15, seed=1234){
 # wss <- (nrow(data)-1)*sum(apply(scale.Train,2,var))
  #for (i in 2:nc){
   # set.seed(seed)
    #wss[i] <- sum(kmeans(scale.Train, centers=i)$withinss)}
  #plot(1:nc, wss, type="b", xlab="Number of Clusters",
   #    ylab="Within groups sum of squares")
#}

#is.na(scale.Train)

#table(is.nan(scale.Train))

#table(is.na(scale.Train)) 

#pdf("Number of Clusters.pdf")
#wssplot(scale.Train)
#dev.off()

fit.km<- kmeans(scale.Train,2,nstart=25)
fit.km$size
fit.km$centers


require(graphics)

# Plot the Clusters
pdf("clusters.pdf")
plot(scale.Train, col=fit.km$cluster)
points(fit.km$centers, col=1:3, pch = 8, cex = 2)
dev.off()

newTrain <- data.frame(scale.Train)
newTrain$cluster <- factor(fit.km$cluster)
centers <- as.data.frame(fit.km$centers)
#install.packages("ggplot2")
require(ggplot2)
pdf("Clustering of KDD data.pdf")
ggplot(data=newTrain, aes(x=outcome.response,y=cluster,color=cluster)) + geom_point()


dev.off()

size <- fit.km$size

meancol<-aggregate(scale.Train,by=list(cluster=fit.km$cluster),mean)

cross <- table(newTrain$cluster, fit.km$cluster)
cross

install.packages("flexclust")

library(flexclust)
randIndex(cross)

fit_centers <- fitted(fit.km)

residue <- scale.Train - fitted(fit.km)

mydata <- data.frame(scale.Train, fit.km$cluster)
abc<-write.table(mydata, file="clustered_observations.csv", sep=",", row.names=F, col.names=T, quote=F)


str(newTrain)



#ct.km<-table(scale.Train,fit.km$cluster)

library(NbClust)
set.seed(1234)
devAskNewPage(ask=TRUE)
nc<- NbClust(scale.Train,min.nc = 2,max.nc = 5,method="Kmeans")
table(nc$Best.n[1,])
#PCA

prin_comp <- prcomp(scale.Train, scale. = T)

