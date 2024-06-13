###### CONTINUE WITH 2ND PAPER CODE 2.R #####
Pollut = read.csv ("Oslo_pollution_2000_2018.csv",header=T, sep =",")
Pollu <- Pollut[,-1]
for(j in 1:ncol (Pollu)){
  Pollu [,j] <- as.numeric(as.factor(Pollu[,j]))
}

for(j in 1 : (ncol(Pollu))){
  b <- class (Pollu[,j])
  print(b)
}
Pollu[Pollu == 1] <- NA
n<-sum(is.na (Pollu))
n
### miss value
library (Amelia)
Pollu <- as.data.frame(Pollu)
missmap (Pollu, col = c ("white","lightblue"))
library(missForest) 
Pollu_comple <- missForest(Pollu,ntree = 100)
Pollu <- cbind (Pollu_comple$ximp,Pollut[,1])
Poll <- Pollu [c(2162:6909),]
##combine pollution and morbi data
Morbi_po <- cbind(Poll , newmorb[4748,])
names(Morbi_po) <- c("nox","pm10","pm2.5","date","dayNum","Dayrecord","patientNum","govern","deductible")
write.table(Morbi_po, "Morbi_po.csv",quote=F,sep=";",row.names=F)
###Now we get morbidity and cost,pollution data
################################################
#### Morbi_Po Cost (Oslo) 3         ############
################################################

Morbi_Po = read.csv("Morbi_po.csv",header = T,sep = ",")
ER = read.csv("OsloER.csv",header = T,sep = ",")
GP = read.csv("OsloGP.csv",header = T,sep = ",")

#### Done for now
Morb_po_nodata <- Morb_po_nodata[,c(1:4,8:10)]
result1 <-data
for (i in 1:ncol(data)){
  df <- ((data[,i]))/(max(data[,i]-min(data[,i])))
  result1[,i]<- as.data.frame(df)
}
head(result1)
Morb_po_nodata <-result1
##outlier
# sp=boxplot(Morb_po_nodata,boxwex=0.7)
# title("Oslo morbidity and pollution daily data")
# xi=1.1
# ##There are outlier in NOx
# sp1= boxplot(Morb_po_nodata,boxwex=0.7)
# sp1=boxplot(Morb_po_nodate,boxwew=0.7)
# morboPo <- as.data.frame(Morb_po_nodata) 
# ##define the outlier
# outliers <- boxplot(morboPo$nox,plot =FALSE) $out 
# data2 <- data2 [-which(morboPo$nox %in% outliers) ,]
# data2<- data2[complete.cases(data2),]
# #replot
# sp=boxplot(data2,boxwew=0.7)
# title("Oslo morbidity and pollution daily data")
# xi=1.1
### KNN
library(pROC)
library(kknn)
data2<-morboPo
set.seed(12345)
index <- sort(sample(nrow(data2), nrow(data2)*.7))
train_data <- data2[index,]
test_data <- data2[-index,]
wine_knn <- kknn( patientNum~ . ,
                  train_data,test_data, k=7,distance = 2)
pre_knn <- fitted(wine_knn)
predic <- as.numeric(pre_knn)
table(test_data$patientNum, pre_knn,dnn=c("Actual value","Predictive value"))
knn_mulroc <- multiclass.roc (test_data$patientNum, as.numeric(pre_knn))  
auc (knn_mulroc)
rsknn <- knn_mulroc[["rocs"]]
plot.roc(rsknn[[1]])
sapply(2:length(rsknn),function(i) lines.roc(rsknn [[i]] ,col=i))
### k means 
Clu <- morboPo
ClueData <- Clu
set.seed(12345)
library(ggplot2)
library(factoextra)
# fviz_nbclust(Clu, kmeans, method = "sihouette")
#                                             km<-kmeans(x=Clu,centers=5, iter.max=10,nstart=50)
#                                             fviz_cluster(km, Clu, ellipse.type="norm" )
#                                             km
#                                             km$cluster
#                                             km$size
#                                             km$centers
#                                             set.seed(12345)
library(randomForest)
Oslo.rf<-randomForest (ClueData$patientNum~.,data =ClueData,mtyr=3, importance=TRUE,proximity=TRUE)
print(Oslo.rf)
getTree(Oslo.rf, 1, labelVar=TRUE)
round(importance(Oslo.rf), 2)
VarImpPlot(Oslo.rf)
plot(Oslo.rf)
pred <- as.numeric(predict(Oslo.rf, CluData, type ="response" ))
rocmu <- multiclass.roc(CluData$patientNum, pred)
auc(rocmu)
### multi auc is 0.9794

rsmorb <- rocmu[["rocs"]]
plot.roc(rsmorb[[1]])
# sapply(2:length(rsmorb), function(i) lines.roc (rsmorb[[i]], col=i))
varImpPlot(Oslo.rf)
# plot(Oslo.rf)
# Oslo.mds= cmdscale(1-Oslo.rf$procximity,eig=TRUE)



ggVarImpPlot = function(importance,type="MSE",shiftFactor=10) {
  if ( require(ggplot2) == FALSE ) { stop("The ggplot2 package is required to execute this function.")}
  if ( require(grid)    == FALSE ) { stop("The grid package is required to execute this function.")}
  
  w = grep(type,colnames(importance))
  if ( length(w) > 0 ) {
    if ( type == "MSE") xlabel = "MSE: Percentage Increase"
    if ( type == "Purity") xlabel = "Increase in Node Purity"
    if ( type == "Gini") xlabel = "Gini: Mean Decrease"
    if ( type == "Accuracy") xlabel = "Accuracy: Mean Decrease"
    o = order(importance[,w])
    x = importance[o,w]
    var = rownames(importance)[o]
  } else {
    xlabel = colnames(importance)[1]
    o = order(importance[,1])
    x = importance[o,1]
    var = rownames(importance)[o]
  }
  
  
  n = length(x)
  y = (1:n)/(n+1)
  df = data.frame(x,y)
  
  p = ggplot(data=df,mapping=aes(x=x,y=y)) + geom_point(size=2) + xlim(0,max(x)) + ylim(0,1) + xlab(xlabel) + 
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    theme(plot.margin=unit(c(1,1,1,2),"cm"))
  for ( ii in 1:n ) {
    p = p + annotation_custom(grob = textGrob(label=var[ii]),ymin=y[ii],ymax=y[ii],xmin=-max(x)/shiftFactor,xmax=-max(x)/shiftFactor) +
      geom_hline(yintercept=y[ii],linetype="dashed",color="red",size=0.3)
  }
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
  invisible(list(p=p,gt=gt))
}

ggVarImpPlot(importance(Oslo.rf))



######

