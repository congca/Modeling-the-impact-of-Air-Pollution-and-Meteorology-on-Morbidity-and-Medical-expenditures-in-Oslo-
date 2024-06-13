Morbi_Po = read.csv("Morbi_po.csv",header = T,sep = ",")
ER = read.csv("OsloER.csv",header = T,sep = ";")
GP = read.csv("OsloGP.csv",header = T,sep = ";")
for(j in 1:ncol (ER)){
  ER [,j] <- as.numeric(as.factor(ER[,j]))
}

for(j in 1 : (ncol(ER))){
  b <- class (ER[,j])
  print(b)
}
for(j in 1:ncol (GP)){
  GP [,j] <- as.numeric(as.factor(GP[,j]))
}

for(j in 1 : (ncol(GP))){
  b <- class (GP[,j])
  print(b)
}
#FOR nox
fit3 <- lm(GP$individual~.,data =GP[,c(2,5,6:30)])
fit4 <- lm(ER$individual~.,data =ER[,c(2,5,6:30)])
fit5 <- lm(GP$Government~.,data =GP[,c(2,4,6:30)])
fit6 <- lm(ER$Government~.,data =ER[,c(2,4,6:30)])
library("jtools")

png('Plot for the impact of NOx and weather factors on different outcomes due to CRD.png',
    height = 45,
    width = 45,
    units = 'cm',
    res = 300)
plot.new()
custom_colors <- c("#93cc82","#e8c559")
title("Plot for the impact of NOx and weather factors on different outcomes due to CRD")
plot_summs( fit3,fit4,fit5, fit6,robust = list(FALSE,  "HC3","HC4", "HC5", "HC6"),
           model.names = c(
                           "The individuals??? medical expenditure at GPs",
                           "The individuals??? medical expenditure at ERs",
                           "The government medical expenditure at GPs",
                           "The government medical expenditure at ERs"))
dev.off()

GPWINTER <-    GP[which(GP$Winter %in% "2"),]
ERWINTER <-    ER[which(ER$Winter %in% "2"),]
fit33 <- lm(GPWINTER$individual~.,data =GPWINTER[,c(5,10:25,27:30)])
fit44 <- lm(ERWINTER$individual~.,data =ERWINTER[,c(5,10:25,27:30)])
fit55 <- lm(GPWINTER$Government~.,data =GPWINTER[,c(4,10:25,27:30)])
fit66 <- lm(ERWINTER$Government~.,data =ERWINTER[,c(4,10:25,27:30)])
library("jtools")

png('Winter subset Plot for the impact of NOx and weather factors on different outcomes due to CRD.png',
    height = 45,
    width = 45,
    units = 'cm',
    res = 300)
plot.new()
custom_colors <- c("#93cc82","#e8c559")
title("Plot for the impact of NOx and weather factors on different outcomes due to CRD")
plot_summs( fit33,fit44,fit55, fit66,robust = list(FALSE, "HC1","HC2",  "HC5", "HC6"),
           model.names = c(
                           "The individuals??? medical expenditure at GPs-Winter subset",
                           "The individuals??? medical expenditure at ERs-Winter subset",
                           "The government medical expenditure at GPs-Winter subset",
                           "The government medical expenditure at ERs-Winter subset"))
dev.off()
#FOR PM2.5
fit3 <- lm(GP$individual~.,data =GP[,c(2,5,6:29,31)])
fit4 <- lm(ER$individual~.,data =ER[,c(2,5,6:29,31)])
fit5 <- lm(GP$Government~.,data =GP[,c(2,4,6:29,31)])
fit6 <- lm(ER$Government~.,data =ER[,c(2,4,6:29,31)])

library("jtools")

png('The impact of PM2.5 and weather factors on different outcomes due to CRD.png',
    height = 45,
    width = 45,
    units = 'cm',
    res = 300)
plot.new()
custom_colors <- c("#93cc82","#e8c559")
title("Plot for the impact of NOx and weather factors on different outcomes due to CRD")
plot_summs( fit3,fit4,fit5, fit6,robust = list(FALSE, "HC1","HC2", "HC3","HC4", "HC5", "HC6"),
           model.names = c(
                           "The individuals??? medical expenditure at GPs",
                           "The individuals??? medical expenditure at ERs",
                           "The government medical expenditure at GPs",
                           "The government medical expenditure at ERs"))
dev.off()


GPWINTER <-    GP[which(GP$Winter %in% "2"),]
ERWINTER <-    ER[which(ER$Winter %in% "2"),]
fit33 <- lm(GPWINTER$individual~.,data =GPWINTER[,c(5,10:25,27:29,31)])
fit44 <- lm(ERWINTER$individual~.,data =ERWINTER[,c(5,10:25,27:29,31)])
fit55 <- lm(GPWINTER$Government~.,data =GPWINTER[,c(4,10:25,27:29,31)])
fit66 <- lm(ERWINTER$Government~.,data =ERWINTER[,c(4,10:25,27:29,31)])
png('Winter subset Plot for the impact of pm25 and weather factors on different outcomes due to CRD.png',
    height = 45,
    width = 45,
    units = 'cm',
    res = 300)
plot.new()
custom_colors <- c("#93cc82","#e8c559")
title("Plot for the impact of pm25 and weather factors on different outcomes due to CRD")
plot_summs( fit33,fit44,fit55, fit66,robust = list(FALSE, "HC1","HC2",  "HC5", "HC6"),
            model.names = c(
              "The individuals??? medical expenditure at GPs-Winter subset",
              "The individuals??? medical expenditure at ERs-Winter subset",
              "The government medical expenditure at GPs-Winter subset",
              "The government medical expenditure at ERs-Winter subset"))
dev.off()
##done



