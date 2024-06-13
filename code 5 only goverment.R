
fit5 <- lm(GP$Government~.,data =GP[,c(4,30,31)])
fit6 <- lm(ER$Government~.,data =ER[,c(4,30,31)])
summary(fit5)
summary(fit6)
library("jtools")

png('Plot for the impact of air pollution on medical expenditures due to CRD.png',
    height = 45,
    width = 45,
    units = 'cm',
    res = 300)
plot.new()
custom_colors <- c("#93cc82","#e8c559")
title("Plot for the impact of air pollution  on medical expenditures due to CRD")
plot_summs( fit5, fit6,robust = list(FALSE,  "HC3", "HC6"),
            model.names = c(
              "The impact of air pollution on medical expenditure at GPs",
              "The impact of air pollution on medical expenditure at ERs"))
dev.off()




GPWINTER <-    GP[which(GP$Winter %in% "2"),]
ERWINTER <-    ER[which(ER$Winter %in% "2"),]
fit55 <- lm(GPWINTER$Government~.,data =GPWINTER[,c(4,30,31)])
fit66 <- lm(ERWINTER$Government~.,data =ERWINTER[,c(4,30,31)])
library("jtools")
png('Winter subset Plot for the impact of air pollution on medical expenditures due to CRD.png',
    height = 45,
    width = 45,
    units = 'cm',
    res = 300)
plot.new()
custom_colors <- c("#93cc82","#e8c559")
title("Winter subset Plot for the impact of air pollution on medical expenditures due to CRD")
plot_summs( fit55, fit66,robust = list(FALSE, "HC1"," HC6"),
            model.names = c(
            
              "The impact of air pollution on medical expenditure at GPs-Winter subset",
              "The impact of air pollution on medical expenditure at ERs-Winter subset"))
dev.off()

summary(fit5)
summary(fit6)
summary(fit55)
summary(fit66)



png('The impact of air pollution on medical expenditures.png',
    height = 45,
    width = 45,
    units = 'cm',
    res = 300)
plot.new()
custom_colors <- c("#93cc82","#e8c559")
plot_summs( fit5, fit6, fit55, fit66,robust = list(FALSE, "HC1","HC2",  "HC5", "HC6"),
            model.names = c(
              "The impact of air pollution on medical expenditure at GPs",
              "The impact of air pollution on medical expenditure at ERs",
              "The impact of air pollution on medical expenditure at GPs-Winter subset",
              "The impact of air pollution on medical expenditure at ERs-Winter subset"))
dev.off()



