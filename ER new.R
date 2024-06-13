
mydata <-ER[,c(2,30,31,10,11,15,22)]
for(j in 1:ncol (mydata)){
  mydata[,j] <- as.numeric(as.factor(mydata[,j]))
}

for(j in 1 : (ncol(mydata))){
  b <- class (mydata[,j])
  print(b)
}

# ?????? df ??????????????????????????????????????????
if (!is.data.frame(mydata)) {
  mydata <- as.data.frame(mydata)
}

var_names <- c("NOx", "PM25", "MiniTemp", "MeanTemp", "MaxTemp", "MeanReHumid")
colnames(mydata[,-1]) <- c("NOx", "PM25", "MiniTemp", "MeanTemp", "MaxTemp", "MeanReHumid")
print(colnames(mydata))

# Set the maximum number of lags to add
lag_count <- 30
forlag <- as.data.frame(mydata[,-1])
# Loop through each variable to add six lagged
library(dplyr)

for (col_name in colnames(forlag)) {
  for (lag in 1:lag_count) {
    new_col_name <- paste0(col_name, "_lag_", lag)
    forlag <- forlag %>% mutate(!!new_col_name := lag(forlag[[col_name]], lag))
  }
}
for(j in 1:ncol (forlag)){
  forlag [,j] <- as.numeric(as.factor(forlag[,j]))
}


# 
# # ?????????????????????????????????
# vars <- list(mydata$nox, mydata$pm2.5, mydata$Minimum.air.temperature..24.h., mydata$Mean.air.temperature..24.h., 
#              mydata$Maximum.air.temperature..24.h.,mydata$Mean.relative.humidity..24.h.)
# var_names <- c("NOx", "PM25", "MiniTemp", "MeanTemp", "MaxTemp", "MeanReHumid")
# 
# # ????????????????????????????????????
# all_interactions <- function(vars, var_names) {
#   n <- length(vars)
#   interactions <- list()
#   
#   # ???????????????????????????
#   for (i in 2:n) {
#     combs <- combn(var_names, i)
#     for (j in 1:ncol(combs)) {
#       interactions[[length(interactions) + 1]] <- combs[, j]
#     }
#   }
#   
#   return(interactions)
# }
# 
# # ?????????????????????
# interactions <- all_interactions(vars, var_names)
# 
# # ?????????????????????
# for (interaction in interactions) {
#   cat("Interaction:", paste(interaction, collapse = ":"), "\n")
# }
# 
# # ?????????????????????????????????????????????
# # ???????????????
# df <- mydata[,c(2:7)]
# colnames(df) <- var_names
# 
# # ??????????????????????????????
# for (interaction in interactions) {
#   interaction_name <- paste(interaction, collapse = ":")
#   interaction_term <- paste(interaction, collapse = "*")
#   data[[interaction_name]] <- with(df, eval(parse(text=interaction_term)))
# }
# 
# # ???????????????
# head(df)
##oNLY LAG
df_with_lags <- cbind(ER[,c(2,4)],forlag)
dim(df_with_lags)

data=df_with_lags 

dim(data)
clean_data <- na.omit(data)
ERgov <- glm(clean_data$Government ~ ., family=quasipoisson(), clean_data)
summary(ERgov, na.action=na.exclude)
# 
options(max.print = 1000)
# Set max.print to a very high number
options(max.print = 99999)
summary(ERgov, na.action=na.exclude)
#  
 

##oNLY iNTERACTION
mydata <-ER[,c(2,30,31,10,11,15,22)]
df <- mydata[,c(2:7)]
colnames(df) <- var_names

# ??????????????????????????????
for (interaction in interactions) {
  interaction_name <- paste(interaction, collapse = ":")
  interaction_term <- paste(interaction, collapse = "*")
  df[[interaction_name]] <- with(df, eval(parse(text=interaction_term)))
}
dim(df)
iNTERACTION <- cbind(ER[,c(2,4)], df)
dim(iNTERACTION)



clean_data <- na.omit(iNTERACTION)
GPgov1 <- glm(clean_data$Government ~ ., family=quasipoisson(), clean_data)
summary(GPgov1, na.action=na.exclude)

 