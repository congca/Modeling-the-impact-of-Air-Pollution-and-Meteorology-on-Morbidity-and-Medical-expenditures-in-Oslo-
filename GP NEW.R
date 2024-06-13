
mydata <-GP[,c(2,30,31,10,11,15,22)]
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


# head(df)
##oNLY LAG
df_with_lags <- cbind(GP[,c(2,4)],forlag)
dim(df_with_lags)

data=df_with_lags 

dim(data)
clean_data <- na.omit(data)
GPgov <- glm(clean_data$Government ~ ., family=quasipoisson(), clean_data)


options(max.print = 1000)
# Set max.print to a very high number
options(max.print = 99999)
summary(GPgov, na.action=na.exclude)
#  
 
##oNLY iNTERACTION
# ???????????????
df <- mydata[,c(2:7)]
colnames(df) <- var_names

# ??????????????????????????????
for (interaction in interactions) {
  interaction_name <- paste(interaction, collapse = ":")
  interaction_term <- paste(interaction, collapse = "*")
  df[[interaction_name]] <- with(df, eval(parse(text=interaction_term)))
}
dim(df)
iNTERACTION <- cbind(GP[,c(2,4)], df)
dim(iNTERACTION)

 

clean_data <- na.omit(iNTERACTION)
GPgov1 <- glm(clean_data$Government ~ ., family=quasipoisson(), clean_data)
summary(GPgov1, na.action=na.exclude)




