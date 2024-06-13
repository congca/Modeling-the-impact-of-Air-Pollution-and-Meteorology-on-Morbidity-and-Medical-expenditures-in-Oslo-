
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
for (col_name in colnames(forlag)) {
  for (lag in 1:lag_count) {
    new_col_name <- paste0(col_name, "_lag_", lag)
    forlag <- forlag %>% mutate(!!new_col_name := lag(forlag[[col_name]], lag))
  }
}
for(j in 1:ncol (forlag)){
  forlag [,j] <- as.numeric(as.factor(forlag[,j]))
}


 
 
# ?????????????????????????????????
vars <- list(mydata$nox, mydata$pm2.5, mydata$Minimum.air.temperature..24.h., mydata$Mean.air.temperature..24.h., 
             mydata$Maximum.air.temperature..24.h.,mydata$Mean.relative.humidity..24.h.)
var_names <- c("NOx", "PM25", "MiniTemp", "MeanTemp", "MaxTemp", "MeanReHumid")

# ????????????????????????????????????
all_interactions <- function(vars, var_names) {
  n <- length(vars)
  interactions <- list()
  
  # ???????????????????????????
  for (i in 2:n) {
    combs <- combn(var_names, i)
    for (j in 1:ncol(combs)) {
      interactions[[length(interactions) + 1]] <- combs[, j]
    }
  }
  
  return(interactions)
}

# ?????????????????????
interactions <- all_interactions(vars, var_names)

# ?????????????????????
for (interaction in interactions) {
  cat("Interaction:", paste(interaction, collapse = ":"), "\n")
}

# ?????????????????????????????????????????????
# ???????????????
data <- mydata[,c(2:7)]
colnames(data) <- var_names

# ??????????????????????????????
for (interaction in interactions) {
  interaction_name <- paste(interaction, collapse = ":")
  interaction_term <- paste(interaction, collapse = "*")
  data[[interaction_name]] <- with(data, eval(parse(text=interaction_term)))
}

# ???????????????
head(data)

df_with_lags <- cbind(ER[,c(2,4)],forlag,data)

dim(df_with_lags)

data=df_with_lags 
# data[data == "NA" | data == "N/A" | data == "NULL"] <- NA
# for(j in 1:ncol (data)){
#   data[,j] <- as.numeric(as.factor(data[,j]))
# }
# 
# for(j in 1 : (ncol(data))){
#   b <- class (data[,j])
#   print(b)
# }
dim(data)
clean_data <- na.omit(data)
ERgov <- glm(clean_data$Government ~ ., family=quasipoisson(), clean_data)
summary(ERgov, na.action=na.exclude)
# 
# dataINDIvia=df_with_lags [,-c(1,3,4)]
# dataINDIvia[dataINDIvia == "NA" | dataINDIvia == "N/A" | dataINDIvia == "NULL"] <- NA
# for(j in 1:ncol (dataINDIvia)){
#   dataINDIvia[,j] <- as.numeric(as.factor(dataINDIvia[,j]))
# }
# 
# for(j in 1 : (ncol(dataINDIvia))){
#   b <- class (dataINDIvia[,j])
#   print(b)
# }
# clean_data <- na.omit(dataINDIvia)
# DNMINDIvia <- glm( clean_data$individual ~ ., 
#                    family=quasipoisson(), clean_data, control = glm.control(maxit = 100))
# Increase the maximum number of rows printed to 1000
options(max.print = 1000)
# Set max.print to a very high number
options(max.print = 99999)
summary(ERgov, na.action=na.exclude)
#  
# 
# # Get the summary of the model
# model_summary <- summary(DNMINDIvia)
# # Extract the coefficients and their statistics
# coefficients <- model_summary$coefficients
# # Set a significance level, for example, 0.05
# significance_level <- 0.05
# # Filter the coefficients to show only those with p-values less than the significance level
# significant_coefficients <- coefficients[coefficients[, 4] < significance_level, ]
# # Print the significant coefficients
#  


# Get the summary of the model
model_summary <- summary(ERgov) 
# Extract the coefficients and their statistics
gov <- model_summary$coefficients
# Set a significance level, for example, 0.05
significance_level <- 0.05
# Filter the coefficients to show only those with p-values less than the significance level
significant_gov <- gov[gov[, 4] < significance_level, ]
# Print the significant coefficients
print(significant_gov)
print(significant_coefficients)

 