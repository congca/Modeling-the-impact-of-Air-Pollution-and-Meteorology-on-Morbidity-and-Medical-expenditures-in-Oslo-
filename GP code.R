
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
df <- mydata[,c(2:7)]
colnames(df) <- var_names

# ??????????????????????????????
for (interaction in interactions) {
  interaction_name <- paste(interaction, collapse = ":")
  interaction_term <- paste(interaction, collapse = "*")
  df[[interaction_name]] <- with(df, eval(parse(text=interaction_term)))
}
dim(df)
# ???????????????

df_with_lags <- cbind(GP[,c(2,4)],forlag,data)

data=df_with_lags 

 
gpclean_data <- na.omit(data)
gpgov <- glm(gpclean_data$Government ~ ., family=quasipoisson(), gpclean_data)
 

options(max.print = 1000)
# Set max.print to a very high number
options(max.print = 99999)
summary(gpgov, na.action=na.exclude)


# Get the summary of the model
model_summary1 <- summary(gpgov) 
# Extract the coefficients and their statistics
gov1 <- model_summary1$coefficients
# Set a significance level, for example, 0.05
significance_level <- 0.05
# Filter the coefficients to show only those with p-values less than the significance level
significant_gov1 <- gov1[gov1[, 4] < significance_level, ]
# Print the significant coefficients
print(significant_gov1)
 

