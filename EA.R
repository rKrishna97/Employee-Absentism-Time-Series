rm(list = ls())

# Set working directory
setwd("C:/Users/Krishna/Google Drive/Data Science/Project/Edwisor/Project 1")


# Importing required libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50",
      "dummies", "e1071", "Information", "MASS", "rpart", "gbm", "ROSE", 'sampling',
      'DataCombine', 'inTrees','readxl', 'tseries', 'forecast', 'qpcR', 'lmtest')
lapply(x, require, character.only=TRUE)


# Importing the data
data_original = read_xls("Absenteeism_at_work_Project.xls")
data_original = as.data.frame(data_original)
names(data_original) = gsub(" ", "_", names(data_original))
data = data_original



# Dimension/Shape of the data
dim(data)

# Structure of the dataset
str(data)

# Column names of the data
colnames(data)


# Storing Continous column names and Categorical column names
#which(colnames(data) == "Work_load_Average/day")
colnames(data)[10] = "Work_load_Average_per_day"
continuous_col = c("Transportation_expense", "Distance_from_Residence_to_Work", "Service_time",
                   "Age", "Work_load_Average_per_day", "Hit_target", "Weight", "Height", "Pet", "Son", "Body_mass_index",
                   "Absenteeism_time_in_hours")
categorical_col = c("ID","Reason_for_absence", "Month_of_absence", "Day_of_the_week", "Seasons",
                    "Disciplinary_failure", "Education", "Social_drinker", "Social_smoker")


# Missing Value Analysis --------------------------------------------------

# Creating dataframe for percentage of Missing Value
missing_val = data.frame(apply(data, 2, function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
colnames(missing_val)[1] = "Sum of missing Value"
missing_val$'Percentage of missing value' = (missing_val$`Sum of missing Value`/nrow(data)) * 100
missing_val = missing_val[,c(2,1,3)]
missing_val = missing_val[order(-missing_val$`Sum of missing Value`),]


ggplot(data,aes_string(y=data$Absenteeism_time_in_hours,x=as.factor(data$Reason_for_absence)))+geom_boxplot()+xlab('Reason_for_absence')+ylab('Absenteeism_time_in_hours')

# Imputing missing values in categorical data manually

# Missing value in Reason for absence will be replaces with Reason for absence with least absence hour. i.e Reason 27 as it is less than 10
data$Reason_for_absence[is.na(data$Reason_for_absence)] = 27

# We have zero category in Reason of absence, So it will be replaced with 26 i.e unjustified absence
data$Reason_for_absence[data$Reason_for_absence==0] = 26

# Replacing na value in Month of absence with 10
data$Month_of_absence[is.na(data$Month_of_absence)] = 10

# Replacing na value in Disciplinary Failure with 0
data$Disciplinary_failure[is.na(data$Disciplinary_failure)] = 0

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#data$ID[is.na(data$Education)]

for(i in c(10, 11,14,24,34)){
  data$Education[is.na(data$Education) & data$ID==i] = mode(data$Education[data$ID!=i])
}

#data$ID[is.na(data$Social_drinker)]
for(i in c(10,14,17)){
  data$Social_drinker[is.na(data$Social_drinker) & data$ID==i] = mode(data$Social_drinker[data$ID != i])
}


#data$ID[is.na(data$Social_smoker)]
for(i in c(34, 1, 11, 15)){
  data$Social_smoker[is.na(data$Social_smoker) & data$ID==i] = mode(data$Social_smoker[data$ID!=i])
}

# Imputing na values in Target variable manually

#unique(data$Reason_for_absence[is.na(data$Absenteeism_time_in_hours)])
for(i in c(23, 14, 10, 22, 26, 6, 28, 11, 13)){
  data$Absenteeism_time_in_hours[is.na(data$Absenteeism_time_in_hours) & data$Reason_for_absence==i] = mode(data$Absenteeism_time_in_hours[data$Reason_for_absence!=i])
}

# Imputing continuous variables with KNN
data = knnImputation(data, k=3)

str(data)


# Converting from numeric class to interger class
for(i in colnames(data)){
  data[,i] = as.integer(data[,i])
}
# Converting from integer to factor for categorical data
for(i in categorical_col){
  data[,i] = as.factor(as.character(data[,i]))
}

str(data)

write.csv(data, file = "EA_MVA.csv")


# Outlier Analysis --------------------------------------------------------

# Boxplot for continuous variable
for (i in 1:length(continuous_col))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (continuous_col[i]), x = "Absenteeism_time_in_hours"), data = subset(data))+
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=continuous_col[i],x="Absenteeism_time_in_hours")+
            ggtitle(paste("Box plot of absenteeism for",continuous_col[i])))
}

gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)

outlier_col = c('Transportation_expense','Service_time','Age','Work_load_Average_per_day','Hit_target','Height','Absenteeism_time_in_hours')

for (i in outlier_col){
  q = quantile(data[,i],c(0.25,0.75))
  iqr1 = q[2]-q[1]
  min1 = q[1]-1.5*iqr1
  max1 = q[2]+1.5*iqr1
  data[,i][data[,i]<min1] = min1
  data[,i][data[,i]>max1] = max1
}


# EDA ---------------------------------------------------------------------

Reasons = aggregate(data$Absenteeism_time_in_hours, by=list(Reason_of_absence=data$Reason_for_absence), FUN=sum)
print(Reasons)

Reasons$Absence = (Reasons$x/sum(data$Absenteeism_time_in_hours))*100

Reasons = Reasons[order(Reasons$Absence),]

Reasons

barplot(Reasons$Absence,names.arg=Reasons$Reason_of_absence,xlab="Reason for absence",ylab="Absence",col="blue")


# Feature Selection -------------------------------------------------------

# Correlation plot for continuous variables because dependent variable is continuous

corrgram(data[,continuous_col], order = F, upper.panel = panel.pie, text.panel = panel.txt,
         main = "Correlation Plot")

for(i in continuous_col[-10]){
  print(i)
  print(cor(data$Absenteeism_time_in_hours, data[,i], method = "pearson"))
  cat("\n")
}

# Anova test for categorical variable

for(i in categorical_col){
  print(i)
  cat("\n")
  print(summary(aov(Absenteeism_time_in_hours~data[,i], data = data)))
  cat("\n")
}
 

data_fsel = subset(data, select = -c(Weight))
data_fsel = as.data.frame(data_fsel)
write.csv(data_fsel, file = "EA_feature_Selection.csv")


# Feature Scaling ---------------------------------------------------------

# plot histogram on variables to check if the distributions are normal

hist(data_fsel$Hit_target)
hist(data_fsel$Work_load_Average_per_day)
hist(data_fsel$Transportation_expense)
hist(data_fsel$Distance_from_Residence_to_Work)
hist(data_fsel$Service_time)
hist(data_fsel$Age)


fscale_col = c("Transportation_expense", "Distance_from_Residence_to_Work", "Service_time", 
               "Work_load_Average_per_day", "Hit_target", "Body_mass_index")

for(i in fscale_col){
  print(i)
  data_fsel[,i]=(data_fsel[,i]-min(data_fsel[,i]))/(max(data_fsel[,i])-min(data_fsel[,i]))
}



# Decision Tree -----------------------------------------------------------

# Clean the environment
rmExcept(c("data_fsel", "data"))

# Divide the data into train and test using stratified sampling method
set.seed(1234)
train.index = createDataPartition(data_fsel$Absenteeism_time_in_hours, p=0.8, list = FALSE)
train = data_fsel[train.index,]
test = data_fsel[-train.index,]
train$Absenteeism_time_in_hours = as.factor(as.character(train$Absenteeism_time_in_hours))
test$Absenteeism_time_in_hours = as.factor(as.character(test$Absenteeism_time_in_hours))

# KNN Algorithm
train$Absenteeism_time_in_hours = as.numeric(train$Absenteeism_time_in_hours)
knn_model = knnreg(train[,-19], train$Absenteeism_time_in_hours, k = 3)
predict_knn = predict(knn_model, test[,-19])
mae_knn = measureMAE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_knn))
mse_knn = measureMSE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_knn))
rmse_knn = measureRMSE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_knn))
accuracy_knn = 100 - rmse_knn

cat("Evaluation Metrics for KNN :",
    "\n",
    "\nMAE      :",mae_knn,
    "\nMSE      : ",mse_knn,
    "\nRMSE     :",rmse_knn,
    "\nAccuracy :",accuracy_knn,"%" )


# # Linear Regression
# train$Absenteeism_time_in_hours = as.numeric(train$Absenteeism_time_in_hours)
# lin_reg_model = lm(Absenteeism_time_in_hours ~., data = train)
# predict_lin_reg = predict(lin_reg_model, test[,-20])
# mae_linr = measureMAE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_lin_reg))
# mse_linr = measureMSE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_lin_reg))
# rmse_linr = measureRMSE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_lin_reg))
# accuracy_linr = 100 - rmse_linr
# 
# cat("Evaluation Metrics for Linear Regression :",
#     "\n",
#     "\nMAE      :",mae_linr,
#     "\nMSE      :",mse_linr,
#     "\nRMSE     :",rmse_linr,
#     "\nAccuracy :",accuracy_linr,"%" )








# Decision Tree
dt_model = rpart(Absenteeism_time_in_hours~., data = train)
summary(dt_model)
predict_dt = predict(dt_model, test[,-20])
mae_dt = measureMAE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_dt))
mse_dt = measureMSE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_dt))
rmse_dt = measureRMSE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_dt))
accuracy_dt = 100 - rmse_dt

cat("Evaluation Metrics for Decision Tree :",
    "\n",
    "\nMAE      :",mae_dt,
    "\nMSE      :",mse_dt,
    "\nRMSE     :",rmse_dt,
    "\nAccuracy :",accuracy_dt,"%" )



# Random Forest
rf_model = randomForest(x = train[,-20], y = train$Absenteeism_time_in_hours,
                                      importance = TRUE, ntree = 500)
summary(rf_model)
predict_rf = predict(rf_model, test[,-20])
mae_rf = measureMAE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_rf))
mse_rf = measureMSE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_rf))
rmse_rf = measureRMSE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_rf))
accuracy_rf = 100 - rmse_rf

cat("Evaluation Metrics for Random Forest :",
    "\n",
    "\nMAE      :",mae_rf,
    "\nMSE      :",mse_rf,
    "\nRMSE     :",rmse_rf,
    "\nAccuracy :",accuracy_rf,"%" )





# XGBoost
xgb_model = gbm(Absenteeism_time_in_hours~., data = train, n.trees = 500, interaction.depth = 2)
predict_xgb = predict(xgb_model, test[,-20], n.trees = 500)
mae_xgb = measureMAE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_xgb))
mse_xgb = measureMSE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_xgb))
rmse_xgb = measureRMSE(as.numeric(test$Absenteeism_time_in_hours), as.numeric(predict_xgb))
accuracy_xgb = 100 - rmse_xgb

cat("Evaluation Metrics for XGBoost :",
    "\n",
    "\nMAE      :",mae_xgb,
    "\nMSE      :",mse_xgb,
    "\nRMSE     :",rmse_xgb,
    "\nAccuracy :",accuracy_xgb,"%" )



# ARIMA MODEL -------------------------------------------------------------

absenteeism_monthly = aggregate(data$Absenteeism_time_in_hours, by=list(Category=data$Month_of_absence),FUN=sum)
absenteeism_monthly = absenteeism_monthly[-1,]
names(absenteeism_monthly) = c("month", "absence_hours")
rownames(absenteeism_monthly) = NULL
absenteeism_monthly

# As total absence hours is the sum from 3 years. Absence hours will be divided by 3
absenteeism_monthly$absence_hours = absenteeism_monthly$absence_hours/3
row.names(absenteeism_monthly) = absenteeism_monthly$month
absenteeism_monthly

time_series = ts(absenteeism_monthly$absence_hours)

# Plotting Time Series
plot(time_series)

# Checking if time series is stationary or not by Augmented Dickey-Fuller test
adf.test(time_series, alternative = "stationary", k=0)
#p-value 0.08365 is greater than 0.05 therefore the time series is not stationary

#ACF test
acf(time_series)
#PACF test
pacf(time_series)

# Shifting time series (1 lag)
time_series_diff = time_series - lag(time_series,1)

# Doing ADFuller test again
adf.test(time_series_diff, alternative = 'stationary', k=0)
#p-value 0.01254 is lesser than 0.05 Therefore the time series is now stationary

# ACF plot
acf(time_series_diff)
#PACF plot
pacf(time_series_diff)



# FORECASTING -------------------------------------------------------------

model_arima = arima(time_series_diff, c(3,0,3))
model_arima_fit = fitted(model_arima)

plot(time_series_diff)
lines(model_arima_fit)


predictions = predict(model_arima, n.ahead = 12)

predictions = cumsum(predictions$pred)
predictions_1 = predictions + rep(time_series[4],12)
as.data.frame(predictions_1)
predictions_2011 = ts(predictions_1)
predictions_1_df = as.data.frame(predictions_1)
row.names(predictions_1_df) = c(13:24)
predictions_2011 = ts(predictions_1_df$predictions_1, start = 13)

#plot time series values and forecast values
plot(time_series,xlim=c(1,24))
lines(predictions_2011)

predictions_2011


