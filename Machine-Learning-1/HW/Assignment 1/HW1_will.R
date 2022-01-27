#SET UP --------------------------------------------------------------------------------------------------
library(FNN)
library(dplyr)
library(ggplot2)
library(caret)
library(psych)

#use the samples provided

train_data <- read.table("C:/Users/Will Jiang/Desktop/Emory Desktop/ML/Individual Assignment/Assignment 1/GradedHW1-Train-Data.csv",
                         header = TRUE, sep = ",",stringsAsFactors = FALSE)
train_data$Building.Age = 2010 - train_data$Year.Built
train_data <-  train_data%>% 
  select(Lot.Area,Total.Bsmt.SF,Gr.Liv.Area,Full.Bath,Bedroom.AbvGr,Building.Age,SalePrice)

test_data <- read.table("C:/Users/Will Jiang/Desktop/Emory Desktop/ML/Individual Assignment/Assignment 1/GradedHW1-Test-Data.csv",
                         header = TRUE, sep = ",",stringsAsFactors = FALSE)
test_data$Building.Age = 2010 - test_data$Year.Built
test_data <-  test_data%>% 
  select(Lot.Area,Total.Bsmt.SF,Gr.Liv.Area,Full.Bath,Bedroom.AbvGr,Building.Age,SalePrice)

validate_data <- read.table("C:/Users/Will Jiang/Desktop/Emory Desktop/ML/Individual Assignment/Assignment 1/GradedHW1-Validation-Data.csv",
                            header = TRUE, sep = ",",stringsAsFactors = FALSE)
validate_data$Building.Age = 2010 - validate_data$Year.Built
validate_data <-  validate_data%>% 
  select(Lot.Area,Total.Bsmt.SF,Gr.Liv.Area,Full.Bath,Bedroom.AbvGr,Building.Age,SalePrice)

#Q2: examine the individual variables---------------------------------------------------------------------
#examine the individual variables
summary(train_data) # summary statistics
summary(validate_data)
summary(test_data)

#deal with missing values
colSums(is.na(train_data))
colSums(is.na(test_data))
colSums(is.na(validate_data)) #here is a NA

validate_data[is.na(validate_data$Total.Bsmt.SF) == TRUE,]
validate_data <- validate_data[is.na(validate_data$Total.Bsmt.SF) == FALSE,]

#show the skewness
par(mfrow=c(1,2))
hist(train_data$Lot.Area); qqnorm(train_data$Lot.Area)
hist(train_data$Total.Bsmt.SF); qqnorm(train_data$Total.Bsmt.SF)
hist(train_data$Gr.Liv.Area); qqnorm(train_data$Gr.Liv.Area)
hist(train_data$Full.Bath); qqnorm(train_data$Full.Bath)
hist(train_data$Bedroom.AbvGr); qqnorm(train_data$Bedroom.AbvGr)
hist(train_data$Building.Age); qqnorm(train_data$Building.Age)

hist(train_data$SalePrice); qqnorm(train_data$SalePrice)

#Q3+Q4: Raw, Unstandardized Data -------------------------------------------------------------------------

X_train <- train_data[,1:6]
y_train <- train_data$SalePrice
X_test <- test_data[,1:6]
y_test <- test_data$SalePrice
X_val <- validate_data[,1:6]
y_val <- validate_data$SalePrice

#calculate root MSE with different k (validation data)
k_RMSE <- data.frame("k"=integer(),"RMSE"=double())
for(k in 1:40) {
  y_val_pred <- knn.reg(train = X_train, test = X_val, y = y_train, k)$pred
  RMSE <- sqrt(sum((y_val-y_val_pred)**2)/length(y_val))
  k_RMSE[k,] = c(k,RMSE)
}
print(k_RMSE)
filter(k_RMSE, k_RMSE$RMSE == min(k_RMSE$RMSE)) #find the best k with minimum RMSE

#plot root MSE with different k (validation data)
ggplot(k_RMSE,aes(x=k, y=RMSE)) +
  geom_point() +
  geom_vline(aes(xintercept=12),linetype='dashed',color='red') +
  annotate('text', label='k=12', x = 13, y = 57000, size=4, hjust=0, color ='red') +
  labs(title = 'RMSE with Different k',
       subtitle = 'Untransformed, Unstandardized Data') +
  theme_light()

#calculate root MSE with different k (test data)
k_RMSE <- data.frame("k"=integer(),"RMSE"=double())
for(k in 1:40) {
  y_test_pred <- knn.reg(train = X_train, test = X_test, y = y_train, k)$pred
  RMSE <- sqrt(sum((y_test-y_test_pred)**2)/length(y_test))
  k_RMSE[k,] = c(k,RMSE)
}
print(k_RMSE)
filter(k_RMSE, k_RMSE$RMSE == min(k_RMSE$RMSE)) #find the best k with minimum RMSE

#Q5+Q6: Raw, Standardized Data-------------------------------------------------------------------------
#standardize only x 

#standardize the training set
X_train_std <- scale(X_train, center = TRUE, scale = TRUE)
#standardize the validation and test data with the same specifications as training data
stdParam <- preProcess(X_train)
X_val_std <- predict(stdParam, X_val)
X_test_std <- predict(stdParam, X_test)

#check if normal distribution with mean=0 and sd=1
describe(X_train_std)
describe(X_val_std)
describe(X_test_std)

#calculate root MSE with different k (validation data)
k_RMSE <- data.frame("k"=integer(),"RMSE"=double())
for(k in 1:40) {
  y_val_pred <- knn.reg(train = X_train_std, test = X_val_std, y = y_train, k)$pred
  RMSE <- sqrt(sum((y_val-y_val_pred)**2)/length(y_val))
  k_RMSE[k,] = c(k,RMSE)
}
print(k_RMSE)
filter(k_RMSE, k_RMSE$RMSE == min(k_RMSE$RMSE)) #find the best k with minimum RMSE

#plot root MSE with different k (validation data)
ggplot(k_RMSE,aes(x=k, y=RMSE)) +
  geom_point() +
  geom_vline(aes(xintercept=12),linetype='dashed',color='red') +
  annotate('text', label='k=12', x = 13, y = 47000, size=4, hjust=0, color ='red') +
  labs(title = 'RMSE with Different k',
       subtitle = 'Untransformed, Standardized Data') +
  theme_light()

#calculate root MSE with different k (test data)
k_RMSE <- data.frame("k"=integer(),"RMSE"=double())
for(k in 1:40) {
  y_test_pred <- knn.reg(train = X_train_std, test = X_test_std, y = y_train, k)$pred
  RMSE <- sqrt(sum((y_test-y_test_pred)**2)/length(y_test))
  k_RMSE[k,] = c(k,RMSE)
}
print(k_RMSE)
filter(k_RMSE, k_RMSE$RMSE == min(k_RMSE$RMSE)) #find the best k with minimum RMSE

#Q7+Q8: Transformed, Unstandardized Data--------------------------------------------------
#transform both x and y
X_test_t <- X_test
X_test_t$Lot.Area <- log(X_test_t$Lot.Area) 
X_test_t$Total.Bsmt.SF <- sqrt(X_test_t$Total.Bsmt.SF) 
X_test_t$Gr.Liv.Area <- log(X_test_t$Gr.Liv.Area)
y_test_t <- log(y_test) 

X_val_t <- X_val
X_val_t$Lot.Area <- log(X_val_t$Lot.Area) 
X_val_t$Total.Bsmt.SF <- sqrt(X_val_t$Total.Bsmt.SF)
X_val_t$Gr.Liv.Area <- log(X_val_t$Gr.Liv.Area)
y_val_t <- log(y_val) 

X_train_t <- X_train
X_train_t$Lot.Area <- log(X_train_t$Lot.Area) 
X_train_t$Total.Bsmt.SF <- sqrt(X_train_t$Total.Bsmt.SF)
X_train_t$Gr.Liv.Area <- log(X_train_t$Gr.Liv.Area)
y_train_t <- log(y_train)

hist(X_train_t$Lot.Area); qqnorm(X_train_t$Lot.Area)
hist(X_train_t$Total.Bsmt.SF); qqnorm(X_train_t$Total.Bsmt.SF)
hist(X_train_t$Gr.Liv.Area); qqnorm(X_train_t$Gr.Liv.Area)
hist(y_train_t); qqnorm(y_train_t)

#calculate root MSE with different k (validation data)
k_RMSE <- data.frame("k"=integer(),"RMSE"=double())
for(k in 1:40) {
  y_val_pred <- knn.reg(train = X_train_t, test = X_val_t, y = y_train_t, k)$pred
  y_val_pred <- exp(y_val_pred) #need transform back the log y to calculate comparable RMSE
  RMSE <- sqrt(sum((y_val-y_val_pred)**2)/length(y_val_t))
  k_RMSE[k,] = c(k,RMSE)
}
print(k_RMSE)
filter(k_RMSE, k_RMSE$RMSE == min(k_RMSE$RMSE)) #find the best k with minimum RMSE

#plot root MSE with different k (validation data)
ggplot(k_RMSE,aes(x=k, y=RMSE)) +
  geom_point() +
  geom_vline(aes(xintercept=2),linetype='dashed',color='red') +
  annotate('text', label='k=2', x = 3, y = 56550, size=4, hjust=0, color ='red') +
  labs(title = 'RMSE with Different k',
       subtitle = 'Transformed, Untandardized Data') +
  theme_light()

#Q9+Q10: Transformed, Standardized Data------------------------------------------------------
#transform both x and y, and standardize only x 
X_train_ts <- scale(X_train_t, center = TRUE, scale = TRUE)
stdParam <- preProcess(X_train_t)
X_val_ts <- predict(stdParam, X_val_t)
X_test_ts <- predict(stdParam, X_test_t)

#check if normal distribution with mean=0 and sd=1
describe(X_train_ts)
describe(X_val_ts)
describe(X_test_ts)

#calculate root MSE with different k (validation data)
k_RMSE <- data.frame("k"=integer(),"RMSE"=double())
for(k in 1:40) {
  y_val_pred <- knn.reg(train = X_train_ts, test = X_val_ts, y = y_train_t, k)$pred
  y_val_pred <- exp(y_val_pred) #need transform back the log y to calculate comparable RMSE
  RMSE <- sqrt(sum((y_val-y_val_pred)**2)/length(y_val_t))
  k_RMSE[k,] = c(k,RMSE)
}
print(k_RMSE)
filter(k_RMSE, k_RMSE$RMSE == min(k_RMSE$RMSE)) #find the best k with minimum RMSE

#plot root MSE with different k (validation data)
ggplot(k_RMSE,aes(x=k, y=RMSE)) +
  geom_point() +
  geom_vline(aes(xintercept=10),linetype='dashed',color='red') +
  annotate('text', label='k=10', x = 11, y = 47600, size=4, hjust=0, color ='red') +
  labs(title = 'RMSE with Different k',
       subtitle = 'Transformed, Standardized Data') +
  theme_light()
