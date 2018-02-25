##import the recent_movies dataset with read.csv
bikeshare <- read.csv("bikeshare.csv")
library("tidyverse")

#1a creating a new varible BADWEATHER 

bikeshare$BADWEATHER <- ifelse(bikeshare$WEATHERSIT >= 3, "YES", "NO")
summary(bikeshare)
attach(bikeshare)
search()
detach(movies)
detach(movies_test)
attach(bikeshare)
#1b plotting with count on Y and ATEMP on X axes
plot(ATEMP,COUNT, col=ifelse(BADWEATHER=="YES","blue","red"))

#1c plotting for Casual on Y and ATEMP on X axes and Registered on Y and ATEMP on X axes 
plot(ATEMP, CASUAL, col = ifelse(BADWEATHER =="YES", 'green', 'red'))
plot(ATEMP, REGISTERED, col = ifelse(BADWEATHER == "YES", 'orange', 'blue'))

#1d correlation of numerical variables
numerical_values <- bikeshare[,c(5:10)]
cor_numerical <- ifelse(cor(numerical_values)>abs(0.7),"HIGH","LOW")
cor_numerical

#2 multivariate regression 
fit1 <- lm(COUNT ~ MONTH+HOLIDAY+BADWEATHER+ATEMP+HUMIDITY+WINDSPEED)
summary(fit1)
#2e count with casual and registered
fit77 <- lm(COUNT ~ MONTH+HOLIDAY+WEEKDAY+BADWEATHER+TEMP+ATEMP+HUMIDITY+WINDSPEED+CASUAL+REGISTERED)
summary(fit77)
#3 count with badweather
fit2 <- lm(COUNT ~ BADWEATHER*WEEKDAY)
summary(fit2)
#3c 
fit3b<- lm(COUNT ~ BADWEATHER + WEEKDAY)
summary(fit3b)

#4 
fit3 <- lm(COUNT ~ MONTH+HOLIDAY+WEEKDAY+BADWEATHER+TEMP+ATEMP)
summary(fit3)

#4 without TEMP and ATEMP 
fit4 <- lm(COUNT ~ MONTH+HOLIDAY+WEEKDAY+BADWEATHER)
summary(fit4)

#training 
set.seed(11217)
train_insts <- sample(nrow(bikeshare),.75*nrow(bikeshare))

#instances
bike_train <- bikeshare[train_insts,]
bike_valid <- bikeshare[-train_insts,]

#test
##train a new model on the training data
fit_train <- lm(COUNT ~ MONTH+HOLIDAY+WEEKDAY+BADWEATHER+TEMP+ATEMP,data=bike_train)
summary(fit_train)
valid_preds <- predict(fit_train, newdata = bike_valid)
valid_actuals <- bike_valid$COUNT
RMSE <- sqrt(mean((valid_actuals - valid_preds)^2))
MAPE <- mean(abs((valid_preds-valid_actuals)/valid_actuals))
AE <- mean(valid_actuals-valid_preds)
RMSE
MAPE
AE


#without temp and atemp
fit_train1 <- lm(COUNT ~ MONTH+HOLIDAY+WEEKDAY+BADWEATHER,data=bike_train)
summary(fit_train1)
valid_preds <- predict(fit_train1, newdata = bike_valid)
valid_actuals <- bike_valid$COUNT
RMSE1 <- sqrt(mean((valid_actuals - valid_preds)^2))
MAPE1 <- mean(abs((valid_preds-valid_actuals)/valid_actuals))
AE1 <- mean(valid_actuals-valid_preds)
RMSE1
AE1
MAPE1
