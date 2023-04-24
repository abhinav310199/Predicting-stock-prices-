rm(list= ls())
library(readxl)

stocks <- read.csv("stocks.csv")
stocks <- na.omit(stocks)
stocks <- stocks[ , -c(1,6)]

stocks <- as.data.frame(stocks)
head(stocks)

set.seed(3)
train.index <- sample(c(1:dim(stocks)[1]), dim(stocks)[1]*0.6)  
train.df <- stocks[train.index, ]
valid.df <- stocks[-train.index, ]

linear.reg <- lm(Close ~ ., data = stocks) 
summary(linear.reg)

predictions <- predict(linear.reg, valid.df)
predictionss<- predict(linear.reg, stocks)
head(predictions)
install.packages("Metrics")
library("Metrics")
rmse_value <- rmse(predictions, valid.df$Close)
#rmse value 62.911
summary(linear.reg)$r.squared
summary(linear.reg)

library("neuralnet")
library("dplyr")
#NN
nn <- neuralnet(Close ~ Open + High +Low + Volume, data = stocks, linear.output = F, hidden = 3)
nn.pred <- predict(nn, valid.df, type = "response")

library(forecast)
stocks <- read.csv("stocks.csv")
stocks <- na.omit(stocks)
stocks <- stocks[ , -6]
set.seed(3)
train.index <- sample(c(1:dim(stocks)[1]), dim(stocks)[1]*0.6)  
train.df <- stocks[train.index, ]
valid.df <- stocks[-train.index, ]
stocks$Date <- as.Date(stocks$Date)
library(lubridate)
fit <- auto.arima(valid.df$Close)
predictions <- forecast(fit, h = nrow(valid.df))
accuracy <- accuracy(predictions, valid.df$Close)
summary(fit)
#rmse value 338.34


library(randomForest)

# Load and prepare the data
stocks <- read.csv("stocks.csv")
stocks <- na.omit(stocks)
stocks <- stocks[, -c(1, 6)]
set.seed(3)
train.index <- sample(c(1:dim(stocks)[1]), dim(stocks)[1]*0.6)
train.df <- stocks[train.index,]
valid.df <- stocks[-train.index,]

# Build the random forest model
rf.model <- randomForest(Close ~ ., data = train.df, ntree = 500, mtry = sqrt(ncol(train.df) - 1))

# Make predictions on the validation set
rf.pred <- predict(rf.model, newdata = valid.df)

# Evaluate the model performance
rf.rmse <- sqrt(mean((rf.pred - valid.df$Close)^2))
print(paste0("Random Forest RMSE: ", rf.rmse))
# RMSE for random forest is 101.7

# Load required packages
library(ggplot2)

# Create a data frame with the predicted and actual values
results.df <- data.frame(Predicted = rf.pred, Actual = valid.df$Close)

# Create a scatterplot with a line of perfect fit
ggplot(data = results.df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Actual Close", y = "Predicted Close", title = "Random Forest Predictions vs Actuals")

# Create a scatterplot with the regression line
ggplot(data = stocks, aes(x = Open, y = Close)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Open Price", y = "Close Price", title = "Linear Regression of Open and Close Prices")
