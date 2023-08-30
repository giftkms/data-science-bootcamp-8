library(caret)
library(tidyr)
library(ggplot2)

dataset <- read_excel("projects/India-House-price/House Price India.xlsx")

View(dataset)

complete.cases(dataset)%>%
  mean()

hist(dataset$Price)

dataset$Price <- log(dataset$Price)


hist(dataset$Price)

## split data

split_data <- function(data){
  set.seed(42)
  n <- nrow(data)
  id <- sample(1:n, size = n*0.8)
  train_data <- data[id,] 
  test_data <- data[-id,]
  return(list(training = train_data,
              testing = test_data))}

prep_data <- split_data(dataset)

train_data <- prep_data[[1]]
test_data <- prep_data$testing

## Train Model 

set.seed(42)

lm_model <- train(Price ~ . ,
                  data = train_data,
                  method = "lm")

## score model 

p <- predict(lm_model, test_data)

## evaluate model

(MAE <- mean(abs(p - test_data$Price)))

(RMSE <- sqrt(mean((p - test_data$Price)**2)))











