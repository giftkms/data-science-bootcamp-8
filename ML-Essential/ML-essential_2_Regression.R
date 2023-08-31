library(tidyr)
library(caret)
library(mlbench)

data("BostonHousing")

# rename data

df <- BostonHousing

# complete data
complete.cases(df) %>%
  mean()


# 1.train test split 
split_data <- function(df,train_size = 0.8){
  set.seed(42)
  n <- nrow(df)
  id <- sample(1:n, size = train_size*n)
  train_df <- df[id, ]
  test_df <- df[-id, ]
  return(list(train = train_df,
              test = test_df))
}

prep_df <- split_data(df, train_size = 0.8)
train_data <- prep_df[[1]]
test_data <- prep_df[[2]]


# 2. train model
set.seed(42)
model <- train(medv ~ rm + b + crim,
               data = train_data,
               method = "lm")


# 3. score / predict new data (test / unseen data)

p <- predict(model, newdata =  test_dat)

# 4. evaluate model  => absolute metrics

cal_mae <- function(actual , prediction){
  error <- actual - prediction
  (mean(abs(error)))
}

cal_mse <- function(actual , prediction){
  error <- actual - prediction
  mean(error**2)
}

cal_rmse <- function(actual , prediction){
  error <- actual - prediction
  sqrt(mean(error**2))
}

cal_mae(test_data$medv , p)
cal_mse(test_data$medv, p)
cal_rmse(test_data$medv, p)

