library(caret)
library(tidyverse)

# Classification and Regression Tree => caret

# Simple ML Pipeline
# 0. prep data/ clean data
# 1. split data 
# 2. train model
# 3. score model aka. prediction
# 4. evaluate model

# 0. prep/clean data

# subset only columns we want
full_df <- mtcars %>%
  select(mpg, hp, wt, am)

# check NA
full_df %>% 
  complete.cases()%>%
  mean()

# drop row with NA
clean_df <- full_df %>%
  drop_na()

# 1. split data


train_test_split <- function(data){
  set.seed(42)
  n <- nrow(data)
  train_id <- sample(1:n, size = 0.8*n)
  train_df <- data[train_id,]
  test_df <- data[-train_id,]
  return(list(training = train_df, 
              testing = test_df))
}

prep_data <-train_test_split(clean_df)

train_df <- prep_data[[1]]
test_df <- prep_data[[2]]

# 2. Train model
set.seed(42)
lm_model <- train(mpg ~ . 
                  ,data = train_df
                  # algorithm
                  ,method = "ranger" )

lm_model

# 3. score model

p <- predict(lm_model, newdata = test_df)


# 4. evaluate model

(MAE <- mean(abs(p - test_df$mpg)))

(RMSE <- sqrt(mean((p - test_df$mpg)**2)))

####






