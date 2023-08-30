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
set.seed(42)

train_test_split <- function(data){
  n <- nrow(data)
  train_id <- sample(1:n, size = 0.8*n)
  train_df <- data[train_id,]
  test_df <- data[-train_id,]
  return(list(train_df, test_df))
}




