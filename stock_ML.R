setwd("/Users/mac/Documents/AV/stock")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Based on five day and three day moving average

train$diff1 <- 5*train$Five_Day_Moving_Average - 3*train$Three_Day_Moving_Average
test$diff1 <- 5*test$Five_Day_Moving_Average - 3*test$Three_Day_Moving_Average

# Based on average true range and true range

train$diff2 <- (14*train$Average_True_Range - train$True_Range)/13
test$diff2 <- (14*test$Average_True_Range - test$True_Range)/13

# Based on twenty day moving average and ten day moving average

train$diff3 <- 20*train$Twenty_Day_Moving_Average - 10*train$Ten_Day_Moving_Average
test$diff3 <- 20*test$Twenty_Day_Moving_Average - 10*test$Ten_Day_Moving_Average

train$fifteenday_moving_average <- (20*train$Twenty_Day_Moving_Average - 10*train$Ten_Day_Moving_Average + 5*train$Five_Day_Moving_Average)/15
test$fifteenday_moving_average <- (20*test$Twenty_Day_Moving_Average - 10*test$Ten_Day_Moving_Average + 5*test$Five_Day_Moving_Average)/15

# Variable if it is in positive direction 

train$direction <- ifelse(train$Positive_Directional_Movement>train$Negative_Directional_Movement,1,0)
test$direction <- ifelse(test$Positive_Directional_Movement>test$Negative_Directional_Movement,1,0)

# Creating directional index

train$DX <- (train$Positive_Directional_Movement + train$Negative_Directional_Movement)/2
test$DX <- (test$Positive_Directional_Movement + test$Negative_Directional_Movement)/2

# If five day moving average greater than three day moving average

train$fto3 <- ifelse(train$Five_Day_Moving_Average>train$Three_Day_Moving_Average,1,0)
test$fto3 <- ifelse(test$Five_Day_Moving_Average>test$Three_Day_Moving_Average,1,0)

# If twenty day moving average greater than ten day moving average

train$ttot <- ifelse(train$Twenty_Day_Moving_Average>train$Ten_Day_Moving_Average,1,0)
test$ttot <- ifelse(test$Twenty_Day_Moving_Average>test$Ten_Day_Moving_Average,1,0)

# If average true range is greater than diff2

train$trd <- ifelse(train$Average_True_Range > train$diff2,1,0)
test$trd <- ifelse(test$Average_True_Range > test$diff2,1,0)

# 25% Volumes are greater than 0.05

train$vol <- ifelse(train$Volume>0,1,0)
test$vol <- ifelse(test$Volume>0,1,0)

library(xgboost)
set.seed(23)

# Input and test matrices

train_matrix <- xgb.DMatrix(data = as.matrix(train[,c(4:12,14:23)]) , label = as.matrix(train$Outcome), missing = "NAN")
test_matrix <- xgb.DMatrix(data = as.matrix(test[,c(4:22)]),missing = "NAN")

set.seed(23)
param <- list(objective = "binary:logistic",
              eval_metric = "logloss",
              max_depth = 5,
              eta = 0.15,
              gamma = 0, 
              subsample = 0.8,
              colsample_bytree = 0.8, 
              min_child_weight = 1)

modelf <- xgb.train(train_matrix,params = param,nrounds = 100, verbose = T, print.every.n = 1)

pred1 <- data.frame("Outcome" = (predict(modelf, newdata = test_matrix)))

test$outcome <- pred1$Outcome

submission <- data.frame("ID" = test$ID, "Outcome" = test$outcome)

colnames(submission) <- c("ID","Outcome")

write.csv(submission,"submission.csv",row.names = F)



