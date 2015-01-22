# Author: Kevin C Limburg
# Description: Cleaning of SECOM training set created from read_data.R

# libraries and source
library(dplyr)
library(caret)
# helper functions

# body
# read in train and test, convert labels to factors
list.train <- readRDS("data/train.RDS")
df.train <- list.train$data
results.train <- factor(list.train$labels$V1, levels=c(1,-1), labels = c("fail", "pass"))
list.test <- readRDS("data/test.RDS")
df.test <- list.test$data
results.test <- factor(list.test$labels$V1, levels=c(1,-1), labels =  c("fail", "pass"))

# id low variance columns and remove from train and test sets
colsToRemove <- nearZeroVar(df.train)
na.col.count <- unname(colSums(is.na(df.train)))
# remove cols with more than 20% missing
countToRemove <- round(nrow(df.train) * 0.2)
colsToRemove <- c(colsToRemove,which(na.col.count > countToRemove))
df.train.mod <- df.train[, -colsToRemove]
df.test.mod <- df.test[, -colsToRemove]

list.mod <- list("train.data"   = df.train.mod,
                 "train.labels" = results.train,
                 "test.data"    = df.test.mod,
                 "test.labels"  = results.test)

saveRDS(list.mod, file = "data/modified.RDS")
