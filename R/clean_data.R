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
# time series
list.ts <- readRDS("data/ts.RDS")
df.train.ts <- list.ts$data.train
df.test.ts <- list.ts$data.test
df.train.ts.labels <- list.ts$labels.train
df.test.ts.labels <- list.ts$labels.test
df.train.ts.labels$V1 <-  factor(df.train.ts.labels$V1 , levels=c(1,-1), labels =  c("fail", "pass"))
df.test.ts.labels$V1 <-  factor(df.test.ts.labels$V1 , levels=c(1,-1), labels =  c("fail", "pass"))

# id low variance columns and remove from train and test sets
colsToRemove <- nearZeroVar(df.train)
na.col.count <- unname(colSums(is.na(df.train)))
# remove cols with more than 20% missing
countToRemove <- round(nrow(df.train) * 0.2)
colsToRemove <- c(colsToRemove,which(na.col.count > countToRemove))
df.train.mod <- df.train[, -colsToRemove]
df.test.mod <- df.test[, -colsToRemove]


# repeat with ts data
colsToRemove.ts <- nearZeroVar(df.train.ts)
na.col.count.ts <- unname(colSums(is.na(df.train.ts)))
# remove cols with more than 20% missing
countToRemove.ts <- round(nrow(df.train.ts) * 0.2)
colsToRemove.ts <- c(colsToRemove.ts,which(na.col.count.ts > countToRemove.ts))
df.train.ts <- df.train.ts[, -colsToRemove.ts]
df.test.ts <- df.test.ts[, -colsToRemove.ts]


list.mod <- list("train.data"   = df.train.mod,
                 "train.labels" = results.train,
                 "test.data"    = df.test.mod,
                 "test.labels"  = results.test)
list.mod.ts <- list("train.data.ts"  = df.train.ts,
                    "train.label.ts" = df.train.ts.labels,
                    "test.data.ts"    = df.test.ts,
                    "test.label.ts"  = df.test.ts.labels)


saveRDS(list.mod, file = "data/modified.RDS")
saveRDS(list.mod.ts, file = "data/ts_mod.RDS")
