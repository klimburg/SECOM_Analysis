# Author: Kevin C Limburg
# Description: Exploratory Analysis of SECOM training set created from read_data.R

# libraries and source
library(caret)
library(dplyr)

# helper functions

# body

list.train <- readRDS("data/train.RDS")
df.train <- list.train$data
summary(df.train)
na.count <- colSums(is.na(df.train))
hist(log10(na.count))

nearZeroVar.cols <- nearZeroVar(df.train)
df.nearZero <- df.train[, nearZeroVar.cols]
df.train.mod <- df.train[, -nearZeroVar.cols]
na.count <- colSums(is.na(df.train.mod))
# id columns with 
hiNA <- na.count>100
df.hiNA <- df.train.mod[,-hiNA]

