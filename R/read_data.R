# Author: Kevin C Limburg
# Description: Script for reading in raw data and split into training and test

df.secom <- read.table("./data/secom.data")
df.labels <- read.table("./data/secom_labels.data")

# split data into train and test set %70, %30
set.seed(123)
nrow.secom <- nrow(df.secom)
sample.train <- sample(x = 1:nrow.secom,
                       size = 0.7*nrow.secom,
                       replace = FALSE)
index.train <- 1:nrow.secom %in% sample.train
df.secom.train <- df.secom[index.train, ]
df.labels.train <- df.labels[index.train, ]
df.secom.test <- df.secom[!index.train, ]
df.labels.test <- df.labels[!index.train, ]

# pack data into lists and store for later use
list.test <- list("data"   = df.secom.test, 
                  "labels" = df.labels.test)

list.train <- list("data"   = df.secom.train, 
                  "labels" = df.labels.train)

saveRDS(list.test,"./data/test.RDS")
saveRDS(list.train,"./data/train.RDS")

# split in time series 70 pct
splitPoint <- floor(nrow(df.labels)*0.7)
df.train.ts <- df.secom[1:splitPoint,]
df.test.ts <- df.secom[(splitPoint+1):nrow(df.secom),]
df.train.ts.label <- df.labels[1:splitPoint,]
df.test.ts.label <- df.labels[(splitPoint+1):nrow(df.labels),]
list.ts <- list("data.train"   = df.train.ts , 
                "labels.train" = df.train.ts.label,
                "data.test"   = df.test.ts , 
                "labels.test" = df.test.ts.label)
saveRDS(list.ts,"./data/ts.RDS")
rm(list=ls())