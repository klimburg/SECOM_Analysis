# Author: Kevin C Limburg
# Description: Script for preProcessing/Feature Selection

# libraries 
library(caret)

# load in the data
list.mod <- readRDS("data/modified.RDS")
df.train.mod <- list.mod$train.data
results.train <- list.mod$train.labels
df.test.mod <- list.mod$test.data
results.test <- list.mod$test.labels

# feature selection - 3 methods pca, ica and chi-squared and full all use knnImpute
# pca
preProcess.pca <- preProcess(x = df.train.mod,
                             pcaComp = 60,
                             k = 5,
                             method = c("center","scale", "knnImpute", "pca"))

df.pca <- predict(preProcess.pca, newdata = df.train.mod)

# ica
preProcess.ica <- preProcess(x = df.train.mod,
                             n.comp = 60, # number of ICA components
                             k = 5,
                             method = c("center","scale", "knnImpute", "ica"))

df.ica <- predict(preProcess.ica, newdata = df.train.mod)

# chi squared
preProcess.chi <- preProcess(x = df.train.mod,
                             k = 5,
                             method = c("center","scale", "knnImpute"))

df.knnImpute <- predict(preProcess.chi, newdata = df.train.mod)
fs.chi <- chi.squared(results.train~., cbind(results.train,df.knnImpute))
subset <- cutoff.k(fs.chi, 60)
df.chisq <- df.knnImpute[, subset]


list.preProcess <- list("pca" = df.pca,
                        "ica" = df.ica,
                        "chisq" = df.chisq,
                        "knnImpute" = df.knnImpute)
saveRDS(list.preProcess, "data/preprocess.RDS")
