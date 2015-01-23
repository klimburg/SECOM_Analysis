# Author: Kevin C Limburg
# Description: Script for preProcessing/Feature Selection the time series version

# libraries 
library(caret)

# load in the data
list.mod.ts <- readRDS("data/ts_mod.RDS")
df.train.ts <- list.mod.ts$train.data.ts
results.train.ts <- list.mod.ts$train.label.ts$V1
df.test.ts <- list.mod.ts$test.data.ts
results.test.ts <- list.mod.ts$test.label.ts$V1

# feature selection - 3 methods pca, ica and chi-squared and full all use knnImpute
# pca
preProcess.pca.ts <- preProcess(x = df.train.ts,
                             pcaComp = 60,
                             k = 5,
                             method = c("center","scale", "knnImpute", "pca"))

df.pca.ts <- predict(preProcess.pca.ts, newdata = df.train.ts)
df.pca.test.ts <- predict(preProcess.pca.ts, newdata = df.test.ts)

# ica
preProcess.ica.ts <- preProcess(x = df.train.ts,
                             n.comp = 60, # number of ICA components
                             k = 5,
                             method = c("center","scale", "knnImpute", "ica"))

df.ica.ts <- predict(preProcess.ica.ts, newdata = df.train.ts)
df.ica.test.ts <- predict(preProcess.ica.ts, newdata = df.test.ts)

# chi squared
preProcess.chi.ts <- preProcess(x = df.train.ts,
                             k = 5,
                             method = c("center","scale", "knnImpute"))

df.knnImpute.ts <- predict(preProcess.chi.ts, newdata = df.train.ts)
df.knnImpute.test.ts <- predict(preProcess.chi.ts, newdata = df.test.ts)
fs.chi <- chi.squared(results.train.ts~., cbind(results.train.ts,df.knnImpute.ts))
subset <- cutoff.k(fs.chi, 60)
df.chisq.ts <- df.knnImpute.ts[, subset]
df.chisq.test.ts <- df.knnImpute.test.ts[, subset]


list.preProcess.ts <- list("pca" = df.pca.ts,
                        "ica" = df.ica.ts,
                        "chisq" = df.chisq.ts,
                        "knnImpute" = df.knnImpute.ts)
saveRDS(list.preProcess.ts, "data/preprocess_ts.RDS")

list.preProcess.test.ts <- list("pca" = df.pca.test.ts,
                        "ica" = df.ica.test.ts,
                        "chisq" = df.chisq.test.ts,
                        "knnImpute" = df.knnImpute.test.ts)
saveRDS(list.preProcess.test.ts, "data/preprocessTest_ts.RDS")
