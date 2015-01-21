# Author: Kevin C Limburg
# Description: Script for preProcessing/Feature Selection

library(doParallel)
library(caret)
library(FSelector)

# helpers
set.seed.cv <- function(init.seed, kFolds, cvRepeats, tuneLength)
{
      # description: generate list of seeds for caret::trainControl. This is
      #     useful for parallel processing as seeds wont be reproducible.
      # args:
      #        init.seed: starting seed
      #           kFolds: number of folds#
      #        cvRepeats: number of repeats for cross validation
      #       tuneLength: number of tuning parameter combos
      
      # returns: a list of seeds for use in caret::trainControl
      
      set.seed(init.seed)
      seeds <- vector(mode = "list", length = kFolds*cvRepeats+1)#length is = (n_repeats*nresampling)+1
      for(i in 1:(kFolds*cvRepeats)){
            seeds[[i]]<- sample.int(n=1000, tuneLength*cvRepeats) 
      }    
      
      seeds[[kFolds*cvRepeats+1]]<-sample.int(1000, 1)#for the last model
      seeds
}

# load in the data
list.preProcess <- readRDS("data/preprocess.RDS")
df.pca <- list.preProcess$pca
df.ica <- list.preProcess$ica
df.chisq <- list.preProcess$chisq
df.knnImpute<- list.preProcess$knnImpute


cl <- makeCluster(4)
registerDoParallel(cl)

# set CV params
kFolds <- 10
cvRepeats <- 10

# set tunelength for rpart, cp parameter
tuneLength <- 30
seeds.rpart <- set.seed.cv(123, kFolds, cvRepeats, tuneLength)

control.model <- trainControl(method = 'repeatedcv', 
                              seeds = seeds.rpart,                            
                              number = kFolds, 
                              repeats = cvRepeats,
                              classProbs = TRUE,                            
                              summaryFunction = twoClassSummary)

model.full_rpart <- train(x = df.knnImpute, 
                          y = results.train,
                          method = "rpart",
                          metric = "ROC",
                          tuneLength = tuneLength,
                          trControl = control.model)
model.full_rpart

model.pca_rpart <- train(x = df.pca, 
                         y = results.train,
                         method = "rpart",
                         metric = "ROC",
                         tuneLength = tuneLength,
                         trControl = control.model)
model.pca_rpart

model.ica_rpart <- train(x = df.ica, 
                         y = results.train,
                         method = "rpart",
                         metric = "ROC",
                         tuneLength = tuneLength,
                         trControl = control.model)
model.ica_rpart 

model.chi_rpart <- train(x = df.chisq, 
                         y = results.train,
                         method = "rpart",
                         metric = "ROC",
                         tuneLength = tuneLength,
                         trControl = control.model)
model.chi_rpart

# gbm model has three tuning params
gbmGrid <- expand.grid(interaction.depth = c(1, 5, 9),
                       n.trees = seq(100,1000,100), 
                       shrinkage = 0.1)

tuneLength <- nrow(gbmGrid)
seeds.gbm <- set.seed.cv(123, kFolds, cvRepeats, tuneLength)

control.model <- trainControl(method = 'repeatedcv', 
                              seeds = seeds.gbm,                            
                              number = kFolds, 
                              repeats = cvRepeats,
                              classProbs = TRUE,                            
                              summaryFunction = twoClassSummary)

model.full_gbm <- train(x = df.knnImpute, 
                        y = results.train,
                        method = "gbm",
                        metric = "ROC",
                        tuneLength = tuneLength,
                        tuneGrid = gbmGrid,
                        trControl = control.model)
model.full_gbm

model.pca_gbm <- train(x = df.pca, 
                       y = results.train,
                       method = "gbm",
                       metric = "ROC",
                       tuneLength = tuneLength,
                       tuneGrid = gbmGrid,
                       trControl = control.model)
model.pca_gbm

model.ica_gbm <- train(x = df.ica, 
                       y = results.train,
                       method = "gbm",
                       metric = "ROC",
                       tuneLength = tuneLength,
                       tuneGrid = gbmGrid,
                       trControl = control.model)
model.ica_gbm 

model.chi_gbm <- train(x = df.chisq, 
                       y = results.train,
                       method = "gbm",
                       metric = "ROC",
                       #tuneLength = tuneLength,
                       #tuneGrid = gbmGrid,
                       trControl = control.model)
model.chi_gbm

# randomForest models tune param is mtry
tuneLength <- 30i
seeds.rf <- set.seed.cv(123, kFolds, cvRepeats, tuneLength)

control.model <- trainControl(method = 'repeatedcv', 
                              seeds = seeds.rf,                            
                              number = kFolds, 
                              repeats = cvRepeats,
                              classProbs = TRUE,                            
                              summaryFunction = twoClassSummary)

model.full_rf <- train(x = df.knnImpute, 
                       y = results.train,
                       method = "parRF",
                       metric = "ROC",
                       tuneLength = tuneLength,
                       trControl = control.model)
model.full_rf

model.pca_rf <- train(x = df.pca, 
                      y = results.train,
                      method = "parRF",
                      metric = "ROC",
                      tuneLength = tuneLength,
                      trControl = control.model)
model.pca_rf

model.ica_rf <- train(x = df.ica, 
                      y = results.train,
                      method = "parRF",
                      metric = "ROC",
                      tuneLength = tuneLength,
                      trControl = control.model)
model.ica_rf 

model.chi_rf <- train(x = df.chisq, 
                      y = results.train,
                      method = "parRF",
                      metric = "ROC",
                      tuneLength = tuneLength,
                      trControl = control.model)
model.chi_rf

# naive bayes models tune params fL, usekernel
nbGrid <- expand.grid(usekernel = c(0,1),
                      fL = seq(0,1,length.out = 15))
tuneLength <- nrow(nbGrid)
seeds.nb <- set.seed.cv(123, kFolds, cvRepeats, tuneLength)

control.model <- trainControl(method = 'repeatedcv', 
                              seeds = seeds.nb,                            
                              number = kFolds, 
                              repeats = cvRepeats,
                              classProbs = TRUE,                            
                              summaryFunction = twoClassSummary)

model.full_nb <- train(x = df.knnImpute, 
                       y = results.train,
                       method = "nb",
                       metric = "ROC",
                       tuneLength = tuneLength,
                       tuneGrid = nbGrid,
                       trControl = control.model)
model.full_nb

model.pca_nb <- train(x = df.pca, 
                      y = results.train,
                      method = "nb",
                      metric = "ROC",
                      tuneLength = tuneLength,
                      tuneGrid = nbGrid,
                      trControl = control.model)
model.pca_nb

model.ica_nb <- train(x = df.ica, 
                      y = results.train,
                      method = "nb",
                      metric = "ROC",
                      tuneLength = tuneLength,
                      tuneGrid = nbGrid,
                      trControl = control.model)
model.ica_nb 

model.chi_nb <- train(x = df.chisq, 
                      y = results.train,
                      method = "nb",
                      metric = "ROC",
                      tuneLength = tuneLength,
                      tuneGrid = nbGrid,
                      trControl = control.model)
model.chi_nb

stopCluster(cl)

