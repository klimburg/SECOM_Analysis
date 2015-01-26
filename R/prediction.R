# Author: Kevin C Limburg
# Description: Script for applying prediction models to test set
suppressMessages(library(caret))
list.models <- readRDS("data/models.RDS")
list.testData <- readRDS("data/preprocessTest.RDS")
list.modified <- readRDS("data/modified.RDS")


# random forest models
prediction.chi_rf <- predict(object = list.models$model.chi_rf, newdata = list.testData$chisq)
confMat.chi_rf <- confusionMatrix(data = prediction.chi_rf,
                                  reference = list.modified$test.labels,
                                  positive = "fail")

prediction.pca_rf <- predict(object = list.models$model.pca_rf, newdata = list.testData$pca)
confMat.pca_rf <- confusionMatrix(data = prediction.pca_rf,
                                  reference = list.modified$test.labels,
                                  positive = "fail")

prediction.ica_rf <- predict(object = list.models$model.ica_rf, newdata = list.testData$ica)
confMat.ica_rf <- confusionMatrix(data = prediction.ica_rf,
                                  reference = list.modified$test.labels,
                                  positive = "fail")

# naive bayes
prediction.chi_nb <- predict(object = list.models$model.chi_nb, newdata = list.testData$chisq)
confMat.chi_nb <- confusionMatrix(data = prediction.chi_nb,
                                  reference = list.modified$test.labels,
                                  positive = "fail")

prediction.pca_nb <- predict(object = list.models$model.pca_nb, newdata = list.testData$pca)
confMat.pca_nb <- confusionMatrix(data = prediction.pca_nb,
                                  reference = list.modified$test.labels,
                                  positive = "fail")

prediction.ica_nb <- predict(object = list.models$model.ica_nb, newdata = list.testData$ica)
confMat.ica_nb <- confusionMatrix(data = prediction.ica_nb,
                                  reference = list.modified$test.labels,
                                  positive = "fail")

# gbm
prediction.chi_gbm <- predict(object = list.models$model.chi_gbm, newdata = list.testData$chisq)
confMat.chi_gbm <- confusionMatrix(data = prediction.chi_gbm,
                                  reference = list.modified$test.labels,
                                  positive = "fail")

prediction.pca_gbm <- predict(object = list.models$model.pca_gbm, newdata = list.testData$pca)
confMat.pca_gbm <- confusionMatrix(data = prediction.pca_gbm,
                                  reference = list.modified$test.labels,
                                  positive = "fail")

prediction.ica_gbm <- predict(object = list.models$model.ica_gbm, newdata = list.testData$ica)
confMat.ica_gbm <- confusionMatrix(data = prediction.ica_gbm,
                                  reference = list.modified$test.labels,
                                  positive = "fail")

prediction.full_gbm <- predict(object = list.models$model.full_gbm, newdata = list.testData$knnImpute)
confMat.full_gbm <- confusionMatrix(data = prediction.full_gbm,
                                   reference = list.modified$test.labels,
                                   positive = "fail")

# rpart
prediction.chi_rpart <- predict(object = list.models$model.chi_rpart, newdata = list.testData$chisq)
confMat.chi_rpart <- confusionMatrix(data = prediction.chi_rpart,
                                   reference = list.modified$test.labels,
                                   positive = "fail")

prediction.pca_rpart <- predict(object = list.models$model.pca_rpart, newdata = list.testData$pca)
confMat.pca_rpart <- confusionMatrix(data = prediction.pca_rpart,
                                   reference = list.modified$test.labels,
                                   positive = "fail")

prediction.ica_rpart <- predict(object = list.models$model.ica_rpart, newdata = list.testData$ica)
confMat.ica_rpart <- confusionMatrix(data = prediction.ica_rpart,
                                   reference = list.modified$test.labels,
                                   positive = "fail")

prediction.full_rpart <- predict(object = list.models$model.full_rpart, newdata = list.testData$knnImpute)
confMat.full_rpart <- confusionMatrix(data = prediction.full_rpart,
                                    reference = list.modified$test.labels,
                                    positive = "fail")

