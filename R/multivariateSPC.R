# Multivariate SPC Models

library(qcc)
library(caret)
list.train <- readRDS("data/preprocess_ts.RDS")
list.test <- readRDS("data/preprocessTest_ts.RDS")
list.results <- readRDS("data/ts_mod.RDS")
df.pca  <- list.train$pca
df.ica  <- list.train$ica
df.chi  <- list.train$chisq
results <- list.results$train.label.ts

df.pca.test  <- list.test$pca
df.ica.test  <- list.test$ica
df.chi.test  <- list.test$chisq
results.test <- list.results$test.label.ts

predictions <- data.frame("actual" = results.test$V1,
                          "pca_pred" = factor("pass", levels=c("fail","pass")),
                          "ica_pred" = factor("pass", levels=c("fail","pass")),
                          "chi_pred" = factor("pass", levels=c("fail","pass")))

pca_t2<- mqcc(data = df.pca, type = "T2.single", newdata = df.pca.test, ylim=c(0,500),pred.limits = T)
splitPoint <- nrow(df.pca)
pca_t2Violations <- pca_t2$violations$beyond.pred.limits
predFail.pca<-pca_t2Violations[which(pca_t2Violations >splitPoint)]-splitPoint
predictions[predFail.pca,"pca_pred"] <- factor("fail", levels=c("fail","pass"))
confusionMatrix(predictions$pca_pred, reference = predictions$actual, positive = "fail")


ica_t2<- mqcc(data = df.ica, type = "T2.single", newdata = df.ica.test, ylim=c(0,1000),pred.limits = T)
ica_t2Violations <- ica_t2$violations$beyond.pred.limits
predFail.ica<-ica_t2Violations[which(ica_t2Violations >splitPoint)]-splitPoint
predictions[predFail.ica,"ica_pred"] <- factor("fail", levels=c("fail","pass"))
confusionMatrix(predictions$ica_pred, reference = predictions$actual, positive = "fail")

chi_t2<- mqcc(data = df.chi, type = "T2.single", newdata = df.chi.test, ylim=c(0,10000),pred.limits = T)
chi_t2Violations <- chi_t2$violations$beyond.pred.limits
predFail.chi<-chi_t2Violations[which(chi_t2Violations >splitPoint)]-splitPoint
predictions[predFail.chi,"chi_pred"] <- factor("fail", levels=c("fail","pass"))
confusionMatrix(predictions$chi_pred, reference = predictions$actual, positive = "fail")


ewma(data = df.pca[,c(1,10)],sizes = 1)



