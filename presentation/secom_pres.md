Predictive Quality Modeling in Semiconductor Manufacturing
========================================================
author: Kevin C. Limburg
date: 2015-01-19

Introduction
========================================================

Put in some info about the data set

* Class Imbalance
* Many missing features
* Feature to observation ratio is high


Methodology
========================================================
* Clean Data
      * Remove zero and near zero variance features
      * Remove features where more than 20% is missing  
* Pre-processing
      * Impute data using k-nearest neighbors (k=5)
      * Feature Selection using PCA, ICA and Chi-Squared (n=60)
* Model Selection
      * Decision Tree (rpart)
      * Random Forest
      * Naive Bayes
      * Stochastic Gradiant Boosting (gbm)

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-1](secom_pres-figure/unnamed-chunk-1-1.png) 
