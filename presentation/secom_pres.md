Predictive Quality Modeling in Semiconductor Manufacturing
========================================================
author: Kevin C. Limburg
date: 2015-01-19
output: beamer_presentation

Introduction
========================================================


In many modern manufacturing settings, the amount of data being generated makes
univariate SPC methods impractical. 

This presentation explores the use of machine learning algorithms 
to identify semiconductor lots with low yields.

The dataset comes from the 
[UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/SECOM).


Methodology
========================================================
* Split data into training and test sets
* Pre-processing the data to remove missing values and reduce feature set    
* Tune models on training data set
* Compare results of best models from training on the held out test set


Split Data
========================================================

```
[1] 4
```
* The data set contains 1567 semiconductor wafer batches with
590 features. 
* Each batch is given a binary classification of acceptable or not based on yield. 
* There are only 104 failures in the entire dataset.
* There are a total of 41951 missing values.
* Split the data into training and test sets (70%/30%).


Pre-processing
======================================================
* The test set is put to the side for now
* Remove zero and near zero variance features
* Remove features where more than 20% of values are missing
* Center and scale each feature to mean of 0 and variance of 1
* Impute missing data using k-nearest neighbors (k=5)
* Feature selection using PCA, ICA and Chi-Squared (n=60)

Model Selection
=========================================================
We attempted to build a prediction model using the following four algorithms:
* Decision Tree ([rpart](http://cran.r-project.org/web/packages/rpart/rpart.pdf))
* [Random Forest](http://cran.r-project.org/web/packages/randomForest/randomForest.pdf)
* [Naive Bayes](need to put a link here) fix links!!!
* Stochastic Gradiant Boosting ([gbm](put gbm link here))

Additionally these models were fit using repeated cross validation (5-fold, 10 times)
and with three feature selection methods and the full data set.

Training
=======================================================

* The models were tuned with the relevant tuning parameters for each algorithm
* Sensitivity was used as the tuning metric versus accuracy or ROC 
due to high class imbalance.
*  One model per combination of feature set and algorithm was chosen for
evaluation against the test set.

Model Comparison
========================================================




Testing
=======================================================

Next Steps
=======================================================

* Try alternative feature selection methods (e.g. Information Gain)
* Compare to multivariate SPC models (T^2, MEWMA)
