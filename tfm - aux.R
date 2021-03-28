library(sas7bdat)
library(nnet)
library(dummies)
library(MASS)
library(reshape)
library(caret)
source("cruzada SVM binaria lineal.R")
source("cruzada SVM binaria polinomial.R")
source("cruzada SVM binaria RBF.R")
source("cruzadas avnnet y log binaria.R")
source("cruzada gbm binaria.R")
source("cruzada rf binaria.R")
source("cruzada xgboost binaria.R")
source("cruzada arbolbin.R")

news_reduced_clean <-
  read.sas7bdat("C:\\Users\\secci\\Workspace\\TFM\\Lib\\news_reduced_clean.sas7bdat")
dput(names(news_reduced_clean))

news_reduced_dummy <-
  read.sas7bdat("C:\\Users\\secci\\Workspace\\TFM\\Lib\\news_reduced_dummy.sas7bdat")
dput(names(news_reduced_dummy))

news_reduced_rare_dummy <-
  read.sas7bdat("C:\\Users\\secci\\Workspace\\TFM\\Lib\\news_reduced_rare_dummy.sas7bdat")
dput(names(news_reduced_rare_dummy))

news_reduced_dummy_select <-
  read.sas7bdat("C:\\Users\\secci\\Workspace\\TFM\\Lib\\news_reduced_dummy_select.sas7bdat")
dput(names(news_reduced_dummy_select))

news_reduced_rare_dummy_group <-
  read.sas7bdat("C:\\Users\\secci\\Workspace\\TFM\\Lib\\news_reduced_rare_dummy_group.sas7bdat")
dput(names(news_reduced_rare_dummy_group))