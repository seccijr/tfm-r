library(sas7bdat)
library(nnet)
library(dummies)
library(MASS)
library(reshape)
library(caret)

source("../cruzada SVM binaria lineal.R")
source("../cruzada SVM binaria polinomial.R")
source("../cruzada SVM binaria RBF.R")
source("../cruzadas avnnet y log binaria.R")
source("../cruzada gbm binaria.R")
source("../cruzada rf binaria.R")
source("../cruzada xgboost binaria.R")
source("../cruzada arbolbin.R")

news_reduced_clean_two_methods <-
  read.sas7bdat("../../Lib/news_reduced_clean_two_methods.sas7bdat")

news_reduced_clean_two_methods$REP_clicked <- factor(news_reduced_clean_two_methods$REP_clicked, levels = c(0, 1), labels = c("No", "Yes"))

news_reduced_clean_two_methods$REP_P102_head <- factor(news_reduced_clean_two_methods$REP_P102_head)
news_reduced_clean_two_methods$REP_P103_head <- factor(news_reduced_clean_two_methods$REP_P103_head)
news_reduced_clean_two_methods$REP_P103_tail <- factor(news_reduced_clean_two_methods$REP_P103_tail)
news_reduced_clean_two_methods$REP_P1050_head <- factor(news_reduced_clean_two_methods$REP_P1050_head)
news_reduced_clean_two_methods$REP_P1050_tail <- factor(news_reduced_clean_two_methods$REP_P1050_tail)
news_reduced_clean_two_methods$REP_P106_head <- factor(news_reduced_clean_two_methods$REP_P106_head)
news_reduced_clean_two_methods$REP_P106_tail <- factor(news_reduced_clean_two_methods$REP_P106_tail)
news_reduced_clean_two_methods$REP_P118_head <- factor(news_reduced_clean_two_methods$REP_P118_head)
news_reduced_clean_two_methods$REP_P131_tail <- factor(news_reduced_clean_two_methods$REP_P131_tail)
news_reduced_clean_two_methods$REP_P1340_tail <- factor(news_reduced_clean_two_methods$REP_P1340_tail)
news_reduced_clean_two_methods$REP_P1343_head <- factor(news_reduced_clean_two_methods$REP_P1343_head)
news_reduced_clean_two_methods$REP_P1343_tail <- factor(news_reduced_clean_two_methods$REP_P1343_tail)
news_reduced_clean_two_methods$REP_P1412_head <- factor(news_reduced_clean_two_methods$REP_P1412_head)
news_reduced_clean_two_methods$REP_P1412_tail <- factor(news_reduced_clean_two_methods$REP_P1412_tail)
news_reduced_clean_two_methods$REP_P1532_head <- factor(news_reduced_clean_two_methods$REP_P1532_head)
news_reduced_clean_two_methods$REP_P1622_head <- factor(news_reduced_clean_two_methods$REP_P1622_head)
news_reduced_clean_two_methods$REP_P166_head <- factor(news_reduced_clean_two_methods$REP_P166_head)
news_reduced_clean_two_methods$REP_P166_tail <- factor(news_reduced_clean_two_methods$REP_P166_tail)
news_reduced_clean_two_methods$REP_P172_head <- factor(news_reduced_clean_two_methods$REP_P172_head)
news_reduced_clean_two_methods$REP_P172_tail <- factor(news_reduced_clean_two_methods$REP_P172_tail)
news_reduced_clean_two_methods$REP_P17_tail <- factor(news_reduced_clean_two_methods$REP_P17_tail)
news_reduced_clean_two_methods$REP_P1884_tail <- factor(news_reduced_clean_two_methods$REP_P1884_tail)
news_reduced_clean_two_methods$REP_P206_head <- factor(news_reduced_clean_two_methods$REP_P206_head)
news_reduced_clean_two_methods$REP_P206_tail <- factor(news_reduced_clean_two_methods$REP_P206_tail)
news_reduced_clean_two_methods$REP_P21_head <- factor(news_reduced_clean_two_methods$REP_P21_head)
news_reduced_clean_two_methods$REP_P27_tail <- factor(news_reduced_clean_two_methods$REP_P27_tail)
news_reduced_clean_two_methods$REP_P2936_head <- factor(news_reduced_clean_two_methods$REP_P2936_head)
news_reduced_clean_two_methods$REP_P2936_tail <- factor(news_reduced_clean_two_methods$REP_P2936_tail)
news_reduced_clean_two_methods$REP_P30_head <- factor(news_reduced_clean_two_methods$REP_P30_head)
news_reduced_clean_two_methods$REP_P30_tail <- factor(news_reduced_clean_two_methods$REP_P30_tail)
news_reduced_clean_two_methods$REP_P31_head <- factor(news_reduced_clean_two_methods$REP_P31_head)
news_reduced_clean_two_methods$REP_P31_tail <- factor(news_reduced_clean_two_methods$REP_P31_tail)
news_reduced_clean_two_methods$REP_P361_head <- factor(news_reduced_clean_two_methods$REP_P361_head)
news_reduced_clean_two_methods$REP_P361_tail <- factor(news_reduced_clean_two_methods$REP_P361_tail)
news_reduced_clean_two_methods$REP_P37_head <- factor(news_reduced_clean_two_methods$REP_P37_head)
news_reduced_clean_two_methods$REP_P37_tail <- factor(news_reduced_clean_two_methods$REP_P37_tail)
news_reduced_clean_two_methods$REP_P39_tail <- factor(news_reduced_clean_two_methods$REP_P39_tail)
news_reduced_clean_two_methods$REP_P421_tail <- factor(news_reduced_clean_two_methods$REP_P421_tail)
news_reduced_clean_two_methods$REP_P463_tail <- factor(news_reduced_clean_two_methods$REP_P463_tail)
news_reduced_clean_two_methods$REP_P495_head <- factor(news_reduced_clean_two_methods$REP_P495_head)
news_reduced_clean_two_methods$REP_P5008_head <- factor(news_reduced_clean_two_methods$REP_P5008_head)
news_reduced_clean_two_methods$REP_P5008_tail <- factor(news_reduced_clean_two_methods$REP_P5008_tail)
news_reduced_clean_two_methods$REP_P552_tail <- factor(news_reduced_clean_two_methods$REP_P552_tail)
news_reduced_clean_two_methods$REP_P641_tail <- factor(news_reduced_clean_two_methods$REP_P641_tail)
news_reduced_clean_two_methods$REP_P6886_head <- factor(news_reduced_clean_two_methods$REP_P6886_head)
news_reduced_clean_two_methods$REP_P6886_tail <- factor(news_reduced_clean_two_methods$REP_P6886_tail)
news_reduced_clean_two_methods$REP_P937_head <- factor(news_reduced_clean_two_methods$REP_P937_head)
news_reduced_clean_two_methods$REP_P937_tail <- factor(news_reduced_clean_two_methods$REP_P937_tail)
news_reduced_clean_two_methods$REP_category <- factor(news_reduced_clean_two_methods$REP_category)
news_reduced_clean_two_methods$REP_subcategory <- factor(news_reduced_clean_two_methods$REP_subcategory)


vardep <- c("REP_clicked")
categoricas <- c(
  "REP_P102_head",
  "REP_P103_head",
  "REP_P103_tail",
  "REP_P1050_head",
  "REP_P1050_tail",
  "REP_P106_head",
  "REP_P106_tail",
  "REP_P118_head",
  "REP_P131_tail",
  "REP_P1340_tail",
  "REP_P1343_head",
  "REP_P1343_tail",
  "REP_P1412_head",
  "REP_P1412_tail",
  "REP_P1532_head",
  "REP_P1622_head",
  "REP_P166_head",
  "REP_P166_tail",
  "REP_P172_head",
  "REP_P172_tail",
  "REP_P17_tail",
  "REP_P1884_tail",
  "REP_P206_head",
  "REP_P206_tail",
  "REP_P21_head",
  "REP_P27_tail",
  "REP_P2936_head",
  "REP_P2936_tail",
  "REP_P30_head",
  "REP_P30_tail",
  "REP_P31_head",
  "REP_P31_tail",
  "REP_P361_head",
  "REP_P361_tail",
  "REP_P37_head",
  "REP_P37_tail",
  "REP_P39_tail",
  "REP_P421_tail",
  "REP_P463_tail",
  "REP_P495_head",
  "REP_P5008_head",
  "REP_P5008_tail",
  "REP_P552_tail",
  "REP_P641_tail",
  "REP_P6886_head",
  "REP_P6886_tail",
  "REP_P937_head",
  "REP_P937_tail",
  "REP_category",
  "REP_subcategory"
)

# Logistic

logistic <- cruzadalogistica(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 12345,
  repe = 10
)

logistic$modelo = "ll-v2"

# avNNet 

avnnetn2c01 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(2),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avnnetn2c01$modelo = "avnnet-n2-c0.1-v2"

avnnetn2c001 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(2),
  decay = c(0.01),
  repeticiones = 5,
  itera = 200
)

avnnetn2c001$modelo = "avnnet-n2-c0.01-v2"

avnnetn3c01 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(3),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avnnetn3c01$modelo = "avnnet-n3-c0.1-v2"

avnnetn3c001 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(3),
  decay = c(0.01),
  repeticiones = 5,
  itera = 200
)

avnnetn3c001$modelo = "avnnet-n3-c0.01-v2"

avnnetn4c01 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(4),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avnnetn4c01$modelo = "avnnet-n4-c0.1-v2"

avnnetn4c001 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(4),
  decay = c(0.01),
  repeticiones = 5,
  itera = 200
)

avnnetn4c001$modelo = "avnnet-n4-c0.01-v2"

avnnetn5c01 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(5),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avnnetn5c01$modelo = "avnnet-n5-c0.1-v2"

avnnetn5c001 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(5),
  decay = c(0.01),
  repeticiones = 5,
  itera = 200
)

avnnetn5c001$modelo = "avnnet-n5-c0.01-v2"

avnnetn6c01 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(6),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avnnetn6c01$modelo = "avnnet-n6-c0.1-v2"

avnnetn6c001 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(6),
  decay = c(0.01),
  repeticiones = 5,
  itera = 200
)

avnnetn6c001$modelo = "avnnet-n6-c0.01-v2"

avnnetn7c01 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(7),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avnnetn7c01$modelo = "avnnet-n7-c0.1-v2"

avnnetn7c001 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(7),
  decay = c(0.01),
  repeticiones = 5,
  itera = 200
)

avnnetn7c001$modelo = "avnnet-n7-c0.01-v2"

avnnetn8c01 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(8),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avnnetn8c01$modelo = "avnnet-n8-c0.1-v2"

avnnetn8c001 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(8),
  decay = c(0.01),
  repeticiones = 5,
  itera = 200
)

avnnetn8c001$modelo = "avnnet-n8-c0.01-v2"

avnnetn9c01 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(9),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avnnetn9c01$modelo = "avnnet-n9-c0.1-v2"

avnnetn9c001 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(9),
  decay = c(0.01),
  repeticiones = 5,
  itera = 200
)

avnnetn9c001$modelo = "avnnet-n9-c0.01-v2"

avnnetn10c01 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(10),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avnnetn10c01$modelo = "avnnet-n10-c0.1-v2"

avnnetn10c001 <- cruzadaavnnetbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  size = c(10),
  decay = c(0.01),
  repeticiones = 5,
  itera = 200
)

avnnetn10c001$modelo = "avnnet-n10-c0.01-v2"

arbol <- cruzadaarbolbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  cp = c(0),
  minbucket = 5
)

arbol$modelo = "arbol-v2"

bagging <- cruzadarfbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  nodesize = 74,
  mtry = 6,
  ntree = 50,
  replace = TRUE
)

bagging$modelo = "bagging-v2"

randomforest <- cruzadarfbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  nodesize = 74,
  mtry = 3,
  ntree = 50,
  replace = TRUE
)

randomforest$modelo = "rf-v2"

randomforestm2 <- cruzadarfbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  nodesize = 74,
  mtry = 2,
  ntree = 50,
  replace = TRUE
)

randomforestm2$modelo = "rf-m2-v2"

gradientboosting <- cruzadagbmbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  n.minobsinnode = 74,
  shrinkage = 0.008,
  n.trees = 250,
  interaction.depth = 2
)

gradientboosting$modelo = "gbm-v2"


gradientboosting01d4 <- cruzadagbmbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  n.minobsinnode = 74,
  shrinkage = 0.1,
  n.trees = 250,
  interaction.depth = 4
)

gradientboosting01d4$modelo = "gbm-01-d4-v2"

gradientboosting003d4 <- cruzadagbmbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  n.minobsinnode = 74,
  shrinkage = 0.03,
  n.trees = 250,
  interaction.depth = 4
)

gradientboosting003d4$modelo = "gbm-003-d4-v2"

xgboost <- cruzadaxgbmbin(
  data = news_reduced_clean_two_methods,
  vardep = vardep,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 10,
  min_child_weight = 74,
  eta = 0.001,
  nrounds = 1250,
  max_depth = 6,
  gamma = 0,
  colsample_bytree = 1,
  subsample = 1,
  alpha = 0,
  lambda = 0,
  lambda_bias = 0
)

xgboost$modelo = "xgbm-v2"

union <-
  rbind(
        logistic,
        svmrbf,
        svmpolr2,
        svmpolr3,
        svmpolr3c100,
        svmpolr4,
        avnnetn2c01,
        avnnetn2c001,
        avnnetn3c01,
        avnnetn3c001,
        avnnetn4c01,
        avnnetn4c001,
        avnnetn5c01,
        avnnetn5c001,
        avnnetn6c01,
        avnnetn6c001,
        avnnetn7c01,
        avnnetn7c001,
        avnnetn8c01,
        avnnetn8c001,
        avnnetn9c01,
        avnnetn9c001,
        avnnetn10c01,
        avnnetn10c001,
        arbol,
        bagging,
        randomforest,
        randomforestm2,
        gradientboosting,
        gradientboosting01d4,
        gradientboosting003d4,
        xgboost
  )


par(cex.axis = 0.8)
boxplot(data = union, auc ~ modelo, main = "AUC")
