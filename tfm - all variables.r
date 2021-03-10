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

mind <-
  read.sas7bdat("C:\\Users\\secci\\Workspace\\TFM\\Lib\\news.sas7bdat")
dput(names(mind))

mind <- na.omit(mind, (!is.na(mind)))
mind$result <-
  factor(mind$clicked)

vardep <- c("clicked")
continuas <- NULL
categoricas <- c(
  "category",
  "subcategory",
  "P102_head",
  "P102_tail",
  "P103_head",
  "P103_tail",
  "P1050_head",
  "P1050_tail",
  "P106_head",
  "P106_tail",
  "P118_head",
  "P118_tail",
  "P131_head",
  "P131_tail",
  "P1340_head",
  "P1340_tail",
  "P1343_head",
  "P1343_tail",
  "P1411_head",
  "P1411_tail",
  "P1412_head",
  "P1412_tail",
  "P1532_head",
  "P1532_tail",
  "P1622_head",
  "P1622_tail",
  "P166_head",
  "P166_tail",
  "P17_head",
  "P17_tail",
  "P172_head",
  "P172_tail",
  "P1884_head",
  "P1884_tail",
  "P206_head",
  "P206_tail",
  "P21_head",
  "P21_tail",
  "P27_head",
  "P27_tail",
  "P2936_head",
  "P2936_tail",
  "P30_head",
  "P30_tail",
  "P31_head",
  "P31_tail",
  "P361_head",
  "P361_tail",
  "P37_head",
  "P37_tail",
  "P39_head",
  "P39_tail",
  "P421_head",
  "P421_tail",
  "P463_head",
  "P463_tail",
  "P495_head",
  "P495_tail",
  "P5008_head",
  "P5008_tail",
  "P530_head",
  "P530_tail",
  "P552_head",
  "P552_tail",
  "P641_head",
  "P641_tail",
  "P6886_head",
  "P6886_tail",
  "P937_head",
  "P937_tail"
)

arbol <- cruzadaarbolbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  cp = c(0),
  minbucket = 5
)

arbol$modelo = "arbol"


bagging <- cruzadarfbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  nodesize = 10,
  mtry = 6,
  ntree = 200,
  replace = TRUE
)

bagging$modelo = "bagging"

randomforest <- cruzadarfbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  nodesize = 10,
  mtry = 3,
  ntree = 200,
  replace = TRUE
)

randomforest$modelo = "rf"

randomforestm2 <- cruzadarfbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  nodesize = 10,
  mtry = 2,
  ntree = 200,
  replace = TRUE
)

randomforestm2$modelo = "rf-m2"

gradientboosting <- cruzadagbmbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  n.minobsinnode = 10,
  shrinkage = 0.008,
  n.trees = 1000,
  interaction.depth = 2
)

gradientboosting$modelo = "gbm"


gradientboosting01d4 <- cruzadagbmbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  n.minobsinnode = 10,
  shrinkage = 0.1,
  n.trees = 1000,
  interaction.depth = 4
)

gradientboosting01d4$modelo = "gbm-01-d4"

gradientboosting003d4 <- cruzadagbmbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  n.minobsinnode = 10,
  shrinkage = 0.03,
  n.trees = 1000,
  interaction.depth = 4
)

gradientboosting003d4$modelo = "gbm-003-d4"

xgboost <- cruzadaxgbmbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  min_child_weight = 10,
  eta = 0.001,
  nrounds = 5000,
  max_depth = 6,
  gamma = 0,
  colsample_bytree = 1,
  subsample = 1,
  alpha = 0,
  lambda = 0,
  lambda_bias = 0
)

xgboost$modelo = "xgbm"

union <-
  rbind(arbol,
        bagging,
        randomforest,
        randomforestm2,
        gradientboosting,
        gradientboosting003d4,
        gradientboosting01d4,
        xgboost)


par(cex.axis = 0.8)
boxplot(data = union, tasa ~ modelo, main = "TASA FALLOS")
