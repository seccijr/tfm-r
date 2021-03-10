library(sas7bdat)
library(nnet)
library(h2o)
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
  read.sas7bdat("C:\\Users\\secci\\Workspace\\TFM\\Lib\\mind_dummy.sas7bdat")
dput(names(mind))

mind <- na.omit(mind, (!is.na(mind)))
mind$result <-
  factor(mind$result,
         levels = c(0, 1),
         labels = c("No", "Yes"))

vardep <- c("REP_clicked")
continuas <- NULL
categoricas <- c(
  "TI_TG_REP_P1340_tail1",
  "TI_TG_REP_P1340_tail2",
  "TI_TG_REP_P1884_tail1",
  "TI_TG_REP_P1884_tail2",
  "TI_TG_REP_P21_tail1",
  "TI_TG_REP_P21_tail2",
  "TI_TG_REP_P21_tail3",
  "TI_TG_REP_P21_tail4",
  "TI_TG_REP_P30_tail1",
  "TI_TG_REP_P30_tail2",
  "TI_TG_REP_P30_tail3",
  "TI_TG_REP_P5008_tail1",
  "TI_TG_REP_P5008_tail2",
  "TI_TG_REP_P5008_tail3",
  "TI_TG_REP_category1",
  "TI_TG_REP_category2",
  "TI_TG_REP_category3",
  "TI_TG_REP_category4"
)

logistic <- cruzadalogistica(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 12345,
  repe = 31
)
logistic$modelo = "logistic"

svmrbf <- cruzadaSVMbinRBF(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 12345,
  repe = 31,
  C = 100,
  sigma = 100
)

svmrbf$modelo = "svm-rbf"

svmpolr2 <- cruzadaSVMbinPoly(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  C = 10,
  degree = 2,
  scale = 1
)

svmpolr2$modelo = "svm-pol-r2"

svmpolr3 <- cruzadaSVMbinPoly(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  C = 10,
  degree = 3,
  scale = 1
)

svmpolr3$modelo = "svm-pol-r3"

svmpolr3c100 <- cruzadaSVMbinPoly(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  C = 100,
  degree = 3,
  scale = 1
)

svmpolr3c100$modelo = "svm-pol-r3-c100"

svmpolr4 <- cruzadaSVMbinPoly(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  C = 10,
  degree = 4,
  scale = 1
)

svmpolr4$modelo = "svm-pol-r4"

avvnet <- cruzadaavnnetbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  size = c(5),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avvnet$modelo = "avnnet"

avvnetn2 <- cruzadaavnnetbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  size = c(2),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avvnetn2$modelo = "avnnet-n2"

avvnetn3 <- cruzadaavnnetbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  size = c(3),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avvnetn3$modelo = "avnnet-n3"

avvnetn4 <- cruzadaavnnetbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  size = c(4),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avvnetn4$modelo = "avnnet-n4"

avvnetn6 <- cruzadaavnnetbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  size = c(6),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avvnetn6$modelo = "avnnet-n6"

avvnetn7 <- cruzadaavnnetbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  size = c(7),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avvnetn7$modelo = "avnnet-n7"

avvnetn8 <- cruzadaavnnetbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  size = c(8),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avvnetn8$modelo = "avnnet-n8"

avvnetn9 <- cruzadaavnnetbin(
  data = mind,
  vardep = vardep,
  listconti = continuas,
  listclass = categoricas,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  size = c(9),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avvnetn9$modelo = "avnnet-n9"

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
  rbind(logistic,
        #svmrbf,
        svmpolr2,
        svmpolr3,
        svmpolr3c100,
        svmpolr4,
        avvnet,
        avvnetn2,
        avvnetn3,
        avvnetn4,
        avvnetn6,
        avvnetn7,
        avvnetn8,
        avvnetn9,
        arbol,
        bagging,
        randomforest,
        randomforestm2,
        gradientboosting,
        gradientboosting003d4,
        gradientboosting01d4,
        xgboost)


par(cex.axis = 0.8)
boxplot(data = union, tasa ~ modelo, main = "TASA FALLOS")
