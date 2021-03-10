library(plyr)
detach(package:plyr)
library(dummies)
library(MASS)
library(reshape)
library(caret)
library(dplyr)
library(pROC)


cruzadalogistica <- function(data = data,
                             vardep = NULL,
                             listconti = NULL,
                             listclass = NULL,
                             grupos = 4,
                             sinicio = 1234,
                             repe = 5) {
  if (listclass != c("")) {
    for (i in 1:dim(array(listclass))) {
      numindi <- which(names(data) == listclass[[i]])
      data[, numindi] <- as.character(data[, numindi])
      data[, numindi] <- as.factor(data[, numindi])
    }
  }
  
  data[, vardep] <- as.factor(data[, vardep])
  
  
  if (listclass != c("")) {
    koko <- c(listconti, listclass)
  }  else   {
    koko <- c(listconti)
  }
  
  modelo <- paste(koko, sep = "", collapse = "+")
  formu <- formula(paste(vardep, "~", modelo, sep = ""))
  
  
  set.seed(sinicio)
  control <-
    trainControl(
      method = "repeatedcv",
      number = grupos,
      repeats = repe,
      savePredictions = "all",
      classProbs = TRUE
    )
  
  
  regresion <- train(
    formu,
    data = data,
    trControl = control,
    method = "glm",
    family = binomial(link = "logit")
  )
  preditest <- regresion$pred
  
  preditest$prueba <- strsplit(preditest$Resample, "[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba <- NULL
  
  tasafallos <- function(x, y) {
    confu <- confusionMatrix(x, y)
    tasa <- confu[[3]][1]
    return(tasa)
  }
  
  
  medias <- preditest %>%
    group_by(Rep) %>%
    summarize(tasa = 1 - tasafallos(pred, obs))
  
  
  auc <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc <- curvaroc$auc
    return(auc)
  }
  
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc(obs, Yes))
  
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}


cruzadaavnnetbin <-  function(data = data,
                              vardep = "vardep",
                              listconti = "listconti",
                              listclass = "listclass",
                              grupos = 4,
                              sinicio = 1234,
                              repe = 5,
                              size = c(5),
                              decay = c(0.01),
                              repeticiones = 5,
                              itera = 100,
                              trace = FALSE) {
  if (listclass != c("")) {
    databis <- data[, c(vardep, listconti, listclass)]
    databis <- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
    databis <- data[, c(vardep, listconti)]
  }
  
  means <- apply(databis[, listconti], 2, mean)
  sds <- sapply(databis[, listconti], sd)
  
  datacon <-
    scale(databis[, listconti], center = means, scale = sds)
  numerocont <- which(colnames(databis) %in% listconti)
  databis <- cbind(datacon, databis[, -numerocont, drop = FALSE])
  
  databis[, vardep] <- as.factor(databis[, vardep])
  
  formu <- formula(paste(vardep, "~.", sep = ""))
  
  set.seed(sinicio)
  control <-
    trainControl(
      method = "repeatedcv",
      number = grupos,
      repeats = repe,
      savePredictions = "all",
      classProbs = TRUE
    )
  
  
  avnnetgrid <-  expand.grid(size = size,
                             decay = decay,
                             bag = FALSE)
  
  avnnet <- train(
    formu,
    data = databis,
    method = "avNNet",
    linout = FALSE,
    maxit = itera,
    repeats = repeticiones,
    trControl = control,
    tuneGrid = avnnetgrid,
    trace = trace
  )
  
  preditest <- avnnet$pred
  
  preditest$prueba <- strsplit(preditest$Resample, "[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba <- NULL
  
  tasafallos <- function(x, y) {
    confu <- confusionMatrix(x, y)
    tasa <- confu[[3]][1]
    return(tasa)
  }
  
  
  medias <- preditest %>%
    group_by(Rep) %>%
    summarize(tasa = 1 - tasafallos(pred, obs))
  
  
  auc <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc <- curvaroc$auc
    return(auc)
  }
  
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc(obs, Yes))
  
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}


cruzadarfbin <- function(data = data,
                         vardep = "vardep",
                         listconti = "listconti",
                         listclass = "listclass",
                         grupos = 4,
                         sinicio = 1234,
                         repe = 5,
                         nodesize = 20,
                         mtry = 2,
                         ntree = 50,
                         replace = TRUE,
                         sampsize = 1) {

  if (listclass != c("")) {
    databis <- data[, c(vardep, listconti, listclass)]
    databis <- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
    databis <- data[, c(vardep, listconti)]
  }
  
  
  means <- apply(databis[, listconti], 2, mean)
  sds <- sapply(databis[, listconti], sd)
  
  
  datacon <-
    scale(databis[, listconti], center = means, scale = sds)
  numerocont <- which(colnames(databis) %in% listconti)
  databis <- cbind(datacon, databis[, -numerocont, drop = FALSE])
  
  databis[, vardep] <- as.factor(databis[, vardep])
  
  formu <- formula(paste("factor(", vardep, ")~.", sep = ""))
  
  
  set.seed(sinicio)
  control <-
    trainControl(
      method = "repeatedcv",
      number = grupos,
      repeats = repe,
      savePredictions = "all",
      classProbs = TRUE
    )
  
  
  rfgrid <- expand.grid(mtry = mtry)
  
  if (sampsize == 1) {
    rf <- train(
      formu,
      data = databis,
      method = "rf",
      trControl = control,
      tuneGrid = rfgrid,
      nodesize = nodesize,
      replace = replace,
      ntree = ntree
    )
  } else  if (sampsize != 1) {
    rf <- train(
      formu,
      data = databis,
      method = "rf",
      trControl = control,
      tuneGrid = rfgrid,
      nodesize = nodesize,
      replace = replace,
      sampsize = sampsize,
      ntree = ntree
    )
  }
  
  preditest <- rf$pred
  
  preditest$prueba <- strsplit(preditest$Resample, "[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba <- NULL
  
  tasafallos <- function(x, y) {
    confu <- confusionMatrix(x, y)
    tasa <- confu[[3]][1]
    return(tasa)
  }
  
  
  medias <- preditest %>%
    group_by(Rep) %>%
    summarize(tasa = 1 - tasafallos(pred, obs))
  
  
  auc <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc <- curvaroc$auc
    return(auc)
  }
  
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc(obs, Yes))
  
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}


cruzadagbmbin <- function(data = data,
                          vardep = "vardep",
                          listconti = "listconti",
                          listclass = "listclass",
                          grupos = 4,
                          sinicio = 1234,
                          repe = 5,
                          n.minobsinnode = 20,
                          shrinkage = 0.1,
                          n.trees = 100,
                          interaction.depth = 2) {
  if (listclass != c("")) {
    databis <- data[, c(vardep, listconti, listclass)]
    databis <- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
    databis <- data[, c(vardep, listconti)]
  }
  
  means <- apply(databis[, listconti], 2, mean)
  sds <- sapply(databis[, listconti], sd)
  
  datacon <-
    scale(databis[, listconti], center = means, scale = sds)
  numerocont <- which(colnames(databis) %in% listconti)
  databis <- cbind(datacon, databis[, -numerocont, drop = FALSE])
  
  databis[, vardep] <- as.factor(databis[, vardep])
  
  formu <- formula(paste("factor(", vardep, ")~.", sep = ""))
  
  set.seed(sinicio)
  control <-
    trainControl(
      method = "repeatedcv",
      number = grupos,
      repeats = repe,
      savePredictions = "all",
      classProbs = TRUE
    )
  
  gbmgrid <- expand.grid(
    n.minobsinnode = n.minobsinnode,
    shrinkage = shrinkage,
    n.trees = n.trees,
    interaction.depth = interaction.depth
  )
  
  gbm <- train(
    formu,
    data = databis,
    method = "gbm",
    trControl = control,
    tuneGrid = gbmgrid,
    distribution = "bernoulli",
    verbose = FALSE
  )
  
  preditest <- gbm$pred
  
  
  preditest$prueba <- strsplit(preditest$Resample, "[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba <- NULL
  
  tasafallos <- function(x, y) {
    confu <- confusionMatrix(x, y)
    tasa <- confu[[3]][1]
    return(tasa)
  }
  
  
  medias <- preditest %>%
    group_by(Rep) %>%
    summarize(tasa = 1 - tasafallos(pred, obs))
  
  
  auc <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc <- curvaroc$auc
    return(auc)
  }
  
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc(obs, Yes))
  
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}

cruzadaxgbmbin <- function(data = data,
                           vardep = "vardep",
                           listconti = "listconti",
                           listclass = "listclass",
                           grupos = 4,
                           sinicio = 1234,
                           repe = 5,
                           min_child_weight = 20,
                           eta = 0.1,
                           nrounds = 100,
                           max_depth = 2,
                           gamma = 0,
                           colsample_bytree = 1,
                           subsample = 1,
                           alpha = 0,
                           lambda = 0,
                           lambda_bias = 0) {
  
  if (listclass != c("")) {
    databis <- data[, c(vardep, listconti, listclass)]
    databis <- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
    databis <- data[, c(vardep, listconti)]
  }
  
  
  
  means <- apply(databis[, listconti], 2, mean)
  sds <- sapply(databis[, listconti], sd)
  
  
  datacon <-
    scale(databis[, listconti], center = means, scale = sds)
  numerocont <- which(colnames(databis) %in% listconti)
  databis <- cbind(datacon, databis[, -numerocont, drop = FALSE])
  
  databis[, vardep] <- as.factor(databis[, vardep])
  
  formu <- formula(paste("factor(", vardep, ")~.", sep = ""))
  
  
  set.seed(sinicio)
  control <-
    trainControl(
      method = "repeatedcv",
      number = grupos,
      repeats = repe,
      savePredictions = "all",
      classProbs = TRUE
    )
  
  
  xgbmgrid <- expand.grid(
    min_child_weight = min_child_weight,
    eta = eta,
    nrounds = nrounds,
    max_depth = max_depth,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    subsample = subsample
  )
  
  xgbm <- train(
    formu,
    data = databis,
    method = "xgbTree",
    trControl = control,
    tuneGrid = xgbmgrid,
    objective = "binary:logistic",
    verbose = FALSE,
    alpha = alpha,
    lambda = lambda,
    lambda_bias = lambda_bias
  )
  
  preditest <- xgbm$pred
  
  
  preditest$prueba <- strsplit(preditest$Resample, "[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba <- NULL
  
  tasafallos <- function(x, y) {
    confu <- confusionMatrix(x, y)
    tasa <- confu[[3]][1]
    return(tasa)
  }
  
  
  medias <- preditest %>%
    group_by(Rep) %>%
    summarize(tasa = 1 - tasafallos(pred, obs))
  
  
  auc <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc <- curvaroc$auc
    return(auc)
  }
  
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc(obs, Yes))
  
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}



cruzadaSVMbin <-  function(data = data,
                           vardep = "vardep",
                           listconti = "listconti",
                           listclass = "listclass",
                           grupos = 4,
                           sinicio = 1234,
                           repe = 5,
                           C = 1,
                           replace = TRUE) {

  if (listclass != c("")) {
    databis <- data[, c(vardep, listconti, listclass)]
    databis <- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
    databis <- data[, c(vardep, listconti)]
  }
  
  
  
  means <- apply(databis[, listconti], 2, mean)
  sds <- sapply(databis[, listconti], sd)
  
  
  datacon <-
    scale(databis[, listconti], center = means, scale = sds)
  numerocont <- which(colnames(databis) %in% listconti)
  databis <- cbind(datacon, databis[, -numerocont, drop = FALSE])
  
  databis[, vardep] <- as.factor(databis[, vardep])
  
  formu <- formula(paste("factor(", vardep, ")~.", sep = ""))
  
  
  set.seed(sinicio)
  control <-
    trainControl(
      method = "repeatedcv",
      number = grupos,
      repeats = repe,
      savePredictions = "all",
      classProbs = TRUE
    )
  
  
  SVMgrid <- expand.grid(C = C)
  
  SVM <- train(
    formu,
    data = databis,
    method = "svmLinear",
    trControl = control,
    tuneGrid = SVMgrid,
    replace = replace
  )
  
  preditest <- SVM$pred
  
  preditest$prueba <- strsplit(preditest$Resample, "[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba <- NULL
  
  tasafallos <- function(x, y) {
    confu <- confusionMatrix(x, y)
    tasa <- confu[[3]][1]
    return(tasa)
  }
  
  
  medias <- preditest %>%
    group_by(Rep) %>%
    summarize(tasa = 1 - tasafallos(pred, obs))
  
  
  auc <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc <- curvaroc$auc
    return(auc)
  }
  
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc(obs, Yes))
  
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}

cruzadaSVMbinPoly <- function(data = data,
                              vardep = "vardep",
                              listconti = "listconti",
                              listclass = "listclass",
                              grupos = 4,
                              sinicio = 1234,
                              repe = 5,
                              C = 1,
                              degree = 2,
                              scale = 1) {
  if (listclass != c("")) {
    databis <- data[, c(vardep, listconti, listclass)]
    databis <- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
    databis <- data[, c(vardep, listconti)]
  }
  
  means <- apply(databis[, listconti], 2, mean)
  sds <- sapply(databis[, listconti], sd)
  
  datacon <-
    scale(databis[, listconti], center = means, scale = sds)
  numerocont <- which(colnames(databis) %in% listconti)
  databis <- cbind(datacon, databis[, -numerocont, drop = FALSE])
  
  databis[, vardep] <- as.factor(databis[, vardep])
  
  formu <- formula(paste("factor(", vardep, ")~.", sep = ""))
  
  set.seed(sinicio)
  control <-
    trainControl(
      method = "repeatedcv",
      number = grupos,
      repeats = repe,
      savePredictions = "all",
      classProbs = TRUE
    )
  
  SVMgrid <- expand.grid(C = C,
                         degree = degree,
                         scale = scale)
  
  SVM <- train(
    formu,
    data = databis,
    method = "svmPoly",
    trControl = control,
    tuneGrid = SVMgrid,
    replace = replace
  )
  
  preditest <- SVM$pred
  
  preditest$prueba <- strsplit(preditest$Resample, "[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba <- NULL
  
  tasafallos <- function(x, y) {
    confu <- confusionMatrix(x, y)
    tasa <- confu[[3]][1]
    return(tasa)
  }
  
  medias <- preditest %>%
    group_by(Rep) %>%
    summarize(tasa = 1 - tasafallos(pred, obs))
  
  auc <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc <- curvaroc$auc
    return(auc)
  }
  
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc(obs, Yes))
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}

cruzadaSVMbinRBF <- function(data = data,
                             vardep = "vardep",
                             listconti = "listconti",
                             listclass = "listclass",
                             grupos = 4,
                             sinicio = 1234,
                             repe = 5,
                             C = 1,
                             sigma = 1) {
  
  if (listclass != c("")) {
    databis <- data[, c(vardep, listconti, listclass)]
    databis <- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
    databis <- data[, c(vardep, listconti)]
  }
  
  means <- apply(databis[, listconti], 2, mean)
  sds <- sapply(databis[, listconti], sd)
  
  datacon <-
    scale(databis[, listconti], center = means, scale = sds)
  numerocont <- which(colnames(databis) %in% listconti)
  databis <- cbind(datacon, databis[, -numerocont, drop = FALSE])
  
  databis[, vardep] <- as.factor(databis[, vardep])
  
  formu <- formula(paste("factor(", vardep, ")~.", sep = ""))
  
  set.seed(sinicio)
  control <-
    trainControl(
      method = "repeatedcv",
      number = grupos,
      repeats = repe,
      savePredictions = "all",
      classProbs = TRUE
    )
  
  SVMgrid <- expand.grid(C = C, sigma = sigma)
  
  SVM <- train(
    formu,
    data = databis,
    method = "svmRadial",
    trControl = control,
    tuneGrid = SVMgrid,
    replace = replace
  )
  
  preditest <- SVM$pred
  
  preditest$prueba <- strsplit(preditest$Resample, "[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba <- NULL
  
  tasafallos <- function(x, y) {
    confu <- confusionMatrix(x, y)
    tasa <- confu[[3]][1]
    return(tasa)
  }
  
  medias <- preditest %>%
    group_by(Rep) %>%
    summarize(tasa = 1 - tasafallos(pred, obs))
  
  auc <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc <- curvaroc$auc
    return(auc)
  }
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc(obs, Yes))
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}
