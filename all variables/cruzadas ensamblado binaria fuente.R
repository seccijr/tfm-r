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
                             listclass = NULL,
                             grupos = 4,
                             sinicio = 1234,
                             repe = 5) {
  
  databis <- data[, c(vardep, listclass)]
  databis <- dummy.data.frame(databis, listclass, sep = ".")
  
  x <- databis[, names(databis) != vardep]
  y <- as.factor(databis[, vardep])
  
  
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
    x = x,
    y = y,
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
  
  auc_func <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc_value <- curvaroc$auc
    return(as.numeric(auc_value))
  }
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc_func(obs, Yes))
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}


cruzadaavnnetbin <-  function(data = data,
                              vardep = "vardep",
                              listclass = "listclass",
                              grupos = 4,
                              sinicio = 1234,
                              repe = 5,
                              size = c(5),
                              decay = c(0.01),
                              repeticiones = 5,
                              itera = 100,
                              trace = FALSE) {
  
  databis <- data[, c(vardep, listclass)]
  databis <- dummy.data.frame(databis, listclass, sep = ".")
  
  x <- databis[, names(databis) != vardep]
  y <- as.factor(databis[, vardep])
  
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
    x = x,
    y = y,
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
  
  auc_func <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc_value <- curvaroc$auc
    return(as.numeric(auc_value))
  }
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc_func(obs, Yes))
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}


cruzadarfbin <- function(data = data,
                         vardep = "vardep",
                         listclass = "listclass",
                         grupos = 4,
                         sinicio = 1234,
                         repe = 5,
                         nodesize = 20,
                         mtry = 2,
                         ntree = 50,
                         replace = TRUE,
                         sampsize = 1) {
  
  databis <- data[, c(vardep, listclass)]
  databis <- dummy.data.frame(databis, listclass, sep = ".")
  
  x <- databis[, names(databis) != vardep]
  y <- as.factor(databis[, vardep])
  
  
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
      x = x,
      y = y,
      method = "rf",
      trControl = control,
      tuneGrid = rfgrid,
      nodesize = nodesize,
      replace = replace,
      ntree = ntree
    )
  } else  if (sampsize != 1) {
    rf <- train(
      x = x,
      y = y,
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
  
  auc_func <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc_value <- curvaroc$auc
    return(as.numeric(auc_value))
  }
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc_func(obs, Yes))
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}


cruzadagbmbin <- function(data = data,
                          vardep = "vardep",
                          listclass = "listclass",
                          grupos = 4,
                          sinicio = 1234,
                          repe = 5,
                          n.minobsinnode = 20,
                          shrinkage = 0.1,
                          n.trees = 100,
                          interaction.depth = 2) {
  
  databis <- data[, c(vardep, listclass)]
  databis <- dummy.data.frame(databis, listclass, sep = ".")
  
  x <- databis[, names(databis) != vardep]
  y <- as.factor(databis[, vardep])
  
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
    x = x,
    y = y,
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
  
  auc_func <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc_value <- curvaroc$auc
    return(as.numeric(auc_value))
  }
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc_func(obs, Yes))
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}

cruzadaxgbmbin <- function(data = data,
                           vardep = "vardep",
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
  
  databis <- data[, c(vardep, listclass)]
  databis <- dummy.data.frame(databis, listclass, sep = ".")
  
  x <- databis[, names(databis) != vardep]
  y <- as.factor(databis[, vardep])
  
  
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
    x = x,
    y = y,
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
  
  auc_func <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc_value <- curvaroc$auc
    return(as.numeric(auc_value))
  }
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc_func(obs, Yes))
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}



cruzadaSVMbin <-  function(data = data,
                           vardep = "vardep",
                           listclass = "listclass",
                           grupos = 4,
                           sinicio = 1234,
                           repe = 5,
                           C = 1,
                           replace = TRUE) {
  
  databis <- data[, c(vardep, listclass)]
  databis <- dummy.data.frame(databis, listclass, sep = ".")
  
  x <- databis[, names(databis) != vardep]
  y <- as.factor(databis[, vardep])
  
  
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
    x = x,
    y = y,
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
  
  auc_func <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc_value <- curvaroc$auc
    return(as.numeric(auc_value))
  }
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc_func(obs, Yes))
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}

cruzadaSVMbinPoly <- function(data = data,
                              listclass = "listclass",
                              grupos = 4,
                              sinicio = 1234,
                              repe = 5,
                              C = 1,
                              degree = 2,
                              scale = 1) {
  
  databis <- data[, c(vardep, listclass)]
  databis <- dummy.data.frame(databis, listclass, sep = ".")
  
  x <- databis[, names(databis) != vardep]
  y <- as.factor(databis[, vardep])
  
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
    x = x,
    y = y,
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
  
  auc_func <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc_value <- curvaroc$auc
    return(as.numeric(auc_value))
  }
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc_func(obs, Yes))
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}

cruzadaSVMbinRBF <- function(data = data,
                             vardep = "vardep",
                             listclass = "listclass",
                             grupos = 4,
                             sinicio = 1234,
                             repe = 5,
                             C = 1,
                             sigma = 1) {
  
  databis <- data[, c(vardep, listclass)]
  databis <- dummy.data.frame(databis, listclass, sep = ".")
  
  x <- databis[, names(databis) != vardep]
  y <- as.factor(databis[, vardep])
  
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
    x = x,
    y = y,
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
  
  auc_func <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc_value <- curvaroc$auc
    return(as.numeric(auc_value))
  }
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc_func(obs, Yes))
  
  medias$auc <- mediasbis$auc
  
  return(list(medias, preditest))
  
}
