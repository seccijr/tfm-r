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
  
  print(xgbm$results)
  
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
  
  return(medias)
  
}
