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
  
  if (!is.null(listclass)) {
    databis <- data[, c(vardep, listconti, listclass)]
    databis <- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
    databis <- data[, c(vardep, listconti)]
  }
  
  means <- apply(databis[, listconti], 2, mean)
  sds <- sapply(databis[, listconti], sd)
  
  datacon <- scale(databis[, listconti], center = means, scale = sds)
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
  
  auc <- function(x, y) {
    curvaroc <- roc(response = x, predictor = y)
    auc <- curvaroc$auc
    return(auc)
  }
  
  mediasbis <- preditest %>%
    group_by(Rep) %>%
    summarize(auc = auc(obs, Yes))
  
  medias$auc <- mediasbis$auc
  
  return(medias)
  
}
