cruzadagbmbin <-  function(data = data,
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
  
  if (!is.null(listclass)) {
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
  databis <- cbind(datacon, databis[,-numerocont, drop = FALSE])
  
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
  
  print(gbm$results)
  
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
  
  return(medias)
  
}
