cruzadagbmbin <-  function(data = data,
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
