cruzadarfbin <- function(data = data,
                         vardep = "vardep",
                         listclass = "listclass",
                         grupos = 4,
                         sinicio = 1234,
                         repe = 5,
                         nodesize = 20,
                         mtry = 2,
                         ntree = 50,
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
  
  rfgrid <- expand.grid(mtry = mtry)
  
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
  
  print(rf$results)
  
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
  
  return(medias)
  
}
