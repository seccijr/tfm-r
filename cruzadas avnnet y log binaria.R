cruzadalogistica <- function(data = data,
                             vardep = NULL,
                             listclass = NULL,
                             grupos = 4,
                             sinicio = 1234,
                             repe = 5) {
  library(dplyr)
  library(pROC)
  
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
  
  return(medias)
  
}


cruzadaavnnetbin <- function(data = data,
                             vardep = "vardep",
                             listclass = "listclass",
                             grupos = 4,
                             sinicio = 1234,
                             repe = 5,
                             size = c(5),
                             decay = c(0.01),
                             repeticiones = 5,
                             itera = 100) {
  
  databis <- data[, c(vardep, listclass)]
  databis <- dummy.data.frame(databis, listclass, sep = ".")
  
  x <- databis[, names(databis) != vardep]
  y <- as.factor(databis[, vardep])
  
  set.seed(sinicio)
  control <- trainControl(
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
    MaxNWts = 10000000
  )
  
  print(avnnet$results)
  
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
  
  return(medias)
  
}
