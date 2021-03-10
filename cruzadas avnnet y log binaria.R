cruzadalogistica <- function(data = data,
                             vardep = NULL,
                             listconti = NULL,
                             listclass = NULL,
                             grupos = 4,
                             sinicio = 1234,
                             repe = 5) {
  library(dummies)
  library(MASS)
  library(reshape)
  library(caret)
  library(dplyr)
  library(pROC)
  
  if (!is.null(listclass)) {
    for (i in 1:dim(array(listclass))) {
      numindi <- which(names(data) == listclass[[i]])
      data[, numindi] <- as.character(data[, numindi])
      data[, numindi] <- as.factor(data[, numindi])
    }
  }
  
  data[, vardep] <- as.factor(data[, vardep])
  
  
  if (!is.null(listclass)) {
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
  
  return(medias)
  
}


cruzadaavnnetbin <- function(data = data,
                             vardep = "vardep",
                             listconti = "listconti",
                             listclass = "listclass",
                             grupos = 4,
                             sinicio = 1234,
                             repe = 5,
                             size = c(5),
                             decay = c(0.01),
                             repeticiones = 5,
                             itera = 100) {
  
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
  databis <- cbind(datacon, databis[, -numerocont, drop = FALSE])
  
  databis[, vardep] <- as.factor(databis[, vardep])
  
  formu <- formula(paste(vardep, "~.", sep = ""))
  
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
    formu,
    data = databis,
    method = "avNNet",
    linout = FALSE,
    maxit = itera,
    repeats = repeticiones,
    trControl = control,
    tuneGrid = avnnetgrid
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
