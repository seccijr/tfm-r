library(plyr)
detach(package:plyr)
library(dummies)
library(MASS)
library(reshape)
library(caret)
library(dplyr)
library(pROC)


cruzadaSVMbinRBF <- function(data = data,
                             vardep = "vardep",
                             listconti = "listconti",
                             listclass = "listclass",
                             grupos = 4,
                             sinicio = 1234,
                             repe = 5,
                             C = 1,
                             sigma = 1) {
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
  
  
  SVMgrid <- expand.grid(C = C, sigma = sigma)
  
  SVM <- train(
    formu,
    data = databis,
    method = "svmRadial",
    trControl = control,
    tuneGrid = SVMgrid,
    replace = replace
  )
  
  print(SVM$results)
  
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
  
  return(medias)
  
}
