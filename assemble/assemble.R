library(sas7bdat)
library(nnet)
library(h2o)
library(dummies)
library(MASS)
library(reshape)
library(caret)
source("cruzadas ensamblado binaria fuente.R")

sel2 <-
  read.sas7bdat("C:\\Users\\secci\\Workspace\\ml\\p2\\lib\\seleccion_metodo_2.sas7bdat")
dput(names(sel2))

sel2 <- na.omit(sel2, (!is.na(sel2)))
sel2$result <-
  factor(sel2$result,
         levels = c(0, 1),
         labels = c("No", "Yes"))

vardep <- c("result")
continuasv2 <- c(
  "assists",
  "deaths",
  "earned_gpm",
  "earnedgold",
  "gspd",
  "inhibitors",
  "monsterkillsenemyjungle",
  "opp_inhibitors",
  "opp_towers",
  "team_kpm",
  "towers"
)
categoricasv2 <- c("firsttothreetowers")

llv2 <- cruzadalogistica(
  data = sel2,
  vardep = vardep,
  listconti = continuasv2,
  listclass = categoricasv2,
  grupos = 4,
  sinicio = 1234,
  repe = 31
)

llv2bis <- as.data.frame(llv2[1])
llv2bis$modelo <- "Logistica"
predillv2 <- as.data.frame(llv2[2])
predillv2$logi <- predillv2$Yes


avnnetv2 <- cruzadaavnnetbin(
  data = sel2,
  vardep = vardep,
  listconti = continuasv2,
  listclass = categoricasv2,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  size = c(5),
  decay = c(0.1),
  repeticiones = 5,
  itera = 200
)

avnnetv2$modelo = "avnnetv2"

avnnetv2bis <- as.data.frame(avnnetv2[1])
avnnetv2bis$modelo <- "avnnetv2"
prediavnnetv2 <- as.data.frame(avnnetv2[2])
prediavnnetv2$avnnet <- prediavnnetv2$Yes

gradientboosting003d4v2 <- cruzadagbmbin(
  data = sel2,
  vardep = vardep,
  listconti = continuasv2,
  listclass = categoricasv2,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  n.minobsinnode = 10,
  shrinkage = 0.03,
  n.trees = 1000,
  interaction.depth = 4
)

gradientboosting003d4v2bis <- as.data.frame(gradientboosting003d4v2[1])
gradientboosting003d4v2bis$modelo <- "gbm"
predigradientboosting003d4v2 <- as.data.frame(gradientboosting003d4v2[2])
predigradientboosting003d4v2$gbm <- predigradientboosting003d4v2$Yes

rfv2 <- cruzadarfbin(
  data = sel2,
  vardep = vardep,
  listconti = continuasv2,
  listclass = categoricasv2,
  grupos = 4,
  sinicio = 1234,
  repe = 31,
  nodesize = 10,
  mtry = 6,
  ntree = 200,
  replace = TRUE
)

rfv2bis <- as.data.frame(rfv2[1])
rfv2bis$modelo <- "rf"
predirfv2 <- as.data.frame(rfv2[2])
predirfv2$rf <- predirfv2$Yes


union <- rbind(llv2bis,
               avnnetv2bis,
               gradientboosting003d4v2bis,
               rfv2bis)

par(cex.axis = 0.8)
boxplot(data = union,
        tasa ~ modelo,
        col = "pink",
        main = 'TASA FALLOS')
boxplot(data = union,
        auc ~ modelo,
        col = "pink",
        main = 'AUC')

unipredi <-
  cbind(predillv2,
        prediavnnetv2,
        predigradientboosting003d4v2,
        predirfv2)

unipredi <- unipredi[, !duplicated(colnames(unipredi))]

unipredi$logiavnnet <- (unipredi$logi + unipredi$avnnet) / 2
unipredi$logigbm <- (unipredi$logi + unipredi$gbm) / 2
unipredi$logirf <- (unipredi$logi + unipredi$rf) / 2

unipredi$avnnetgbm <- (unipredi$avnnet + unipredi$gbm) / 2
unipredi$avnnetrf <- (unipredi$avnnet + unipredi$rf) / 2

unipredi$gbmrf <- (unipredi$gbm + unipredi$rf) / 2

unipredi$logiavnnetgbm <-
  (unipredi$logi + unipredi$avnnet + unipredi$gbm) / 3
unipredi$logiavnnetrf <-
  (unipredi$logi + unipredi$avnnet + unipredi$rf) / 3
unipredi$logigbmrf <-
  (unipredi$logi + unipredi$gbm + unipredi$rf) / 3

unipredi$avnnetgbmrf <-
  (unipredi$avnnet + unipredi$gbm + unipredi$rf) / 3

unipredi$logiavnnetgbmrf <-
  (unipredi$logi + unipredi$avnnet + unipredi$gbm + unipredi$rf) / 4


listado <- c(
  "logi",
  "avnnet",
  "rf",
  "gbm",
  "logiavnnet",
  "logigbm",
  "logirf",
  "avnnetgbm",
  "avnnetrf",
  "gbmrf",
  "logiavnnetgbm",
  "logiavnnetrf",
  "logigbmrf",
  "avnnetgbmrf",
  "logiavnnetgbmrf"
)


tasafallos <- function(x, y) {
  confu <- confusionMatrix(x, y)
  tasa <- confu[[3]][1]
  return(tasa)
}

auc <- function(x, y) {
  curvaroc <- roc(response = x, predictor = y)
  auc <- curvaroc$auc
  return(auc)
}

# Se obtiene el numero de repeticiones CV y se calculan las medias por repe en
# el data frame medias0

repeticiones <- nlevels(factor(unipredi$Rep))
unipredi$Rep <- as.factor(unipredi$Rep)
unipredi$Rep <- as.numeric(unipredi$Rep)


medias0 <- data.frame(c())
for (prediccion in listado)
{
  unipredi$proba <- unipredi[, prediccion]
  unipredi[, prediccion] <-
    ifelse(unipredi[, prediccion] > 0.5, "Yes", "No")
  for (repe in 1:repeticiones)
  {
    paso <- unipredi[(unipredi$Rep == repe),]
    pre <- factor(paso[, prediccion])
    archi <- paso[, c("proba", "obs")]
    archi <- archi[order(archi$proba),]
    obs <- paso[, c("obs")]
    tasa = 1 - tasafallos(pre, obs)
    t <- as.data.frame(tasa)
    t$modelo <- prediccion
    auc <- auc(archi$obs, archi$proba)
    t$auc <- auc
    medias0 <- rbind(medias0, t)
  }
}

# Finalmente boxplot

par(cex.axis = 0.7, las = 2)
boxplot(data = medias0,
        tasa ~ modelo,
        col = "pink",
        main = "TASA FALLOS")

# Para AUC se utiliza la variable auc del archivo medias0

boxplot(data = medias0,
        auc ~ modelo,
        col = "pink",
        main = "AUC")

# PRESENTACION TABLA MEDIAS

tablamedias <- medias0 %>%
  group_by(modelo) %>%
  summarize(tasa = mean(tasa))

tablamedias <- tablamedias[order(tablamedias$tasa),]


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN TASA
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo, tasa, mean))
par(cex.axis = 0.7, las = 2)
boxplot(data = medias0,
        tasa ~ modelo,
        col = "pink",
        main = 'TASA FALLOS')

# ************************************
# PARA AUC
# ************************************

# PRESENTACION TABLA MEDIAS

tablamedias2 <- medias0 %>%
  group_by(modelo) %>%
  summarize(auc = mean(auc))

tablamedias2 <- tablamedias2[order(-tablamedias2$auc),]


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN AUC
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo, auc, mean))
par(cex.axis = 0.7, las = 2)
boxplot(data = medias0,
        auc ~ modelo,
        col = "pink",
        main = 'AUC')


shortlist <- c(
  "logiavnnetgbm",
  "logiavnnetgbmrf",
  "logiavnnetrf",
  "logigbmrf",
  "avnnetrf",
  "avnnet"
)

medias0$modelo <- as.character(medias0$modelo)

mediasver <- medias0[medias0$modelo %in% shortlist, ]


mediasver$modelo <- with(mediasver,
                         reorder(modelo, auc, median))

par(cex.axis = 0.7, las = 2)
boxplot(data = mediasver,
        auc ~ modelo,
        col = "pink",
        main = 'AUC')


mediasver$modelo <- with(mediasver,
                         reorder(modelo, tasa, median))

par(cex.axis = 0.7, las = 2)
boxplot(data = mediasver,
        tasa ~ modelo,
        col = "pink",
        main = 'Tasa de Error Media')

