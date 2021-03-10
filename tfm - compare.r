gradientboosting003d4v0 <- gradientboosting003d4
gradientboosting01d4v0 <- gradientboosting01d4
logisticv0 <- logistic

avnnetv1 <- avvnet
gradientboosting003d4v1 <- gradientboosting003d4
logisticv1 <- logistic

avnnetv2 <- avvnet
gradientboosting003d4v2 <- gradientboosting003d4
logisticv2 <- logistic

avnnetn2v3 <- avvnetn2
gradientboostingv3 <- gradientboosting
logisticv3 <- logistic

avnnetv4 <- avvnet
gradientboosting003d4v4 <- gradientboosting003d4
logisticv4 <- logistic

avnnetv5 <- avvnet
logisticv5 <- logistic

gradientboosting003d4v0$modelo <- "gradientboosting003d4v0"
gradientboosting01d4v0$modelo <- "gradientboosting01d4v0"
logisticv0$modelo <- "logisticv0"
avnnetv1$modelo <- "avnnetv1"
gradientboosting003d4v1$modelo <- "gradientboosting003d4v1"
logisticv1$modelo <- "logisticv1"
avnnetv2$modelo <- "avnnetv2"
gradientboosting003d4v2$modelo <- "gradientboosting003d4v2"
logisticv2$modelo <- "logisticv2"
avnnetn2v3$modelo <- "avnnetn2v3"
gradientboostingv3$modelo <- "gradientboostingv3"
logisticv3$modelo <- "logisticv3"
avnnetv4$modelo <- "avnnetv4"
gradientboosting003d4v4$modelo <- "gradientboosting003d4v4"
logisticv4$modelo <- "logisticv4"
avnnetv5$modelo <- "avnnetv5"
logisticv5$modelo <- "logisticv5"


union <-
  rbind(gradientboosting003d4v0,
        gradientboosting01d4v0,
        logisticv0,
        avnnetv1,
        gradientboosting003d4v1,
        logisticv1,
        avnnetv2,
        gradientboosting003d4v2,
        logisticv2,
        avnnetn2v3,
        gradientboostingv3,
        logisticv3,
        avnnetv4,
        gradientboosting003d4v4,
        logisticv4,
        avnnetv5,
        logisticv5)


par(cex.axis = 0.6)
boxplot(data = union, tasa ~ modelo, main = "TASA FALLOS")
