library(vegan)
library(mclust)
library(RColorBrewer)
library(factoextra)
library(dbscan)

# Ward D2 Funciones ------------------------------------------------------------
clustering.wardd2 <- function(num_estandar_data) {
  data_dist <- dist(num_estandar_data, method="euclidean")
  return(hclust(data_dist, method = "ward.D2"))
}

clustering.wardd2.dendograma <- function(hc, k=4) {
  dev.new();
  fviz_dend(hc, k = k, #numero de grupos
      cex = 0.6, lwd=.3, color_labels_by_k = TRUE, 
      rect = TRUE, 
      main = paste0("Ward D2 - k =", k))
}

clustering.wardd2.scree <- function(hc, title = "Scree plot") {
  heights <- sort(hc$height, decreasing = TRUE) 
  n_max <- 15
  dev.new()
  plot(1:n_max, heights[1:n_max], type = "b", pch = 19, 
       xlab = "Número de fusión (de la más tardía a la más temprana)", 
       ylab = "Altura de fusión (disimilitud o heights)", main = title)
  
}
# K-Means funciones ------------------------------------------------------------
clustering.kmeans <- function(data_num_estandar, k) {
  return(kmeans(data_num_estandar, centers = k, nstart = 100, iter.max = 1000))
  
}

clustering.kmeans.brute <- function(data_num_estandar, data_complete, factores) {
  for (factor in factores) {
    k <- length(levels(data_complete[, factor]))
    km <- clustering.kmeans(data_num_estandar, k)
    print(paste(factor, "-----------------------"))
    print(adjustedRandIndex(data_complete[, factor], km$cluster))
  }
}


# GMM Funciones ----------------------------------------------------------------
clustering.gmm <- function(data_num_estandar) {
  gmm <- Mclust(data_num_estandar)
  gmm_icl <- mclustICL(data_num_estandar)
  return(list(gmm=gmm, gmm_icl=gmm_icl))
}

clustering.gmm.bic_vs_icl_entropia <- function(gmm, gmm_icl, title="") {
  entropia <- -rowSums(gmm$z * log(gmm$z + 1e-10))

  dev.new();
  par(mfrow=c(1,3))
  plot(gmm, what = "BIC", main=paste0(title, " - GMM - BIC"))
  plot(gmm_icl, main = paste0(title, " - GMM - ICL"))
  hist(entropia, main = paste0(title, " - GMM - Entropía"))
  abline(v=mean(entropia), col = "red") #Entropía promedio
  
}

# dev.new()
# # a) Clasificación con elipses de densidad:
# dev.new();plot(autos_gmm, what = "classification")
# # b) Densidad estimada:
# dev.new();plot(autos_gmm, what = "density")
# # c) Incertidumbre por observación:
# dev.new();plot(autos_gmm, what = "uncertainty")


clustering.gmm.brute <- function(gmm, data_complete, factores) {
  for (factor in factores) {
    print(paste(factor, "-----------------------"))
    print(adjustedRandIndex(data_complete[, factor], gmm$classification))
  }
}

# OPTICS Funciones -------------------------------------------------------------
clustering.optics.brute <- function(data_num_estandar, minPts=1:9) {
  for (i in minPts) {
    optics_results <- optics(data_num_estandar, eps = Inf, minPts = i)
    dev.new();plot(optics_results, main = paste("minPts = ", i))
  }
}

clustering.optics.corte.brute <- function(
    data_num_estandar, 
    data_complete, 
    minPts, 
    factores, 
    corte_seq) {
  optics_results <- optics(data_num_estandar, eps = Inf, minPts = minPts)
  min_noise <- nrow(data_num_estandar)
  min_corte <- 0
  min_grupos <- NULL
  for (corte in corte_seq) {
    grupos <- extractDBSCAN(optics_results, eps_cl = corte)
    print(grupos)
    if (table(grupos$cluster)["0"] < min_noise) {
      min_corte <- corte
      min_noise <- table(grupos$cluster)["0"]
      min_grupos <- grupos
    }
  }
  
  dev.new()
  plot(optics_results, main = paste0("Corte eps=", min_corte))
  abline(h = min_corte, col = "red", lty = 2)

  for (factor in factores) {
    print(paste(factor, "-----------------------"))
    print(adjustedRandIndex(data_complete[, factor], min_grupos$cluster))
  }
  
  return(list(noise=min_noise, corte=min_corte, grupos=min_grupos))
  
}
# Agrupamientos ----------------------------------------------------------------
source("./db.R")

# Ward D2
hc <- clustering.wardd2(scale(autos_no_loses$num_noatip_df))
clustering.wardd2.dendograma(hc, k = 7)
clustering.wardd2.scree(hc, title = "Autos - Ward D2 - Scree Plot")

autos_wardd2 <- cutree(hc, k = 7)

# K-means
autos_kmeans <- clustering.kmeans(scale(autos_no_loses$num_noatip_df), k = 7)
clustering.kmeans.brute(scale(autos_no_loses$num_noatip_df), autos_no_loses$no_atip_df, factores_nombres)

# GMM
#autos_gmm <- clustering.gmm(autos_todos$numdf) #falla por NAs
autos_gmm_sin_na <- clustering.gmm(autos_no_loses$num_noatip_df) 
summary(autos_gmm_sin_na$gmm)
summary(autos_gmm_sin_na$gmm_icl)

clustering.gmm.bic_vs_icl_entropia(autos_gmm_sin_na$gmm, autos_gmm_sin_na$gmm_icl, title="Autos Sin Indefinidos")
clustering.probabilidades.grafico(autos_gmm_sin_na$gmm$classification, autos_gmm_sin_na$gmm$z, "Clasificación GMM")
clustering.gmm.brute(autos_gmm_sin_na$gmm, autos_no_loses$no_atip_df, factores_nombres)

# OPTICS
clustering.optics.brute(scale(autos_no_loses$num_noatip_df), minPts = 1:22)
# Se ven algunos valles en 5, elección por fuerza bruta del corte con menos puntos ruido
optics_5 <- clustering.optics.corte.brute(
  scale(autos_no_loses$num_noatip_df), 
  autos_no_loses$no_atip_df, 
  minPts = 5, 
  factores_nombres, corte_seq = seq(1, 2.5, 0.1))
# Mejor Rand Index
# [1] "num.of.cylinders -----------------------"
# [1] 0.2704228
print(optics_5$grupos)

