library(mvShapiroTest)
library(ggplot2)
library(biotools)
# Funciones comunes ------------------------------------------------------------
distancias.mahalanobis <- function(data_frame) {
  data_frame_num <- data_frame[, sapply(as.data.frame(data_frame), is.numeric)]
  center <- colMeans(data_frame_num)
  cov_mat <- cov(data_frame_num)
  return(mahalanobis(data_frame_num, center, cov_mat))
}
# Normalidad multivariada ------------------------------------------------------
normality.check <- function (data_frame) {
  return(mvShapiro.Test(as.matrix(data_frame)))
}

# Ausencia de valores atípicos multivariados
normality.outliers.check <- function(data_frame, alpha=0.001) {
  data_frame_num <- data_frame[, sapply(as.data.frame(data_frame), is.numeric)]
  distancias <- distancias.mahalanobis(data_frame)
  chi_critico <- qchisq(p = 1-alpha, df = ncol(data_frame_num)) 
  return(distancias > chi_critico)
}

normality.qq <- function(data_frame, title="QQ Distancias Mahalanobis", outliers_alpha=0.001) {
  chi_critico <- qchisq(p = 1-outliers_alpha, df = ncol(data_frame)) 
  dists = distancias.mahalanobis(data_frame)
  colors = ifelse(dists > chi_critico, scales::hue_pal()(2)[1], scales::hue_pal()(2)[2])
  colors <- colors[order(dists)]
  return(ggplot(data.frame(x = dists, color = colors), aes(sample = x)) +
    stat_qq(color=colors, size = 2, alpha = 0.8) +
    stat_qq_line(color = "red", linewidth = 1) +
    labs(title = title,
         x = "Cuantiles teóricos N(0, 1)",
         y = "Cuantiles empíricos") +
    theme_minimal(base_size = 12))
}

normality.hist <- function(data_frame, title="Histograma Distancias Mahalanobis") {
  
  return(ggplot(data.frame(x = distancias.mahalanobis(data_frame)), aes(x = x)) +
           geom_histogram(color="steelblue") +
           labs(title = title,
                x = "Distancia Mahalanobis",
                y = "Frecuencia") +
           theme_minimal(base_size = 12))
} 
# MANOVA Verificación de supuestos----------------------------------------------

# Homogeneidad de las matrices de covarianzas
manova.covhomogeneity.check <- function(allgroups_data_frame, groups_vector) {
  Y <- as.matrix(allgroups_data_frame)
  boxM(Y, groups_vector)
}

colores <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

clustering.probabilidades.grafico <- function(clases, probabilidades, grupos, titulo="") {
  orden <- order(clases, -apply(probabilidades, 1, max))
  probs_ord <- probabilidades[orden, ]
  k_gmm  <- ncol(probabilidades)
  dev.new()
  par(mar = c(4, 4, 3, 1))
  barplot( t(probs_ord), 
           beside  = FALSE, 
           col     = colores, border  = "gray30", space   = 0,
           xlab    = paste0("Individuos (n=", nrow(probabilidades), ", ordenados por grupo)"), 
           ylab    = "Probabilidad de pertenencia", 
           ylim    = c(0, 1), 
           las = 2, 
           names.arg = rep("", nrow(probabilidades)),
           main=titulo)
  cambios <- cumsum(table(clases[orden]))
  abline(v = cambios[-length(cambios)], col = "black", lwd = 2)
  legend(140, 1.12, legend = levels(grupos), fill = colores, border = NA, bty = "n", cex = 0.9, xpd = T)
}

autos.split <- function(autos_data_frame) {
  
  numericas <- autos_data_frame[, sapply(autos_data_frame, is.numeric)]
  outliers <- normality.outliers.check(numericas)
  numericas_sin_outliers <- numericas[!outliers,]
  todas_sin_outliers <- autos_data_frame[!outliers,]
  
  return(list(numdf=numericas, df=autos_data_frame, num_noatip_df=numericas_sin_outliers, no_atip_df=todas_sin_outliers))
}

