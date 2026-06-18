library(patchwork)
library(GGally)
library(factoextra)
library(ggbiplot)
library(corrplot)
library(geomorph)
source("./utils.R")
source("./db.R")

# Análisis Exploratorio --------------------------------------------------------


dev.new();
wrap_plots(normality.qq(autos_no_loses$numdf, title = "Autos - Cuantiles vs Cuantiles teóricos"),
           normality.hist(autos_no_loses$numdf, title = "Autos - Distancias Mahalanobis"))

autos_no_loses$num_noatip_df
pca <- prcomp(autos_no_loses$num_noatip_df, scale = T, center = T)
dev.new();fviz_pca_biplot(pca, geom = c("point"))
# El precio y el tamaño del motor son casi colineales

pca_colored_by <- function(pca, data, variables) {
  plots <- lapply(variables, function(variable) {
    return(ggbiplot(pca, obs.scale = 1, var.scale = 1,
           groups=data[, c(variable)],
           point.size=1,
           varname.size = 4, 
           varname.color = "black",
           varname.adjust = 1.2,
           ellipse = F, 
           circle = F)+
        scale_color_manual(values=colores))
    }
  )
  return(plots)
}

head(autos_no_loses$num_noatip_df)

plots <- pca_colored_by(pca, autos_no_loses$no_atip_df, factores_nombres)
# 
dev.new();wrap_plots(nrow=1,plots[1:3])
dev.new();wrap_plots(nrow=1,plots[4:6])
dev.new();wrap_plots(nrow=1,plots[7:9])
dev.new();wrap_plots(nrow=1,plots[10:11])
# Todas la variables menos el precio
autos_caracteristicas <- autos_no_loses$numdf[, -14]
autos_economicas <- autos_no_loses$numdf[, "price"]
comparacion <- geomorph::two.b.pls(autos_caracteristicas, autos_economicas)
dev.new();plot(comparacion)
comparacion

dev.new();corrplot(cor(autos_no_loses$numdf,  use = "complete.obs"),method = "color", type = "upper",tl.cex = 0.8, title = "Lineales", mar = c(0,0,1,0))

dev.new()
par(mfrow=c(1,2))
boxplot(autos_economicas)
boxplot(scale(autos_caracteristicas))
