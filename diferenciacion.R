library(RRPP)
library(ape)

source("./db.R")
# Funciones --------------------------------------------------------------------
grafico.comparacion.pares <- function(tabla_dF, clases, titulo) {
  pcoa.Z <- pcoa(as.dist(tabla_dF$pairwise.tables$Z), correction = "lingoes")
  percentages <- round(pcoa.Z$values$Rel_corr_eig*100, 2) 
  coordenadas <- pcoa.Z$vectors.cor
  diferencias.totales <- rowSums(tabla_dF$pairwise.tables$Z*(tabla_dF$pairwise.tables$Z>0)) # esto es para graficar: diferencias totales que suma cada especie. si hay algun valor negativo, lo quito.
  tabla.clas <- table(clases)
  
  datos_plot <- data.frame(
    especie =  rownames(coordenadas),
    dim1 = coordenadas[,1],
    dim2 = coordenadas[,2],
    diferencias.totales = diferencias.totales[rownames(coordenadas)],  # alinear nombres
    n.total = as.vector(tabla.clas[rownames(coordenadas)])
  )
  
  comparaciones.plot <- ggplot(datos_plot, aes(x = dim1, y = dim2, size = n.total, color = diferencias.totales)) +
    geom_point(alpha = .8) +
    scale_size_continuous(name = "sample size", range = c(2, 15)) +
    scale_color_gradient(low = "blue", high = "red", name = "Z score") +
    labs(title = titulo, x = paste0("PCoA 1 - ",percentages[1],"%"), y = paste0("PCoA 2 ",percentages[2],"%")) +
    geom_text_repel(aes(label = especie),
                    colour = "black",
                    force = 60,                # Fuerza de repulsión entre etiquetas
                    force_pull = 0.5,          # Fuerza de atracción hacia su punto
                    point.padding = 0.05,      # Reduce el padding alrededor del punto
                    box.padding = 0.5,         # Reduce el padding alrededor del texto
                    max.overlaps = Inf,        # Fuerza a mostrar todas las etiquetas
                    segment.color = "gray35",
                    segment.size = 0.1,
                    parse = TRUE,
                    size = 3)+
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),   # quitar cuadrícula
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),  # ejes
      axis.ticks = element_line(color = "black"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5))

  return(comparaciones.plot)  
}


autos_caracteristicas <- autos_no_loses$num_noatip_df[, -14]
autos_economicas <- autos_no_loses$num_noatip_df[, "price"]

autos_caracteristicas_dist_mat <- as.matrix(dist(scale(autos_caracteristicas)))
autos_economicas_dist_mat <- as.matrix(dist(scale(autos_economicas)))

autos_caracteristicas_rrpp <- lm.rrpp(
  autos_caracteristicas_dist_mat ~ symbol+make+fuel.type+aspiration+doors+body.style+drive.wheels+engine.location+engine.type+num.of.cylinders+fuel.system, 
  data=autos_no_loses$no_atip_df,
  SS.type = "III")
anova(autos_caracteristicas_rrpp)
#symbol
#make
#aspiration
#body.style
#drive.wheels
#engine.type
#num.of.cylinder


autos_economicas_rrpp <- lm.rrpp(
  autos_economicas_dist_mat ~ symbol+make+fuel.type+aspiration+doors+body.style+drive.wheels+engine.location+engine.type+num.of.cylinders+fuel.system, 
  data=autos_no_loses$no_atip_df,
  SS.type = "III")
anova(autos_caracteristicas_rrpp)

# El sistema de inyección, la marca y la transmisión definen las variables ecónomicas
autos_economicas_rrpp_marca <- lm.rrpp(
  autos_economicas_dist_mat ~ make, 
  data=autos_no_loses$no_atip_df,
  SS.type = "III")
anova(autos_economicas_rrpp_marca)

autos_economicas_rrpp_symbol <- lm.rrpp(
  autos_economicas_dist_mat ~ symbol, 
  data=autos_no_loses$no_atip_df,
  SS.type = "III")
anova(autos_economicas_rrpp_symbol)


autos_economicas_rrpp_cat_marca <- lm.rrpp(
  autos_economicas_dist_mat ~ symbol*make, 
  data=autos_no_loses$no_atip_df,
  SS.type = "III")
anova(autos_economicas_rrpp_cat_marca)

autos_caracteristicas_symbol_rrpp <- lm.rrpp(
  autos_caracteristicas_dist_mat ~ symbol+price, 
  data=autos_no_loses$no_atip_df,
  SS.type = "III")
anova(autos_caracteristicas_symbol_rrpp)


symbol_pairs <- pairwise(autos_economicas_rrpp_cat_marca, fit.null = autos_economicas_rrpp_marca, groups=autos_no_loses$no_atip_df$symbol)
summary(symbol_pairs)
summary(symbol_pairs, type="var")
  
make_pairs <- pairwise(autos_economicas_rrpp_cat_marca, fit.null = autos_economicas_rrpp_symbol, groups=autos_no_loses$no_atip_df$make)
summary(make_pairs)
summary(make_pairs, type="var")

dev.new();grafico.comparacion.pares(summary(make_pairs, stat.table = F), autos_no_loses$no_atip_df$make, "Comparación Marcas")
dev.new();grafico.comparacion.pares(summary(symbol_pairs, stat.table = F), autos_no_loses$no_atip_df$symbol, "Comparación Symbol")

dev.new();grafico.comparacion.pares(summary(make_pairs, test.type = "var", stat.table = F), autos_no_loses$no_atip_df$make, "Comparación Marcas - Varianzas")
dev.new();grafico.comparacion.pares(summary(symbol_pairs, test.type = "var", stat.table = F), autos_no_loses$no_atip_df$symbol, "Comparación Symbol - Varianzas")

