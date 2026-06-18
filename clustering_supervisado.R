library(MASS)
library(caret)
source("./db.R")


lda_por_agrupamiento <- function(data_num_estandar, data, agrupamiento) {
  
  group.lda <- lda(x = data_num_estandar, grouping = data[, agrupamiento], CV = F)
  
  pred_lda <- predict(group.lda, data_num_estandar)
  scores_lda <- as.data.frame(pred_lda$x)
  scores_lda$group <- data[, agrupamiento]
  
  if ("LD2" %in% colnames(scores_lda)) {
    
    x11()
    print(ggplot(scores_lda, aes(x = LD1, y = LD2, color = group)) +
      geom_point(size = 2.5, alpha = 0.8) +
      stat_ellipse(level = 0.95, linewidth = 0.7) +
      theme_minimal() +
      scale_color_manual(values=colores) +
      # scale_color_brewer(palette = colores) +
      labs(title = paste("LDA por", agrupamiento), x = "LD1", y = "LD2"))
  }

  
  clustering.probabilidades.grafico(pred_lda$class, 
                                    pred_lda$posterior, 
                                    data[, agrupamiento],
                                    titulo=paste("Probabilidades por", agrupamiento))
  
  class.lda <- lda(x = data_num_estandar, grouping = data[, agrupamiento], CV = T)
  return(confusionMatrix(class.lda$class, data[, agrupamiento]))
}

for (factor in factores_nombres) {
  print(paste(factor, "======================================================"))
  print(lda_por_agrupamiento(autos_no_loses$num_noatip_df, autos_no_loses$no_atip_df, factor))
}

# EDDA -------------------------------------------------------------------------
dim(autos_no_loses$num_noatip_df)
length(autos_no_loses$no_atip_df$engine.type)


edda <- function(factor, data) {
  class.EDDA <- MclustDA(
    data=data$num_noatip_df, 
    class = data$no_atip_df[,factor], 
    modelType = "EDDA")
  return(summary(class.EDDA))
}
for (factor in factores_nombres) {
  print(paste(factor, "==============================================="))
  print(tryCatch({edda(factor, autos_no_loses)}, error = function(e) {
    message("Ocurrió un error: ", conditionMessage(e))
    return(NA) 
  }))
}

?MclustDA


