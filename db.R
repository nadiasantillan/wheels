source("./utils.R")

autos_crudo <- read.csv("autos.csv")

# Columnas con ?
indef_x_columna <- colSums(autos_crudo == "?")
indef_x_columna[which(indef_x_columna>0)]
round(indef_x_columna[which(indef_x_columna>0)]/nrow(autos_crudo),2)

# Creación de Factores ---------------------------------------------------------
orden_symbol <- c(-3, -2, -1, 0, 1, 2, 3)
autos_crudo$symbol <- factor(autos_crudo$symbol, levels = orden_symbol, ordered=T)
autos_crudo$make <- factor(autos_crudo$make)
autos_crudo$fuel.type <- factor(autos_crudo$fuel.type)
autos_crudo$aspiration <- factor(autos_crudo$aspiration)
orden_doors <- c("?", "two", "four")
autos_crudo$doors <- factor(autos_crudo$doors, levels = orden_doors, ordered = T)
autos_crudo$body.style <- factor(autos_crudo$body.style)
autos_crudo$drive.wheels <- factor(autos_crudo$drive.wheels)
autos_crudo$engine.location <- factor(autos_crudo$engine.location)
autos_crudo$engine.type <- factor(autos_crudo$engine.type)
orden_num_cylinders <- c("two", "three", "four", "five", "six", "eight", "twelve")
autos_crudo$num.of.cylinders <- factor(autos_crudo$num.of.cylinders, levels = orden_num_cylinders, ordered = T)
autos_crudo$fuel.system <- factor(autos_crudo$fuel.system)

# Conversión a variables numéricas ---------------------------------------------
autos_crudo$normalized.losses <- as.numeric(autos_crudo$normalized.losses)
autos_crudo$bore <- as.numeric(autos_crudo$bore)
autos_crudo$stroke <- as.numeric(autos_crudo$stroke)
autos_crudo$horsepower <- as.numeric(autos_crudo$horsepower)
autos_crudo$peak.rpm <- as.numeric(autos_crudo$peak.rpm)
autos_crudo$price <- as.numeric(autos_crudo$price)

# Tratamiento de datos faltantes -----------------------------------------------
autos_original <- data.frame(autos_crudo)

# Eliminación de filas con indefinidos - se pierde el 22% de los datos
autos_sin_indefinidos <- autos_crudo[complete.cases(autos_crudo), ]

# # 45 de 205 filas tienen valores faltantes
# se reemplazan NAs en normalized.losses por mediana
autos_imputacion <- data.frame(autos_original)
autos_imputacion[is.na(autos_imputacion$normalized.losses), "normalized.losses"] <- median(autos_imputacion$normalized.losses, na.rm=T)
autos_imputacion <- autos_imputacion[complete.cases(autos_imputacion), ]
# # se eliminan el resto de las filas con NA, quedan 195 filas
# autos <- autos[!complete.cases(autos), ]
# # porcentake de filas conservadas
# round(nrow(autos)/filas_original, 2)
# 
# autos_num <- autos[, sapply(autos, is.numeric)]
# autos_num_estandar <- scale(autos_num)

# Eliminación de outliers ------------------------------------------------------
round(nrow(autos_sin_indefinidos)/nrow(autos_original), 2)
outliers <- normality.outliers.check(autos_sin_indefinidos)
sum(outliers)
autos_sin_indefinidos_sin_outliers <- autos_sin_indefinidos[!outliers,]

factores_nombres <- colnames(autos_original)[sapply(autos_original, is.factor)]
caracteristicas_nombres <- colnames(autos_original)[sapply(autos_original, is.numeric)]
economicas_nombres <- c("normalized.losses", "price")
setdiff(caracteristicas_nombres, economicas_nombres)



autos_sin_na <- autos.split(autos_sin_indefinidos)
dim(autos_sin_na$df)
dim(autos_sin_na$df_sin_outliers)

autos_todos <- autos.split(autos_original)

autos_sin_normalized_loses <- subset(autos_original, select = -2)
autos_sin_normalized_loses <- autos_sin_normalized_loses[complete.cases(autos_sin_normalized_loses), ]

autos_no_loses <- autos.split(autos_sin_normalized_loses)
