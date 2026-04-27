# Dr. Byron González
# http://byrong.cc

# Dr. Byron González
# http://byrong.cc

if(!require(agricolae)) install.packages("agricolae")
if(!require(desplot)) install.packages("desplot")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lattice)) install.packages("lattice")

crear_croquis_split<- function(factor_principal, factor_secundario, n_bloques, nombre_archivo = "croquis_parcelas_gruesas.png") {
  library(agricolae)
  library(desplot)
  library(ggplot2)
  library(lattice)
  
  # 1. Generar el diseño base
  set.seed(floor(runif(1, 1, 1000)))
  dis_split <- design.split(factor_principal, factor_secundario, r = n_bloques, serie = 2)
  df <- dis_split$book
  
  # 2. Crear columnas de posición si no existen
  # Asumiendo que tenemos columnas: plots, block, factor_principal, factor_secundario
  if (!"col" %in% names(df)) {
    # Crear grid de columnas y filas basado en el número de parcelas
    n_parcelas_por_fila <- length(unique(df[[1]]))
    df$col <- rep(1:n_parcelas_por_fila, length.out = nrow(df))
    df$row <- rep(1:n_bloques, each = nrow(df) / n_bloques)
  }
  
  # 4. Asegurar que col y row sean numéricas
  df$col <- as.numeric(df$col)
  df$row <- as.numeric(df$row)
  
  # 5. Identificar correctamente los nombres de las columnas del factor principal y secundario
  col_names <- names(df)
  factor_cols <- setdiff(col_names, c("plots", "splots", "block", "col", "row"))
  if (length(factor_cols) < 2) {
    stop("No se pudieron identificar las columnas de tratamiento en el diseño.")
  }
  col_factor_principal <- factor_cols[1]
  col_factor_secundario <- factor_cols[2]
  
  # 5.5. Crear etiquetas reducidas pero identificables a partir de los nombres originales
  crear_etiqueta_reducida <- function(nombres) {
    sapply(nombres, function(nombre) {
      if (nchar(nombre) <= 4) {
        nombre
      } else {
        paste0(substr(nombre, 1, 3), ".")
      }
    }, USE.NAMES = FALSE)
  }
  
  # Crear mapeos de etiquetas
  etiq_principal <- crear_etiqueta_reducida(factor_principal)
  etiq_secundario <- crear_etiqueta_reducida(gsub("PP_", "PP", factor_secundario))
  
  # Crear diccionarios para reemplazar valores
  mapeo_principal <- setNames(etiq_principal, factor_principal)
  mapeo_secundario <- setNames(etiq_secundario, factor_secundario)
  
  # Aplicar los mapeos al dataframe
  df[[col_factor_principal]] <- mapeo_principal[as.character(df[[col_factor_principal]])]
  df[[col_factor_secundario]] <- mapeo_secundario[as.character(df[[col_factor_secundario]])]
  
  # Renombrar las columnas a nombres fijos para desplot
  names(df)[names(df) == col_factor_principal] <- "principal"
  names(df)[names(df) == col_factor_secundario] <- "secundario"
  df$principal <- factor(df$principal)
  df$secundario <- factor(df$secundario)
  
  # 6. Crear la visualización con énfasis en bordes
  p <- desplot(df, secundario ~ col + row,
               out1 = block,                         # Borde grueso para cada bloque
               out1.gpar = list(col = "black", lwd = 3),
               out2 = principal,                     # Divisiones internas por factor principal
               out2.gpar = list(col = "gray40", lwd = 2),
               col = principal,
               text = secundario,
               cex = 1.2,
               main = "Croquis de parcelas divididas",
               subtitle = "Bordes NEGROS: Bloques | Bordes GRISES: Parcelas principales",
               show.key = TRUE)
  
  # 7. Guardar el archivo en alta resolución
  png(nombre_archivo, width = 11, height = 8, units = "in", res = 300)
  print(p)
  dev.off()
  
  message(paste("¡Archivo generado! Revisa:", nombre_archivo))
  return(df)
}

# Detalles del experimento
riego_niveles <- c("Goteo", "Aspersión")
fertilizante_niveles <- c("Dosis_0", "Dosis_50", "Dosis_100")

# Llamada a la función
libro_campo <- crear_croquis_split(
  factor_principal = riego_niveles,
  factor_secundario = fertilizante_niveles,
  n_bloques = 3
)

# Ver las primeras filas del libro de diseño
head(libro_campo)

# Exportar a Excel
write.csv(libro_campo,"ParDiv.csv", row.names = FALSE)
