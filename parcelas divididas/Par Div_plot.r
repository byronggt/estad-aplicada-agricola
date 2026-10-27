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
  
  # Mantener los nombres completos de los niveles en el libro de campo
  # Renombrar las columnas a nombres fijos para desplot
  names(df)[names(df) == col_factor_principal] <- "principal"
  names(df)[names(df) == col_factor_secundario] <- "secundario"
  df$principal <- factor(df$principal, levels = factor_principal)
  df$secundario <- factor(df$secundario, levels = factor_secundario)
  
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


# ==========================================================================
# VERSIÓN 2: croquis de parcelas divididas con agricolae + funciones
# gráficas base de R (plot, rect, text, legend), sin desplot ni lattice
# Cada fila es un bloque; dentro del bloque, el factor principal se asigna
# a parcelas grandes y el factor secundario a las subparcelas
# ==========================================================================

crear_croquis_split_agricolae <- function(factor_principal, factor_secundario, n_bloques,
                                          semilla = NULL,
                                          nombre_archivo = "croquis_split_agricolae.png") {
  
  # 1. Aleatorización con agricolae (principal en parcelas, secundario en subparcelas)
  if (is.null(semilla)) semilla <- floor(runif(1, 1, 1000))
  dis <- design.split(factor_principal, factor_secundario, r = n_bloques,
                      serie = 2, seed = semilla)
  df  <- dis$book
  names(df)[4] <- "principal"
  names(df)[5] <- "secundario"
  df$principal  <- factor(df$principal,  levels = factor_principal)
  df$secundario <- factor(df$secundario, levels = factor_secundario)
  df$block  <- as.numeric(as.character(df$block))
  df$splots <- as.numeric(as.character(df$splots))
  
  # 2. Posición: fila = bloque; columnas = parcela principal x subparcela
  n_prin <- length(factor_principal)
  n_sec  <- length(factor_secundario)
  pos_parcela <- ave(df$plots, df$block, FUN = function(x) match(x, sort(unique(x))))
  df$row <- df$block
  df$col <- (pos_parcela - 1) * n_sec + df$splots
  n_col  <- n_prin * n_sec
  
  colores <- setNames(hcl.colors(n_sec, "Pastel 1"), factor_secundario)
  
  # 3. Dibujar con funciones gráficas base
  dibujar <- function() {
    op <- par(mar = c(5, 5, 4, 1), xpd = TRUE)
    on.exit(par(op))
    plot(NA, xlim = c(0.5, n_col + 0.5), ylim = c(n_bloques + 0.5, 0.5),
         asp = 1, axes = FALSE, xlab = "", ylab = "",
         main = "Croquis de parcelas divididas",
         sub = paste("Bordes NEGROS: bloques | Bordes GRISES: parcelas principales - semilla:", semilla))
    # Subparcelas
    rect(df$col - 0.5, df$row - 0.5, df$col + 0.5, df$row + 0.5,
         col = colores[as.character(df$secundario)], border = "gray60")
    text(df$col, df$row + 0.05, labels = df$secundario, cex = 0.75, font = 2)
    text(df$col, df$row + 0.32, labels = paste0(df$plots, "-", df$splots), cex = 0.5)
    # Parcelas principales: borde gris grueso y nombre del factor principal
    prin <- df[df$splots == 1, ]
    rect(prin$col - 0.5 + 0.03, prin$row - 0.5 + 0.03,
         prin$col + n_sec - 0.5 - 0.03, prin$row + 0.5 - 0.03,
         border = "gray40", lwd = 2.5)
    text(prin$col + (n_sec - 1) / 2, prin$row - 0.3, labels = prin$principal,
         cex = 0.85, font = 2)
    # Bloques: borde negro grueso
    rect(0.5, 1:n_bloques - 0.5, n_col + 0.5, 1:n_bloques + 0.5,
         border = "black", lwd = 3.5)
    text(0.45, 1:n_bloques, labels = paste("Bloque", 1:n_bloques),
         adj = 1, font = 2, cex = 0.9)
    legend("bottom", inset = c(0, -0.1), legend = factor_secundario,
           fill = colores, horiz = TRUE, bty = "n", cex = 0.8,
           title = "Factor secundario (subparcelas)")
  }
  
  dibujar()  # vista previa en el panel de gráficos
  
  # 4. Guardar el archivo en alta resolución
  png(nombre_archivo, width = 11, height = 8, units = "in", res = 300)
  dibujar()
  dev.off()
  
  message(paste("¡Listo! El croquis de parcelas divididas (agricolae) se guardó como:", nombre_archivo))
  return(df)
}

# Misma estructura de factores y bloques que la versión anterior
libro_campo_agricolae <- crear_croquis_split_agricolae(
  factor_principal = riego_niveles,
  factor_secundario = fertilizante_niveles,
  n_bloques = 3,
  semilla = 123   # fija la aleatorización para poder reproducirla
)

head(libro_campo_agricolae)
