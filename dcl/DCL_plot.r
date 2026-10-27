# Dr. Byron González
# http://byrong.cc

if(!require(agricolae)) install.packages("agricolae")
if(!require(desplot)) install.packages("desplot")
if(!require(ggplot2)) install.packages("ggplot2")

# Colocar la función en memoria para crear el croquis de Cuadrado Latino

crear_croquis_latino <- function(tratamientos, nombre_archivo = "croquis_latino.png") {
  
  # 1. Generar el diseño de Cuadrado Latino
  set.seed(floor(runif(1, 1, 1000)))
  dis <- design.lsd(tratamientos, serie = 2)
  df <- dis$book
  
  # Asegurar que col y row sean numéricos
  df$col <- as.numeric(df$col)
  df$row <- as.numeric(df$row)
  
  # Convertir tratamientos a ASCII para evitar warnings
  ascii_trat = iconv(tratamientos, "UTF-8", "ASCII//TRANSLIT")
  df$tratamientos = iconv(df$tratamientos, "UTF-8", "ASCII//TRANSLIT")
  
  # 2. Crear el gráfico
  # Usamos 'col' y 'row' para que R dibuje la cuadrícula exacta
  p <- desplot(df, tratamientos ~ col * row,
               text = tratamientos, 
               cex = 1.5,
               fill = tratamientos,      # Cada tratamiento un color diferente
               palette = "Set3",         # Paleta de colores suave y profesional
               main = "Croquis de campo: Cuadrado Latino",
               subtitle = paste("Tratamientos:", paste(ascii_trat, collapse=", ")),
               show.key = TRUE,
               abbreviate = FALSE)
  
  # 3. Guardar el archivo
  png(nombre_archivo, width = 8, height = 8, units = "in", res = 300)
  print(p)
  dev.off()
  
  message(paste("¡Listo! El croquis de Cuadrado Latino se guardó como:", nombre_archivo))
  return(df)
}

# --- Aplicación ---
mis_tratamientos <- c("Testigo", "NPK", "Orgánico", "Bioestimulante")

libro_latino <- crear_croquis_latino(
  tratamientos = mis_tratamientos,
  nombre_archivo = "Cuadrado_Latino.png"
)

# Ver las primeras filas del libro de diseño
head(libro_latino)

# Exportar a Excel
write.csv(libro_latino, "Cuadrado_Latino.csv", row.names = FALSE)


# ==========================================================================
# VERSIÓN 2: croquis de Cuadrado Latino con agricolae + funciones gráficas
# base de R (plot, rect, text, axis, legend), sin desplot ni ggplot2
# Cada tratamiento aparece una sola vez por fila y por columna
# ==========================================================================

crear_croquis_latino_agricolae <- function(tratamientos, semilla = NULL,
                                           nombre_archivo = "croquis_latino_agricolae.png") {
  
  # 1. Aleatorización con agricolae
  if (is.null(semilla)) semilla <- floor(runif(1, 1, 1000))
  dis <- design.lsd(tratamientos, serie = 2, seed = semilla)
  df  <- dis$book
  names(df)[4] <- "tratamiento"
  df$tratamiento <- factor(df$tratamiento, levels = tratamientos)
  df$row <- as.numeric(as.character(df$row))
  df$col <- as.numeric(as.character(df$col))
  k <- length(tratamientos)
  
  colores <- setNames(hcl.colors(k, "Pastel 1"), tratamientos)
  
  # 2. Dibujar con funciones gráficas base
  dibujar <- function() {
    op <- par(mar = c(5, 5, 5, 1), xpd = TRUE)
    on.exit(par(op))
    plot(NA, xlim = c(0.5, k + 0.5), ylim = c(k + 0.5, 0.5),
         asp = 1, axes = FALSE, xlab = "", ylab = "",
         main = "Croquis de campo: Cuadrado Latino",
         sub = paste("Un tratamiento por fila y por columna - semilla:", semilla))
    rect(df$col - 0.5, df$row - 0.5, df$col + 0.5, df$row + 0.5,
         col = colores[as.character(df$tratamiento)], border = "gray30", lwd = 1.5)
    text(df$col, df$row - 0.1, labels = df$tratamiento, cex = 0.9, font = 2)
    text(df$col, df$row + 0.25, labels = paste("Parcela", df$plots), cex = 0.6)
    axis(3, at = 1:k, labels = paste("Columna", 1:k), tick = FALSE, line = -1)
    axis(2, at = 1:k, labels = paste("Fila", 1:k), tick = FALSE, las = 1, line = -1)
    legend("bottom", inset = c(0, -0.07), legend = tratamientos,
           fill = colores, horiz = TRUE, bty = "n", cex = 0.8)
  }
  
  dibujar()  # vista previa en el panel de gráficos
  
  # 3. Guardar el archivo
  png(nombre_archivo, width = 8, height = 8, units = "in", res = 300)
  dibujar()
  dev.off()
  
  message(paste("¡Listo! El croquis de Cuadrado Latino (agricolae) se guardó como:", nombre_archivo))
  return(df)
}

# Misma estructura de tratamientos que la versión anterior
libro_latino_agricolae <- crear_croquis_latino_agricolae(
  tratamientos = mis_tratamientos,
  semilla = 123   # fija la aleatorización para poder reproducirla
)

head(libro_latino_agricolae)