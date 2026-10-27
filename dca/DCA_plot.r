# Dr. Byron González
# http://byrong.cc 

 if(!require(desplot)) install.packages("desplot", dependencies = TRUE)
 if(!require(agricolae)) install.packages("agricolae", dependencies = TRUE)
 if(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)

crear_croquis_dca <- function(tratamientos, repeticiones, nombre_archivo = "croquis_dca.png") {
  
  # 1. Generar el diseño DCA
  set.seed(floor(runif(1, 1, 1000)))
  dis <- design.crd(tratamientos, r = repeticiones, serie = 2)
  df <- dis$book
  
  # Creamos coordenadas ficticias para que desplot dibuje una cuadrícula
  n_total <- nrow(df)
  columnas <- ceiling(sqrt(n_total))
  df$col <- rep(1:columnas, length.out = n_total)
  df$row <- rep(1:ceiling(n_total/columnas), each = columnas, length.out = n_total)
  
  # 2. Crear el gráfico
  p <- desplot(df, tratamientos ~ col * row,
               text = tratamientos, 
               cex = 1.5,
               fill = tratamientos,
               palette = "Pastel1",
               main = "Croquis: Diseño completamente al azar (DCA)",
               subtitle = "Aleatorización total sin restricciones de bloques",
               show.key = TRUE)
  
  # 3. Guardar el archivo
  png(nombre_archivo, width = 8, height = 6, units = "in", res = 300)
  print(p)
  dev.off()
  
  message(paste("¡Listo! El croquis DCA se guardó como:", nombre_archivo))
  return(df)
}

# Definir tratamientos
mis_sustratos <- c("Turba", "Compost", "Fibra_Coco", "Tierra_simple")

# Definir número de repeticiones por tratamiento
n_reps <- 5

# Ejecutar la función para crear el croquis DCA
crear_croquis_dca(tratamientos = mis_sustratos, repeticiones = n_reps)

# Ejecutar y guardar el libro de campo en una variable
mi_ensayo <- crear_croquis_dca(
  tratamientos = mis_sustratos, 
  repeticiones = n_reps, 
  nombre_archivo = "plan_invernadero_DCA.png"
)

# Ver las primeras filas del diseño
head(mi_ensayo)

# Exportar a Excel/CSV para llevarlo al invernadero
write.csv(mi_ensayo, "etiquetas_macetas.csv", row.names = FALSE)


# ==========================================================================
# VERSIÓN 2: croquis DCA con agricolae + funciones gráficas base de R
# (plot, rect, text, legend), sin desplot ni ggplot2
# ==========================================================================

crear_croquis_dca_agricolae <- function(tratamientos, repeticiones,
                                        semilla = NULL,
                                        nombre_archivo = "croquis_dca_agricolae.png") {
  
  # 1. Aleatorización con agricolae (misma estructura que la versión anterior)
  if (is.null(semilla)) semilla <- floor(runif(1, 1, 1000))
  dis <- design.crd(tratamientos, r = repeticiones, serie = 2, seed = semilla)
  df  <- dis$book
  names(df)[3] <- "tratamiento"
  df$tratamiento <- factor(df$tratamiento, levels = tratamientos)
  
  # 2. Cuadrícula de unidades experimentales (se rellena por filas)
  n_total  <- nrow(df)
  columnas <- ceiling(sqrt(n_total))
  filas    <- ceiling(n_total / columnas)
  df$col <- rep(1:columnas, length.out = n_total)
  df$row <- rep(1:filas, each = columnas, length.out = n_total)
  
  colores <- setNames(hcl.colors(length(tratamientos), "Pastel 1"), tratamientos)
  
  # 3. Dibujar con funciones gráficas base
  dibujar <- function() {
    op <- par(mar = c(5, 1, 4, 1), xpd = TRUE)
    on.exit(par(op))
    plot(NA, xlim = c(0.5, columnas + 0.5), ylim = c(filas + 0.5, 0.5),
         asp = 1, axes = FALSE, xlab = "", ylab = "",
         main = "Croquis: Diseño completamente al azar (DCA)",
         sub = paste("Aleatorización total sin restricciones de bloques - semilla:", semilla))
    rect(df$col - 0.5, df$row - 0.5, df$col + 0.5, df$row + 0.5,
         col = colores[as.character(df$tratamiento)], border = "gray30")
    text(df$col, df$row - 0.15, labels = df$tratamiento, cex = 0.75, font = 2)
    text(df$col, df$row + 0.25, labels = paste0("Parcela ", df$plots, " (r", df$r, ")"),
         cex = 0.6)
    legend("bottom", inset = c(0, -0.04), legend = tratamientos,
           fill = colores, horiz = TRUE, bty = "n", cex = 0.8)
  }
  
  dibujar()  # vista previa en el panel de gráficos
  
  # 4. Guardar el archivo
  png(nombre_archivo, width = 8, height = 6, units = "in", res = 300)
  dibujar()
  dev.off()
  
  message(paste("¡Listo! El croquis DCA (agricolae) se guardó como:", nombre_archivo))
  return(df)
}

# Misma estructura de tratamientos y repeticiones (mis_sustratos, n_reps)
mi_ensayo_agricolae <- crear_croquis_dca_agricolae(
  tratamientos = mis_sustratos,
  repeticiones = n_reps,
  semilla = 123   # fija la aleatorización para poder reproducirla
)

head(mi_ensayo_agricolae)
