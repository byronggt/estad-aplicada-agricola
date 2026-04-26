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