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