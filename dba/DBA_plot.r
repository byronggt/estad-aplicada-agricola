# Dr. Byron González
# http://byrong.cc   

if(!require("agricolae")) install.packages("agricolae")
if(!require("desplot")) install.packages("desplot")
trt <- c("Control", "N50", "N100", "N150")
outdesign <- design.rcbd(trt, r = 4, serie = 2) # 4 repeticiones
field_book <- outdesign$book
field_book

# Asigna columnas y filas: ajusta para que cada bloque (rep) ocupe filas consecutivas
# Ejemplo: rep 1 en filas 1-4, rep 2 en 5-8, etc. (total 16 filas para 4 reps)
field_book$col <- rep(1:4, times = 4)  # Columnas 1-4 repetidas
field_book$row <- rep(1:4, each = 4)  # Filas 1-16, una por parcela (para separar bloques claramente)
field_book

# Genera el plot base con desplot

windows(10,10)
p <- desplot(field_book, trt ~ col * row, 
             text = trt, cex = 1, 
             main = "Croquis de Campo: Ensayo de Fertilización Nitrogenada",
             show.key = TRUE)
print(p)
trellis.focus("panel", 1, 1)
panel.abline(h = seq(1.5, 3.5, by = 1), lwd = 2, col = "black")
trellis.unfocus()


# ==========================================================================
# VERSIÓN 2: croquis DBA con agricolae + funciones gráficas base de R
# (plot, rect, text, legend), sin desplot
# Cada fila del croquis es un bloque completo con todos los tratamientos
# ==========================================================================

crear_croquis_dba_agricolae <- function(tratamientos, repeticiones,
                                        semilla = NULL,
                                        nombre_archivo = "croquis_dba_agricolae.png") {
  
  # 1. Aleatorización con agricolae: los tratamientos se sortean dentro de cada bloque
  if (is.null(semilla)) semilla <- floor(runif(1, 1, 1000))
  dis <- design.rcbd(tratamientos, r = repeticiones, serie = 2, seed = semilla)
  df  <- dis$book
  names(df)[3] <- "tratamiento"
  df$tratamiento <- factor(df$tratamiento, levels = tratamientos)
  df$block <- as.numeric(as.character(df$block))
  
  # 2. Posición: una fila por bloque, una columna por parcela dentro del bloque
  df$row <- df$block
  df$col <- ave(seq_len(nrow(df)), df$block, FUN = seq_along)
  k <- length(tratamientos)
  
  colores <- setNames(hcl.colors(k, "Pastel 1"), tratamientos)
  
  # 3. Dibujar con funciones gráficas base
  dibujar <- function() {
    op <- par(mar = c(5, 5, 4, 1), xpd = TRUE)
    on.exit(par(op))
    plot(NA, xlim = c(0.5, k + 0.5), ylim = c(repeticiones + 0.5, 0.5),
         asp = 1, axes = FALSE, xlab = "", ylab = "",
         main = "Croquis de campo: Diseño en bloques al azar (DBA)",
         sub = paste("Cada fila es un bloque completo - semilla:", semilla))
    rect(df$col - 0.5, df$row - 0.5, df$col + 0.5, df$row + 0.5,
         col = colores[as.character(df$tratamiento)], border = "gray30")
    text(df$col, df$row - 0.12, labels = df$tratamiento, cex = 0.9, font = 2)
    text(df$col, df$row + 0.25, labels = paste("Parcela", df$plots), cex = 0.6)
    # Borde grueso para separar bloques
    rect(0.5, 1:repeticiones - 0.5, k + 0.5, 1:repeticiones + 0.5,
         border = "black", lwd = 3)
    text(0.45, 1:repeticiones, labels = paste("Bloque", 1:repeticiones),
         adj = 1, font = 2, cex = 0.9)
    legend("bottom", inset = c(0, -0.04), legend = tratamientos,
           fill = colores, horiz = TRUE, bty = "n", cex = 0.8)
  }
  
  dibujar()  # vista previa en el panel de gráficos
  
  # 4. Guardar el archivo
  png(nombre_archivo, width = 8, height = 8, units = "in", res = 300)
  dibujar()
  dev.off()
  
  message(paste("¡Listo! El croquis DBA (agricolae) se guardó como:", nombre_archivo))
  return(df)
}

# Misma estructura de tratamientos y bloques que la versión anterior
libro_dba_agricolae <- crear_croquis_dba_agricolae(
  tratamientos = trt,
  repeticiones = 4,
  semilla = 123   # fija la aleatorización para poder reproducirla
)

head(libro_dba_agricolae)
