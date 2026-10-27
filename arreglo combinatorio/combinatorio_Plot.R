# Dr. Byron González
# http://byrong.cc

# Croquis aleatorizado para un arreglo combinatorio (factorial) de dos factores
# en bloques completos al azar (DBCA), con agricolae + funciones gráficas base de R.
#
# Modelo estadístico del anova (brocoli_combinatorio.R):
#   Rendimiento ~ Bloque + Híbrido * Distanciamiento
#   y_ijk = mu + bloque_k + hibrido_i + distanciamiento_j + (hibrido x distanc.)_ij + e_ijk
# Cada bloque contiene TODOS los tratamientos (combinaciones híbrido x distanciamiento),
# aleatorizados de forma independiente dentro de cada bloque.

if(!require(agricolae)) install.packages("agricolae", dependencies = TRUE)

crear_croquis_combinatorio <- function(niveles_A, niveles_B, bloques,
                                       nombre_A = "Hibrido", nombre_B = "Dist",
                                       semilla = NULL,
                                       nombre_archivo = "croquis_combinatorio_agricolae.png") {
  
  # 1. Aleatorización con agricolae: tratamientos = combinaciones A x B, r = bloques
  if (is.null(semilla)) semilla <- floor(runif(1, 1, 1000))
  dis <- design.ab(trt = c(length(niveles_A), length(niveles_B)), r = bloques,
                   design = "rcbd", serie = 2, seed = semilla)
  df <- dis$book
  names(df)[3:4] <- c("A", "B")
  
  # Etiquetas con los niveles reales de cada factor
  df$A <- factor(niveles_A[as.integer(as.character(df$A))], levels = niveles_A)
  df$B <- factor(niveles_B[as.integer(as.character(df$B))], levels = niveles_B)
  df$tratamiento <- paste(df$A, df$B, sep = "-")
  names(df)[names(df) == "A"] <- nombre_A
  names(df)[names(df) == "B"] <- nombre_B
  
  # 2. Cuadrícula: cada bloque es una fila; las columnas son las parcelas del bloque
  n_trat   <- length(niveles_A) * length(niveles_B)
  df$block <- as.integer(as.character(df$block))
  df$col   <- ave(df$plots, df$block, FUN = seq_along)
  df$row   <- df$block
  
  # Color según el nivel del factor A
  colores <- setNames(hcl.colors(length(niveles_A), "Pastel 1"), niveles_A)
  col_par <- colores[as.character(df[[nombre_A]])]
  
  # 3. Dibujar con funciones gráficas base
  dibujar <- function() {
    op <- par(mar = c(5, 4, 4, 1), xpd = TRUE)
    on.exit(par(op))
    plot(NA, xlim = c(0.5, n_trat + 0.5), ylim = c(bloques + 0.5, 0.5),
         asp = 1, axes = FALSE, xlab = "", ylab = "",
         main = "Croquis: Arreglo combinatorio en bloques completos al azar",
         sub = paste("Tratamientos =", nombre_A, "x", nombre_B, "- semilla:", semilla))
    rect(df$col - 0.5, df$row - 0.5, df$col + 0.5, df$row + 0.5,
         col = col_par, border = "gray30")
    text(df$col, df$row - 0.12, labels = df$tratamiento, cex = 0.8, font = 2)
    text(df$col, df$row + 0.25, labels = paste0("P", df$plots), cex = 0.55)
    text(0.3, 1:bloques, labels = paste("Bloque", 1:bloques), adj = 1, font = 2, cex = 0.8)
    legend("bottom", inset = c(0, -0.12), legend = paste(nombre_A, niveles_A),
           fill = colores, horiz = TRUE, bty = "n", cex = 0.8)
  }
  
  dibujar()  # vista previa en el panel de gráficos
  
  # 4. Guardar el archivo
  png(nombre_archivo, width = 12, height = 5, units = "in", res = 300)
  dibujar()
  dev.off()
  
  message(paste("¡Listo! El croquis se guardó como:", nombre_archivo))
  return(df)
}

# Factores del experimento de brócoli: 5 híbridos x 3 distanciamientos, 3 bloques
hibridos         <- c("H1", "H2", "H3", "H4", "H5")
distanciamientos <- c("D1", "D2", "D3")
n_bloques        <- 3

mi_ensayo <- crear_croquis_combinatorio(
  niveles_A = hibridos,
  niveles_B = distanciamientos,
  bloques   = n_bloques,
  nombre_A  = "Hibrido",
  nombre_B  = "Distanciamiento",
  semilla   = 123   # fija la aleatorización para poder reproducirla
)

# Ver las primeras filas del libro de campo
head(mi_ensayo)

# Cada tratamiento debe aparecer una vez por bloque
table(mi_ensayo$tratamiento, mi_ensayo$block)

# Exportar el libro de campo
write.csv(mi_ensayo, "libro_campo_combinatorio.csv", row.names = FALSE)
