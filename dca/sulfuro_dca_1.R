# Dr. Byron González 
# http://byrong.cc

# --- 1. Carga e instalación de paquetes ---
# Función para instalar y cargar paquetes
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

packages <- c("readxl", "agricolae", "performance", "AgroR", "FielDHub", "gplots", "gridExtra", "grid")
lapply(packages, install_and_load)

# --- 2. Carga y preparación de datos ---
sulfuros <- read_excel("sulfuros.xlsx")
head(sulfuros)

# Modelo matemático-estadístico:
# sulfuros = media + laboratorio + error experimental 
sulfuros$laboratorio <- factor(sulfuros$laboratorio)
sulfuros$sulfuro <- as.numeric(sulfuros$sulfuro) 
str(sulfuros)

# --- 3. Análisis de Varianza (ANOVA) y Modelo Lineal ---
resultado <- aov(sulfuro ~ laboratorio, data = sulfuros)
mod <- lm(sulfuro ~ laboratorio, data = sulfuros)

# --- 4. Generación de reporte en PDF ---
pdf("resultados_analisis.pdf", width = 8.5, height = 11)

# Página 1: Boxplot
grid.newpage() # Asegura una página limpia
plot(sulfuros$sulfuro ~ sulfuros$laboratorio, 
     col = "skyblue", 
     main = "Distribución de Sulfuro por Laboratorio",
     xlab = "Laboratorio",
     ylab = "Contenido de Sulfuro")

# Página 2: Tabla ANOVA
grid.newpage()
grid.text("Tabla de Análisis de Varianza (ANOVA)", y = 0.9, gp = gpar(fontsize = 14, fontface = "bold"))
# Usar grid.table para un formato de tabla limpio
anova_table <- anova(resultado)
grid.table(round(anova_table, 4), theme = ttheme_default(core = list(gp = gpar(fontsize = 10)),
                                                         colhead = list(gp = gpar(fontsize = 10, fontface = "bold")),
                                                         rowhead = list(gp = gpar(fontsize = 10, fontface = "bold"))))

# Página 3: Gráficos de Diagnóstico del Modelo
grid.newpage()
par(mfrow = c(2,2))
plot(mod)
mtext("Gráficos de Diagnóstico del Modelo Lineal", side = 3, line = -2, outer = TRUE, font = 2)
par(mfrow = c(1,1)) # Resetear layout

# Página 4: Verificación de Supuestos (Texto)
grid.newpage()
grid.text("Prueba de Normalidad de Residuos (Shapiro-Wilk)", y = 0.9, gp = gpar(fontsize = 14, fontface = "bold"))
normality_check <- capture.output(check_normality(mod))
# Usar grid.text para posicionar el texto capturado de forma ordenada
grid.text(paste(normality_check, collapse = "\n"), y = 0.5, just = "left", x = 0.1, 
          gp = gpar(fontfamily = "mono", fontsize = 10))

# Página 5: Gráficos de Diagnóstico (performance)
grid.newpage()
# El paquete performance usa ggplot, por lo que debemos imprimir el resultado explícitamente
plot(check_model(mod))

# Página 6: Prueba de Comparación de Medias de Tukey
grid.newpage()
grid.text("Prueba de Comparación de Medias (Tukey HSD)", y = 0.9, gp = gpar(fontsize = 14, fontface = "bold"))
tukey_test <- HSD.test(resultado, "laboratorio", console = FALSE)
tukey_output <- capture.output(print(tukey_test))
# Usar grid.text nuevamente para el texto
grid.text(paste(tukey_output, collapse = "\n"), y = 0.5, just = "left", x = 0.1,
          gp = gpar(fontfamily = "mono", fontsize = 9))

dev.off() # Cierra el dispositivo PDF y guarda el archivo

cat("El archivo 'resultados_analisis.pdf' ha sido generado con éxito en el directorio de trabajo.\n")

# --- Fin del script principal ---
# El resto del código original se ejecuta interactivamente

# with(sulfuros,DIC(laboratorio,sulfuro,mcomp = "sk"))
# run_app()