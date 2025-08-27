# Dr. Byron González
# http://byrong.cc

if(!require(readxl)){install.packages("readxl")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(car)){install.packages("car")}
if(!require(multcomp)){install.packages("multcomp")}

# Importar archivo de Excel 
pino <- read_excel("pinus_ponderosa.xlsx")

# Verificar el contenido
head(pino)


# Convertir a factores
pino$bloque <- as.factor(pino$bloque)
pino$trat <- as.factor(pino$trat)

modelo <- aov(FP ~ trat + bloque, data = pino)
summary(modelo)

# Crear matriz de contrastes
contrastes <- matrix(c(
  5, -1, -1, -1, -1, -1,     # C1
  0, -1, -1, -1, -1,  4,     # C2
  0, -1, -1, -1,  3,  0,     # C3
  0, -1, -1,  2,  0,  0,     # C4
  0,  1, -1,  0,  0,  0      # C5
), nrow = 5, byrow = TRUE)

# Asignar nombres
rownames(contrastes) <- c("Testigo vs resto", "FGC vs adiciones", 
                          "FGCB vs FGCP", "FGCP100 vs FGCP5025", 
                          "FGCP25 vs FGCP50")

# Asignar contrastes a la variable
contrasts(pino$trat) <- contras <- t(contrastes)

modelo_contrastes <- aov(FP ~ trat + bloque, data = pino)
summary(modelo_contrastes, split = list(trat = list(
  "Testigo vs resto" = 1,
  "FGC vs adiciones" = 2,
  "FGCB vs FGCP" = 3,
  "FGCP100 vs FGCP5025" = 4,
  "FGCP25 vs FGCP50" = 5
)))

# Calcular los valores de los contrastes
colnames(contrastes) <- levels(pino$trat)

# Aplicar contrastes con glht()
res_contrastes <- glht(modelo, linfct = mcp(trat = contrastes))

# Mostrar resumen con valores estimados de los contrastes (con signo)
summary(res_contrastes)

==