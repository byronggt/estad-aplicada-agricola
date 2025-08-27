# Dr. Byron González
# http://byrong.cc

if(!require(readxl)){install.packages("readxl")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(car)){install.packages("car")}
if(!require(multcomp)){install.packages("multcomp")}

# Importar archivo de Excel 
nema <- read_excel("tabla_nematicidas.xlsx")

# Verificar el contenido
head(nema)


# Convertir a factores
nema$rep <- as.factor(nema$rep)
nema$nematicida <- as.factor(nema$nematicida)

modelo <- aov(nemvivos ~ nematicida + rep, data = nema)
summary(modelo)

# Crear matriz de contrastes
contrastes <- matrix(c(
  5, -1, -1, -1, -1, -1,     # C1
  0, 1, 1, 1, -4,  1,        # C2
  0, -1, 1, -1,  0,  1,      # C3
  0, -1, 0,  1,  0,  0,      # C4
  0,  0, -1,  0,  0,  1      # C5
), nrow = 5, byrow = TRUE)

# Asignar nombres
rownames(contrastes) <- c("Testigo vs otros", "Carbofuran vs Oxamyl", 
                          "OxamylF vs OxamylS", "Oxamyl1.5F vs Oxamyl2.0F", 
                          "Oxamyl1.5S vs Oxamyl2.0S")

# Asignar contrastes a la variable
contrasts(nema$nematicida) <- contras <- t(contrastes)

modelo_contrastes <- aov(nemvivos ~ nematicida + rep, data = nema)
summary(modelo_contrastes, split = list(trat = list(
  "Testigo vs otros" = 1,
  "Carbofuran vs Oxamyl" = 2,
  "OxamylF vs OxamylS" = 3,
  "Oxamyl1.5F vs Oxamyl2.0F" = 4,
  "Oxamyl1.5S vs Oxamyl2.0S" = 5
)))

# Calcular los valores de los contrastes
colnames(contrastes) <- levels(nema$nematicida)

# Aplicar contrastes con glht()
res_contrastes <- glht(modelo, linfct = mcp(trat = contrastes))

# Mostrar resumen con valores estimados de los contrastes (con signo)
summary(res_contrastes)

==