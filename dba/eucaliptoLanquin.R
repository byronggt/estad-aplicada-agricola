# Dr. Byron González
# http://byrong.cc

# Detalles del experimento disponibles en la página 83
# del libro de texto "Diseño y análisis de experimentos" de
# López B. y González B. (2016) CETE, Facultad de Agronomía

if(!require(data.table)){install.packages("data.table")}
if(!require(performance)){install.packages("performance")}
if(!require(car)){install.packages("car")}
if(!require(AgroR)){install.packages("AgroR")}

DBA <- fread("https://archive.org/download/byrong_DBA1/DBA1.txt",header=T, sep="\t", dec=",")

attach(DBA)
trat<-factor(Material)
bloque<-factor(Bloque)
alt<-as.vector(Altura)
altura<-as.numeric(alt)

# Diagramas de dispersión para la altura
plot(altura~trat, col="cyan")

# Análisis de varianza para la altura de acuerdo
# al modelo matemático-estadístico
# altura = media + trat + error

resultado<-lm(altura~trat+bloque)
anova(resultado)

# Revisión de los supuestos del modelo
par(mfrow = c(2,2))
plot(resultado)
check_normality(resultado)
win.graph(11,11)
check_model(resultado)

# Prueba de Scott Knott (propósitos ilustrativos)
with(DBA,DBC(trat,bloque,altura,mcomp = "sk"))

# Croquis de campo
library(FielDHub)
run_app()

