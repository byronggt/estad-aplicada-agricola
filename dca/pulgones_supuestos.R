# Dr. Byron González
# http://byrong.cc

# Detalles del experimento disponibles en la página 73
# del libro de texto "Diseño y análisis de experimentos" de
# López B. y González B. (2016) CETE, Facultad de Agronomía

if(!require(data.table)){install.packages("data.table")}
if(!require(performance)){install.packages("performance")}
if(!require(car)){install.packages("car")}
if(!require(AgroR)){install.packages("AgroR")}

DICt<- fread("https://archive.org/download/pulgones/pulgones.txt",header=T, sep="\t", dec=",")
head(DICt)
str(DICt)
attach(DICt)
trat<-factor(trat)
pul<-as.vector(pulgones)
pulg<-as.numeric(pul)

# Diagramas de dispersión por tratamiento
plot(pulg~trat, col="orange")

# Análisis de varianza
resultado<-lm(pulg~trat)
anova(resultado)

# Revisión de los supuestos del Andeva
par(mfrow = c(2,2))
plot(resultado)
check_normality(resultado)

# Transformación de la variable original mediante la familia de Box-Cox
summary(powerTransform(DICt$pulgones)) # Aplicar potencia 0.0424

# Realizar el Andeva con la variable transformada
pulg_t<-(pulg^0.0424-1)/0.0424; pulg_t
resultado1<-lm(pulg_t~trat)
anova(resultado1)

# Revisión de supuestos con la variable transformada
par(mfrow = c(2,2))
plot(resultado1)
check_normality(resultado1)

# Prueba múltiple de medias bajo el criterio de Scott Knott
with(DICt,DIC(trat,pulg_t,mcomp = "sk"))

# Croquis de campo
library(FielDHub)
run_app()