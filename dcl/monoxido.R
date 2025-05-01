# Dr. Byron González
# http://byrong.cc

if(!require(data.table)){install.packages("data.table")}
if(!require(performance)){install.packages("performance")}
if(!require(AgroR)){install.packages("AgroR")}


dcl<- fread("https://archive.org/download/byrong_DCL/DCL.txt",header=T, sep="\t", dec=",")

attach(dcl)
puntos<-factor(Puntos)
dia<-factor(Dia) # fila
hora<-factor(Hora) # columna
ppm1<-as.vector(ppm)
monoxido<-as.numeric(ppm1)

# Diagramas de dispersión para el monóxido
plot(monoxido~puntos, col="gray")

# Análisis de varianza para el monóxido
resultado<-lm(monoxido~puntos+dia+hora)
anova(resultado)

# Revisión de los supuestos del Andeva
par(mfrow = c(2,2))
plot(resultado)
check_normality(resultado)
check_model(resultado)

# Prueba múltiple de medias bajo el criterio de Scott-Knott
with(dcl,DQL(puntos,dia,hora,monoxido,mcomp = "sk"))

# Croquis de campo
library(FielDHub)
run_app()

