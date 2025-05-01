# Dr. Byron González
# http://byrong.cc

if(!require(data.table)){install.packages("data.table")}
if(!require(performance)){install.packages("performance")}
if(!require(AgroR)){install.packages("AgroR")}


fact<- fread("https://archive.org/download/Brocoli/Brocoli.txt",header=T, sep="\t", dec=",")

attach(fact)
head(fact)
hibrido<-factor(Hib)
distanciamiento<-factor(Dist)
bloque<-factor(Bloque)
ren<-as.vector(Rend)
rendimiento<-as.numeric(ren)

# Gráfico de la interacción entre factores
interaction.plot( distanciamiento,hibrido,rendimiento, fixed=T, xlab="Distanciamiento", ylab="Rendimiento",col = "blue")

# Análisis de varianza en arreglo combinatorio para ambos factores
resultado<-lm(rendimiento~bloque+hibrido*distanciamiento)
anova(resultado)

# Revisión de los supuestos del modelo
par(mfrow = c(2,2))
plot(resultado)
check_normality(resultado)
win.graph(11,11)
check_model(resultado)

# Prueba múltiple de medias bajo el criterio de Scott Knott
with(fact,FAT2DBC(hibrido,distanciamiento,bloque,rendimiento,mcomp = "sk"))