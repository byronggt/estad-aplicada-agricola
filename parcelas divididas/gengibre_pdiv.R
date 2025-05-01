# Dr. Byron González
# http://byrong.cc

if(!require(data.table)){install.packages("data.table")}
if(!require(performance)){install.packages("performance")}
if(!require(Matrix)){install.packages("Matrix")}
if(!require(AgroR)){install.packages("AgroR")}

pdiv<- fread("https://archive.org/download/Gengibre/Gengibre.txt",header=T, sep="\t", dec=",")

head(pdiv)
attach(pdiv)
gallinaza<-factor(Gal)
urea<-factor(Urea)
bloque<-factor(Bloque)
p1<-as.vector(Peso)
peso<-as.numeric(p1)

# Gráfico de la interacción
interaction.plot(gallinaza,urea,peso, fixed=T, xlab="Gallinaza", ylab="Peso",col = "blue")

# Análisis de varianza para ambos factores
resultado<-aov(peso~bloque+gallinaza+urea+gallinaza*urea+Error(bloque/gallinaza))
summary(resultado)

# Revisión de los supuestos del modelo
resultado1<-aov(peso~bloque+gallinaza*urea+bloque/gallinaza)

par(mfrow = c(2,2))
plot(resultado1)
check_normality(resultado1)
win.graph(11,11)
check_model(resultado1)

# Prueba de Tukey para gallinaza
Tukey_G<-HSD.test(peso, gallinaza, DFerror = 4, MSerror = 644);Tukey_G

# Prueba múltiple de medias bajo el criterio de Scott Knott (pendiente)
with(pdiv,PSUBDBC(gallinaza,urea,bloque,peso, ylab = "Peso"))

#=======
data(tomate)
with(tomate, PSUBDBC(parc, subp, bloco, resp, ylab="Dry mass (g)"))



