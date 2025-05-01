# Dr. Byron González
# http://byrong.cc

if(!require(data.table)){install.packages("data.table")}
if(!require(performance)){install.packages("performance")}

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
win.graph(11,11)
check_model(resultado)
check_normality(resultado) # 

# Transformación de la variable original




