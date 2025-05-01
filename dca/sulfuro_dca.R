# Dr. Byron González
# http://byrong.cc

if(!require(readxl)){install.packages("readxl")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(performance)){install.packages("performance")}
sulfuros<-read_excel("sulfuros.xlsx")
head(sulfuros)
attach(sulfuros)
lab<-factor(laboratorio)
plot(sulfuro~lab, col="green")
resultado<-aov(sulfuro~lab)
anova(resultado)
mod<-lm(sulfuro~lab)
sulfuros$predichos<-mod$fitted.values
sulfuros$resid<-mod$residuals
head(sulfuros)
anova(mod, test=F)
win.graph(11,11)
check_model(mod)
check_normality(mod)

#Prueba de Tukey
Tukey<-HSD.test(resultado,"lab",console=T)
