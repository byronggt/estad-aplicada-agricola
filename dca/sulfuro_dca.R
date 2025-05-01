# Dr. Byron González
# http://byrong.cc

if(!require(readxl)){install.packages("readxl")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(performance)){install.packages("performance")}
if(!require(ScottKnott)){install.packages("ScottKnott")}
sulfuros<-read_excel("sulfuros.xlsx")
head(sulfuros)
sulfuros$laboratorio<-factor(sulfuros$laboratorio)
sulfuros$sulfuro <- as.numeric(sulfuros$sulfuro) 
str(sulfuros)
plot(sulfuros$sulfuro~sulfuros$laboratorio, col="green")
resultado<-aov(sulfuro~laboratorio, data = sulfuros)
anova(resultado)
mod<-lm(sulfuro~laboratorio, data = sulfuros)
sulfuros$predichos<-mod$fitted.values
sulfuros$resid<-residuals(mod, scientific=F)
head(sulfuros)
tail(sulfuros)
anova(mod, test=F)
win.graph(11,11)
check_model(mod)
check_normality(mod)

#Prueba de Tukey
Tukey<-HSD.test(resultado,"laboratorio",console=T)

# Prueba de Scott Knott (revisar)
sk <- SK(mod, dispersion = "se", sig.level = 0.05)
sk

