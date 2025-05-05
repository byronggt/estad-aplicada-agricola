# Dr. Byron González
# http://byrongcc

if(!require(readxl)){install.packages("readxl")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(samplingbook)){install.packages("samplingbook")}

#===============Muestreo simple aleatorio=======================================
data<-read_excel("data_brix.xlsx")
set.seed(123)
head(data)
colnames(data)

# Tomar 30 datos iniciales para calcular tamaño de muestra
muestra <- sample(1:nrow(data), size = 30, replace=FALSE)
muestra
data_msa <- data[muestra, ]
print(data_msa, n=30)

# Calcular el valor de "e" 
precision=qnorm(0.025, lower.tail = F)*(sd(data_msa$punta)/sqrt(30));precision

# Calcular el tamaño de muestra para un e y sd
# Aquí se ha seleccionado un e= 0.5
n_muestra <- sample.size.mean(e = 0.5, sd(data_msa$punta), N = Inf, level = 0.95)
n_muestra

# Completar muestra considerando los 30 iniciales
data_restante <- data[-muestra, ] # A la tabla original se resta 30
muestra_complemento <- sample(1:nrow(data_restante),size=(n_muestra$n-30), replace=FALSE)
muestra_definitiva<-rbind(data[muestra_complemento, ], data_msa)
print(muestra_definitiva, n=61)

ic_mean<- MeanCI(x=muestra_definitiva$punta, trim = 0, conf.level = 0.95, na.rm = FALSE);ic_mean
