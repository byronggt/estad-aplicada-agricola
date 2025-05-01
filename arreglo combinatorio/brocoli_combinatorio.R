# Dr. Byron González
# http://byrong.cc

if(!require(data.table)){install.packages("data.table")}
if(!require(performance)){install.packages("performance")}

fact<- fread("https://archive.org/download/Brocoli/Brocoli.txt",header=T, sep="\t", dec=",")


