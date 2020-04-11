library(magrittr)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyverse)
library(ggExtra)

frecuenciaCuantitativa <- function(tabla) {
  frecAbs <- table(tabla)
  frecRel <- round(frecAbs/length(tabla), digits = 2)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(frecAbsAcum/length(tabla), digits = 2)
  return(cbind(frecAbs, frecRel, frecAbs, frecAbsAcum, frecRelAcum))
}

frecuenciaCualitativa <- function(tabla) {
  frecAbs <- table(tabla)
  frecRel <- round(frecAbs/length(tabla), digits = 2)
  return(cbind(frecAbs, frecRel, frecAbs))
}

#Tabla de frecuencia de verdad
Hist_tabla<- function (variable,min,max,amplitud){
  factorx <- factor(cut(variable, breaks=seq(min,max,amplitud), right = FALSE))
  df_tabla <- as.data.frame(table(factorx))
  relative <- prop.table(df_tabla$Freq)
  df_tabla <- cbind(transform(df_tabla, cumFreq = cumsum(Freq), cum.prop=cumsum(relative)),relative)
  df_tabla <- df_tabla[,c(1,2,3,5,4)]
  colnames(df_tabla)<-c("Intervalos","Frecuencia absoluta","Frecuencia acumulada","Proporción","Acumulada")
  return (df_tabla)
}

df = data.frame(table(Especie=base4$especie))
attach(df)
df$FrecunciaAcumulada = cumsum(Freq)
df$FrecuenciaRelativa = Freq/350
attach(df)
df$FrecuenciaRelativaAcumulada = cumsum(FrecuenciaRelativa)
df = df %>% rename(Frecuencia = Freq)


centrar = theme(plot.title = element_text(hjust = 0.5))
#Amplitud de intervalos = (max-min)/cantidad
#Cantidad de intervalos = piso(raiz(length))

#Graficos de la altura
#Media = 14.46857
#Mediana = 15
#Moda =  13-16
#Variancia = 49.79127
#Desviacion estandar = 7.056293
#Histograma + Poligono de frecuencia
base = ggplot(base4, aes(altura)) 
base + geom_histogram(binwidth = 3,boundary = 1,col = "black", fill = "skyblue", closed = "left") + scale_x_continuous(breaks = seq(1,40,3))+ scale_y_continuous(breaks = seq(0,70,10)) + ggtitle("Altura de los árboles en el centro de Buenos Aires\nAño 2011") + labs(y = "Cantidad de árboles", x = "Altura en metros") +  centrar 
#Dijo Gabina que mejor hacer la tabla de frecuencias
tabla_altura = Hist_tabla(base4$altura,1,40,3)




#Graficos del diametro
#Media = 38.78571
#Mediana = 32
#Moda = 14-28
#Variancia = 899.9912
#Desviacion estandar = 29.99985
#Histograma 
base = ggplot(base4, aes(diametro)) 
base + geom_histogram(col = "black", fill = "skyblue", boundary = 0, breaks = c(0,15,30,45,60,75,90,105,120,165,210,260), closed = "left") + scale_x_continuous(breaks = seq(0,260,15))+ scale_y_continuous(breaks = seq(0,110,10)) + ggtitle("Diámetro de los árboles en el centro de Buenos Aires\nAño 2011") + labs(y = "Cantidad", x = "Diámetro") +  centrar 
#Dijo Gabina que mejor hacer la tabla de frecuencias
# tabla_altura = Hist_tabla(base4$diametro,2,285,15)
# Hacerla a manopla





#Graficos de inclinacion
#Media = 2.502857
#Mediana = 0
#Moda = 0-3
#Variancia = 28.33094
#Desviacion estandar = 5.322681
#Histograma + Poligono de frecuencia
base = ggplot(base4, aes(inclinacion)) 
base + geom_histogram(binwidth = 3,boundary = 0, col = "black", fill = "skyblue", breaks = c(0,3,6,9,12,15,30,45), closed = "left") + scale_x_continuous(breaks = seq(0,45,3))+ scale_y_continuous(breaks = seq(0,300,50)) + ggtitle("Inclinación de los árboles en el centro de Buenos Aires\nAño 2011") + labs(y = "Cantidad", x = "Inclinación")+ centrar +  theme(plot.title = element_text(hjust = 0.5)) 
# Dijo Gabina que mejor hacer la tabla de frecuencias
# Hacerla a manopla





#Graficos de origen
#Torta
#Moda = Exotico
df = data.frame(Origen = c("Nativo/Autóctono", "Exótico"), value = c(101,249))
ggplot(df, aes(x="",y=value,fill=Origen)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0) + theme_void() + ggtitle("Origen de los árboles en el centro de Buenos Aires \n Año 2011") + centrar
#Preguntar como poner los porcentajes






#Graficos de especie
#Barras
df = data.frame(table(base4$especie))
ggplot(df, aes(x=Var1, y = sort(Freq))) + geom_col(width = 0.2)  + labs(y = "Cantidad", x = "Especie") + ggtitle("Árboles por especie") + centrar






#Graficos de brotes
#Bastones
#Media = 3.614286
#Mediana = 3
#Moda = 3
#Variancia = 1.979738
#Desvio estandar = 1.407032
df = data.frame(table(base4$brotes))
ggplot(df, aes(Var1,sort(Freq))) +geom_segment( aes(x=Var1, xend=Var1, y=0, yend=sort(Freq)),lwd = 1, color ="black" ) + geom_point(size = 2, color = "black", shape = 19)+ scale_y_continuous(breaks = seq(0,110,10)) + labs(x ="Cantidad de brotes", y = "Cantidad de árboles") + ggtitle("Cantidad de brotes por árbol en el centro de Buenos Aires \n Año 2011") + centrar 





#Arbol mas torcido
#Es el eucalipto
ggplot(base4,aes(inclinacion,especie)) + geom_boxplot(fill="skyblue", alpha=1, outlier.shape = "cross", outlier.size = 3, outlier.color = "Black")





#Arbol mas alto
ggplot(base4,aes(altura,especie)) + geom_boxplot(fill="skyblue", alpha=1, outlier.shape = "cross", outlier.size = 3, outlier.color = "Black")




#Brotes por especie
brot <- base4 %>% group_by(especie) %>% summarise(cantBrotes = sum(brotes))
ggplot(brot,aes(especie,sort(cantBrotes))) + geom_point(size = 1)+geom_segment( aes(x=especie, xend=especie, y=0, yend=sort(cantBrotes))) + scale_y_continuous(breaks = seq(0,250,15)) + labs(x ="Cantidad de brotes", y = "Cantidad de árboles") + ggtitle("Cantidad de brotes por árbol en el centro de Buenos Aires \n Año 2011") + centrar
#Brotes según nativo/exótico
brot <- base4 %>% group_by(origen) %>% summarise(cantBrotes = sum(brotes))
ggplot(brot, aes(x="",y=cantBrotes,fill=origen)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0) + theme_void()


#Cantidad por especie
cantEspecie <- data.frame(table(base4$especie))
ggplot(cantEspecie,aes(reorder(Var1,Freq),Freq)) + geom_col( width = 0.2) + scale_y_continuous(breaks = seq(0,80,5))

#Alto diametro
#df <- data.frame(table(base4$altura,base4$diametro))
ggplot(base4, aes(x=altura, y=diametro)) + 
  geom_point(size=3) + geom_smooth(method = 'lm',)

ggplot(base4, aes(x=altura, y=inclinacion)) + 
  geom_point(size=3) + geom_smooth(method = 'lm',)



  
  
