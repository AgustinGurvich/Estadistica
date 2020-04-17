library(magrittr)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyverse)
library(ggExtra)
library(scales)

frecuenciaCuantitativa <- function(tabla) {
  frecAbs <- table(tabla)
  frecRel <- round(frecAbs/length(tabla), digits = 4)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(frecAbsAcum/length(tabla), digits = 4)
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


centrar = theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"), axis.title=element_text(size=18,face="bold"), axis.text.y =element_text(size=15),axis.text.x =element_text(size=15))
a45 = theme(axis.text.x = element_text(angle = 45, hjust =1))
colorDeSanti = rgb(red = 31,green = 181,blue = 234,maxColorValue = 255)
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
base + geom_histogram(binwidth = 3,boundary = 1,col = "black", fill = colorDeSanti, closed = "left")+ scale_x_continuous(breaks = seq(1,40,3))+ scale_y_continuous(breaks = seq(0,70,10)) +  labs(y = "Frecuencia absoluta", x = "Altura en metros") +  centrar 
intervalos = seq(1,40,3)
freqAbs = table(cut(base4$altura,intervalos,right = FALSE))
freqRel = freqAbs/length(base4$altura)
freqAbsAcum = cumsum(freqAbs)
freqRelAcum = freqAbsAcum/length(base4$altura)
tabla_altura = as.data.frame(cbind(freqAbs,freqRel,freqAbsAcum,freqRelAcum))
plot(c(0, tabla_altura$freqRelAcum), type = "l", xlab = "Altura en metros", ylab = "Frecuencia relativa acumulada", xaxt = "n", xlim = c(1,14),cex.lab = 1.3)
axis(side = 1, at = (1:14), labels = seq(1, 40, 3))
abline(h = seq(0, 1, 0.2), lty = 3)




#Graficos del diametro
#Media = 38.78571
#Mediana = 32
#Moda = 30-45
#Variancia = 899.9912
#Desviacion estandar = 29.99985
#Histograma 
base = ggplot(base4, aes(diametro)) 
base + geom_histogram(col = "black", fill = colorDeSanti, boundary = 0, breaks = c(0,15,30,45,60,75,90,105,120,165,210,261), closed = "left") + scale_x_continuous(breaks = seq(0,260,15))+ scale_y_continuous(breaks = seq(0,110,10)) + labs(y = "Frecuencia absoluta", x = "Diámetro en centímetros") +  centrar 
intervalos = c(0,15,30,45,60,75,90,105,120,165,210,261)
freqAbs = table(cut(base4$diametro,intervalos,right = FALSE))
freqRel = freqAbs/length(base4$diametro)
freqAbsAcum = cumsum(freqAbs)
freqRelAcum = freqAbsAcum/length(base4$diametro)
tabla_diametro = as.data.frame(cbind(freqAbs,freqRel,freqAbsAcum,freqRelAcum))
plot(c(0, tabla_diametro$freqRelAcum), type = "l", xlab = "Diámetros en centímetros", ylab = "Frecuencia relativa acumulada", xaxt = "n", xlim = c(1,12), cex.lab = 1.3)
axis(side = 1, at = (1:12), labels = intervalos)
abline(h = seq(0, 1, 0.2), lty = 3)





#Graficos de inclinacion
#Media = 2.502857
#Mediana = 0
#Moda = 0-3
#Variancia = 28.33094
#Desviacion estandar = 5.322681
#Histograma + Poligono de frecuencia
base = ggplot(base4, aes(inclinacion)) 
base + geom_histogram(boundary = 0, col = "black", fill = colorDeSanti, breaks = c(0,3,6,9,12,15,30,46), closed = "left") + scale_x_continuous(breaks = seq(0,45,3))+ scale_y_continuous(breaks = seq(0,300,30))+ labs(y = "Frecuencia absoluta", x = "Inclinación en grados")+ centrar +  theme(plot.title = element_text(hjust = 0.5)) 
intervalos = c(0,3,6,9,12,15,30,46)
freqAbs = table(cut(base4$inclinacion,intervalos,right = FALSE))
freqRel = freqAbs/length(base4$inclinación)
freqAbsAcum = cumsum(freqAbs)
freqRelAcum = freqAbsAcum/length(base4$inclinación)
tabla_inclinacion = as.data.frame(cbind(freqAbs,freqRel,freqAbsAcum,freqRelAcum))
plot(c(0, tabla_inclinacion$freqRelAcum), type = "l", xlab = "Inclinación en grados", ylab = "Frecuencia relativa acumulada", xaxt = "n", xlim = c(1,8),cex.lab = 1.3)
axis(side = 1, at = (1:8), labels = intervalos)
abline(h = seq(0, 1, 0.2), lty = 3)




#Graficos de origen
#Torta
#Moda = Exotico
df = data.frame(Origen = c("Nativo/Autóctono", "Exótico"), value = c(101,249))
ggplot(df, aes(x="",y=value,fill=Origen)) +geom_bar(stat="identity", width=1, colour = "black") +coord_polar("y", start=0) + theme_void()  + centrar +  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]),label = percent(value/350)), size=6) + labs(x = "", y ="") + theme_void()





#Graficos de especie
#Barras
df = data.frame(table(base4$especie))
ggplot(df, aes(x=Freq, y = reorder(Var1,Freq))) + geom_bar(stat = "identity", fill = colorDeSanti, colour = "black")  + labs(x = "Frecuencia absoluta", y = "Especie") + scale_x_continuous(breaks = seq(0,80,10)) + centrar
base5 = base4 %>% rename(Origen = origen)
ggplot(base4, aes(x=especie, fill = Origen)) + geom_bar(colour = "black")  + labs(y = "Frecuencia absoluta", x = "Especie")  +centrar + coord_flip() + scale_y_continuous(breaks = seq(0,80,10))
datosPie <- base4 %>%
  group_by(especie) %>%
  count() %>%
  ungroup() %>%
  mutate(per = `n`/sum(`n`)) %>%
  arrange(desc(especie))
robert <- scales::percent(datosPie$per)
datosPie = datosPie %>% rename(Especie = especie)
ggplot(datosPie, aes(x="", y = per,fill = Especie)) + geom_bar(stat = "identity", width = 1, colour = "black") +coord_polar("y", start=0) + theme_void() + geom_text(aes(x = 1,y = cumsum(per) - per / 2,label = robert), nudge_x = 0.60, size = 4)


#Graficos de brotes
#Bastones
#Media = 3.614286
#Mediana = 3
#Moda = 3
#Variancia = 1.979738
#Desvio estandar = 1.407032
df = data.frame(table(base4$brotes))
ggplot(df, aes(Var1,Freq)) +geom_col(width = 0.2, fill = colorDeSanti, colour = "Black", size=1)+scale_y_continuous(breaks = seq(0,110,10)) + labs(x ="Cantidad de brotes", y = "Frecuencia absoluta") + centrar 
acumulacion = cumsum(df$Freq)/length(base4$brotes)
df = cbind(df,acumulacion)
ggplot(df, aes(Var1,acumulacion)) + geom_step(group = 1, linetype = 1) + geom_point(size = 2, shape = 21, fill = colorDeSanti) + labs(x = "Cantidad de brotes", y = "Frecuencia relativa acumulada") + scale_y_continuous(breaks = seq(0,1,0.2)) + centrar



#Arbol mas torcido
#Es el eucalipto
ggplot(base4,aes(inclinacion,especie)) + geom_boxplot(fill=colorDeSanti, alpha=0.6, outlier.shape = "cross", outlier.size = 3, outlier.color = "Black") + labs(x = "Inclinación  en grados", y = "Especie")  + centrar





#Arbol mas alto
ggplot(base4,aes(altura,especie)) + geom_boxplot(fill=colorDeSanti, alpha=0.6, outlier.shape = "cross", outlier.size = 3, outlier.color = "Black") + labs(x = "Altura en metros", y = "Especie")   + centrar + scale_x_continuous(breaks = seq(0,40,5))




#Brotes por especie
brot <- base4 %>% group_by(especie) %>% summarise(cantBrotes = sum(brotes))
ggplot(brot,aes(especie,sort(cantBrotes))) + geom_point(size = 2, shape = 21, fill = colorDeSanti)+geom_segment( aes(x=especie, xend=especie, y=0, yend=(sort(cantBrotes)-1))) + scale_y_continuous(breaks = seq(0,255,15)) + labs(x ="Especie", y = "Cantidad de brotes") + centrar + coord_flip()
#Brotes según nativo/exótico
brot <- base4 %>% group_by(origen) %>% summarise(cantBrotes = sum(brotes))
brot <- brot %>% rename(Origen = origen)
ggplot(brot, aes(x="",y=cantBrotes,fill= Origen)) +geom_bar(stat="identity", width=1, color = "black") + coord_polar("y", start=0) + theme_void() +
  geom_text(aes(y =rev(cantBrotes/3 + c(0, 769)),label = percent(cantBrotes/1265)), size=5)

#Averiguar como cambiar de lugar las etiquetas


label = brot$Origen
label <- label %>%  paste(c("61%","39%"))
pie(brot$cantBrotes,label,col = c("grey",colorDeSanti))
  

#Cantidad por especie
cantEspecie <- data.frame(table(base4$especie))
ggplot(cantEspecie,aes(reorder(Var1,Freq),Freq)) + geom_bar(stat = "identity", fill = colorDeSanti, colour = "Black") + scale_y_continuous(breaks = seq(0,80,10)) + labs(x = "Especie", y = "Cantidad de árboles") + centrar + coord_flip()

#Alto diametro
ggplot(base4, aes(x=altura, y=diametro)) + geom_point(size=3,shape = 21 ,fill = colorDeSanti) + geom_smooth(method = 'lm',colour = "Black") + labs(x = "Altura en metros", y = "Diámetro en centímetros") + centrar + scale_x_continuous(breaks = seq(0,40,5)) + ylim(0,300)
ggplot(base4, aes(x=altura, y=inclinacion)) + geom_point(size=3,shape = 21 ,fill = colorDeSanti) + geom_smooth(method = 'lm',colour = "Black") + labs(x="Altura en metros", y = "Inclinación en grados") + centrar+ scale_x_continuous(breaks = seq(0,40,5)) + ylim(0,50)



