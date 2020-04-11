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

#Pasar los dos primeros argumentos con unique() primero
promedioPorFilas <- function(categoria,valoresDeCategoria, tabla){
  valoresDeCategoria = sort(valoresDeCategoria)
  categoria = sort(categoria)
  i = 1
  j = 1
  contador = 0
  cantidadSumada = 0
  promedios = c()
  for (i in seq(1:length(categoria))){
    for(j in seq(1:length(valoresDeCategoria))){
      contador = contador + tabla[i,j] * valoresDeCategoria[j]
      cantidadSumada = cantidadSumada + tabla[i,j]
    }
  promedios = c(promedios, contador/cantidadSumada)
  contador = 0
  }
  #maximo = max(promedios)
  #indicemax = match(maximo, promedios)
  # return(c(categoria[indicemax],maximo))
  print(promedios)
  return(promedios)  
}

centrar = theme(plot.title = element_text(hjust = 0.5))
#Amplitud de intervalos = (max-min)/cantidad
#Cantidad de intervalos = piso(raiz(length))

#Graficos de la altura
  #Media = 14.46857
  #Mediana = 15
  #Moda =  15-17
  #Variancia = 49.79127
  #Desviacion estandar = 7.056293
  #Histograma + Poligono de frecuencia
  base = ggplot(base4, aes(altura)) 
  base + geom_histogram(bins = 20,col = "black", fill = "skyblue") + geom_freqpoly(bins = 20,size = 1.2) + scale_x_continuous(breaks = seq(1,40,2))+ scale_y_continuous(breaks = seq(0,45,5)) + ggtitle("Altura de los árboles en el centro de Buenos Aires\nAño 2011") + labs(y = "Cantidad", x = "Altura") +  centrar
  #Ver que onda el otro poligono

#Graficos del diametro
  #Media = 38.78571
  #Mediana = 32
  #Moda = 14-28
  #Variancia = 899.9912
  #Desviacion estandar = 29.99985
  #Histograma + Poligono de frecuencia
  base = ggplot(base4, aes(diametro)) 
  base + geom_histogram(bins =18,col = "black", fill = "skyblue") + geom_freqpoly(bins =18,size = 1) + scale_x_continuous(breaks = seq(0,260,14))+ scale_y_continuous(breaks = seq(0,110,10)) + ggtitle("Diámetro de los árboles en el centro de Buenos Aires\nAño 2011") + labs(y = "Cantidad", x = "Diámetro") +  centrar 

#Graficos de inclinacion
  #Media = 2.502857
  #Mediana = 0
  #Moda = 0-3
  #Variancia = 28.33094
  #Desviacion estandar = 5.322681
  #Histograma + Poligono de frecuencia
  base = ggplot(base4, aes(inclinacion)) 
  base + geom_histogram(binwidth = 3, col = "black", fill = "skyblue") + geom_freqpoly(binwidth =3,size = 1) + scale_x_continuous(breaks = seq(0,45,3))+ scale_y_continuous(breaks = seq(0,300,50)) + ggtitle("Inclinación de los árboles en el centro de Buenos Aires\nAño 2011") + labs(y = "Cantidad", x = "Inclinación")+ centrar +  theme(plot.title = element_text(hjust = 0.5)) 

#Graficos de origen
  #Torta
  #Moda = Exotico
  df = data.frame(Origen = c("Nativo/Autóctono", "Exótico"), value = c(101,249))
  ggplot(df, aes(x="",y=value,fill=Origen)) +geom_bar(stat="identity", width=1) +coord_polar("y", start=0) + theme_void() + ggtitle("Origen de los árboles en el centro de Buenos Aires \n Año 2011") + centrar

#Graficos de especie
  #Barras
  df = data.frame(table(base4$especie))
  ggplot(df, aes(x=Var1, y = Freq)) + geom_bar(stat = "identity") + labs(y = "Cantidad", x = "Especie") + ggtitle("Árboles por especie") + centrar
  
#Graficos de brotes
  #Bastones
  #Media = 3.614286
  #Mediana = 3
  #Moda = 3
  #Variancia = 1.979738
  #Desvio estandar = 1.407032
  df = data.frame(table(base4$brotes))
  ggplot(df, aes(Var1,Freq)) + geom_point(size = 1)+geom_segment( aes(x=Var1, xend=Var1, y=0, yend=Freq)) + scale_y_continuous(breaks = seq(0,110,10)) + labs(x ="Cantidad de brotes", y = "Cantidad de árboles") + ggtitle("Cantidad de brotes por árbol en el centro de Buenos Aires \n Año 2011") + centrar 

#Arbol mas torcido
  #Es el eucalipto
  torcidoProm <- base4 %>% group_by(especie) %>% summarise(inclinacionProm = mean(inclinacion))
  ggplot(torcidoProm,aes(x=especie,y=inclinacionProm))+ geom_bar(stat = "identity") + labs(y = "Cantidad", x = "Especie") + ggtitle("Inclinacion de árboles por especie") + centrar + scale_y_continuous(breaks = seq(0,8,1))

#Arbol mas alto
  alturasMax <- base4 %>% group_by(especie) %>% summarise(alturaMax = max(altura))
  ggplot(alturasMax,aes(especie,alturaMax)) + geom_boxplot(size = 1)+geom_segment( aes(x=especie, xend=especie, y=0, yend=alturaMax)) + scale_y_continuous(breaks = seq(0,110,10)) + labs(x ="Cantidad de brotes", y = "Cantidad de árboles") + ggtitle("Cantidad de brotes por árbol en el centro de Buenos Aires \n Año 2011") + centrar

#Altura promedio
  # alturasProm <- base4 %>% group_by(especie) %>% summarise(alturaProm = mean(altura))
  ggplot(base4,aes(altura,especie)) + geom_boxplot(fill="black", alpha=0.2)

#Brotes por especie
  brot <- base4 %>% group_by(especie) %>% summarise(cantBrotes = sum(brotes))
  ggplot(brot,aes(especie,cantBrotes)) + geom_point(size = 1)+geom_segment( aes(x=especie, xend=especie, y=0, yend=cantBrotes)) + scale_y_continuous(breaks = seq(0,250,12.5)) + labs(x ="Cantidad de brotes", y = "Cantidad de árboles") + ggtitle("Cantidad de brotes por árbol en el centro de Buenos Aires \n Año 2011") + centrar

#Cantidad por especie
  cantEspecie <- data.frame(table(base4$especie))
  ggplot(cantEspecie,aes(reorder(Var1,Freq),Freq)) + geom_col() + scale_y_continuous(breaks = seq(0,80,5))
  
#Alto diametro
  #df <- data.frame(table(base4$altura,base4$diametro))
  ggplot(base4, aes(x=altura, y=diametro)) + 
    geom_point(size=3) + geom_smooth(method = 'lm',)
  
  ggplot(base4, aes(x=altura, y=inclinacion)) + 
    geom_point(size=3) + geom_smooth(method = 'lm',)
 
  ggplot(base4,aes(inclinacion,especie)) + geom_boxplot(fill="black", alpha=0.2)
  
  