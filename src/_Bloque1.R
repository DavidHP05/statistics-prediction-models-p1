#Ejercicio 1, calcular media, mediana y desviación típica
datos <- read.csv("data/students_A.csv")
media       <- mean(datos$nota, na.rm = TRUE)
mediana     <- median(datos$nota, na.rm = TRUE)
desv_tipica <- sd(datos$nota, na.rm = TRUE)

#Mostramos los datos
cat("Media:", round(media, 4), "\n")
cat("Mediana:", round(mediana, 4), "\n")
cat("Desviación típica:", round(desv_tipica, 4), "\n")
#Mera curiosidad
nota_maxima <- max(datos$nota, na.rm = TRUE)
print(nota_maxima)


#Apartado 2, el histograma

library(ggplot2)
ggplot(datos, aes(x = nota)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Histograma de Nota",
       x = "Nota",
       y = "Frecuencia") +
  theme_minimal()

#Apartado 3 proporciones de sexo

#Calcular la tabla de frecuencias absolutas
conteo_sexo <- table(datos$sexo)
#Calculamos las proporciones
proporciones <- prop.table(conteo_sexo)
print(proporciones)


#Diagrama de cajas según campus
ggplot(datos, aes(x = campus, y = nota, fill = campus)) +
  geom_boxplot() +
  labs(title = "Diagrama de Cajas de Nota según Campus",
       x = "Campus",
       y = "Nota") +
  theme_minimal() +
  theme(legend.position = "none")


#Gráfico de dispersión 
ggplot(datos, aes(x = horas_estudio, y = nota)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Gráfico de Dispersión: Horas de Estudio vs Nota",
       x = "Horas de Estudio",
       y = "Nota") +
  theme_minimal()

#Matriz de correlaciones
#Seleccionamos las variables numéricas
vars_numericas <- datos[, sapply(datos, is.numeric)]

#Cálculo de la matriz de correlación
matriz_cor <- round(cor(vars_numericas, use = "complete.obs"), 2)
print(matriz_cor)

#Hay que ver como ponerlo más bonito
library(corrplot)
corrplot(matriz_cor, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")