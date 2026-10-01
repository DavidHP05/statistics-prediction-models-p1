#Ejercicio 1, calcular media, mediana y desviación típica
datos <- read.csv("students_A.csv")
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