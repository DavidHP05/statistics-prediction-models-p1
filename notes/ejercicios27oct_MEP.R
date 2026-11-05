# EJERCICIO 3
datos <- pressure
modelo <- lm(pressure ~ temperature, datos)
summary(modelo)

# EJERCICIO 4
nuevos_datos <- data.frame(temperature = 250)
predict(modelo, newdata = nuevos_datos, interval = "confidence", level = 0.95)
predict(modelo, newdata = nuevos_datos, interval = "prediction", level = 0.95)

# Scatterplot
plot(datos$temperature, datos$pressure,
     main = "Relación entre Temperatura y Presión",
     xlab = "Temperatura",
     ylab = "Presión",
     pch = 19,
     col = "blue")

# Añadir línea de tendencia
abline(lm(pressure ~ temperature, datos), col="red",lwd=2)

modelo2 <- lm(I(log(pressure)) ~ temperature, datos)
summary(modelo2)

#datos_log <- subset(datos, temperature > 0)

modelo3 <- lm(I(log(pressure)) ~ I(log(temperature+273.15)), datos)
summary(modelo3)

plot(log(datos$temperature + 273.15), log(datos$pressure),
     main = "Escala Log-Log (Lineal)",
     xlab = "log(Temperatura en Kelvin)",
     ylab = "log(Presión)",
     pch = 19, col = "darkgreen")

# En esta escala transformada, la línea de tendencia es recta
abline(modelo3, col = "red", lwd = 2)

# EJERCICIO 6
library(car)
modelo6 <- lm(mpg ~ disp + hp + wt + cyl, data = mtcars)
summary(modelo6)
vif(modelo6)
# Matriz de correlaciones y veríamos que disp da más problemas
matriz_cor <- cor(mtcars, use = "pairwise.complete.obs")
print(matriz_cor)
modelo6_2 <- lm(mpg ~ hp + wt + cyl, data = mtcars)
summary(modelo6_2)
vif(modelo6_2)

# EJERCICIO 10
# 1. Ajustas el modelo completo con todas las variables
modelo10 <- lm(Fertility ~ Agriculture + Education, data = swiss)
summary(modelo10)
modelo10_2 <- lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, data = swiss)
summary(modelo10_2)
anova(modelo10, modelo10_2)


# EJERCICIO 7 INGENIERÍA DE CARACTERÍSTICAS
modelo7 <- lm(mpg ~ wt + hp + wt:hp, data = mtcars)
summary(modelo7)


# 



