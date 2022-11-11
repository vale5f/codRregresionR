# REGRESIONES LINEALES SIMPLES

# Ejercicio 1:

# Se sabe que la Temperatura y la Altitud están fuertemente relacionadas. Se tiene los 
# siguientes datos:
# (Altitud, Temperatura)
# (100, 30), (300, 28), (500, 27), (700, 25), (900, 24), (1200, 22), (1900, x)

# Se pide hallar lo siguiente:
# 1. Le ecuación de la regresión lineal Y = B0 + B1X
# 2. La correlación de las variables
# 3. Rechar o aceptar H0 (Hipótesis nula que B1 = 0), o la Hipótesis alternativa (H1)
# 4. El test de confiabilidad, el nivel de significancia P<0.05.
# 5. Luego calcule en R todo lo anterior y contraste resultados obtenidos


## Solucion en R

temp <- c(30, 28, 27.6, 26, 23, 22)
alt <- c(100, 300, 500, 700, 900, 1200)

data <- cbind(temp, alt)

# visualizar datos
head(data)

# dimension de los datos
dim(data)

# cambiar name de cols
colnames(data) <- c("Temperatura", "Altitud")
head(data)

# Grafica de los datos
plot(data[,1], data[,2])

x <- data[,1]
y <- data[,2]
plot(x, y, type = 'p', col = 'blue', lwd = 2)

# regresion lineal
reg <- lm(y ~ x)

# podemos ver los coeficientes
reg

# Graficamos la regresion lineal
plot(y~x, type='p', xlab="time", ylab="AirPassengers")
lines(predict(reg)~x, lty=1, col='red', lwd=2)

# Resumen matemático de la regresion
summary(reg)

# Predecir la temperatura para 1900 de altitud

temp_1900 <- (3952.0 - 1900)/127.8
temp_1900

# FUNCIONES ARMONICAS
# Ejercicio 2.

plot(AirPassengers)
y <- as.vector(AirPassengers)
x <- as.vector(time(AirPassengers))

# Dos armónicos
r <- lm(y ~ x + sin(2*pi*x) + cos(2*pi*x))
plot(y~x, type='l', xlab="time", ylab="AirPassengers")
lines(predict(r)~x, lty=3, col='red', lwd=3)

# Resumen
summary(r)

# Aumentamos los armónicos
r <- lm( y ~ x + cos(pi*x) + sin(2*pi*x) + cos(4*pi*x) + sin(4*pi*x))
plot(y~x, type='l', xlab="time", ylab="AirPassengers")
lines(predict(r)~x, lty=3, col='blue', lwd=3)

# Resumen
summary(r)
