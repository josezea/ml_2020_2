library(nloptr)
library(fastDummies)

# clase 2
setwd("~/Laboral2020/Konrad Lorenz/Semestre2/MachineLearning/1.gradiente_descendente")
datos <- read.csv("houses_portland.csv")
head(datos)

# Ejercicio: convertir los pies cuadrados a metros cuadrados
datos$area <- datos$area * 0.092903

# Realizar exploración de los datos
summary(datos$area)
summary(datos$price)

plot(datos$area, datos$price, pch = 20, xlab = "Área (mts2)",
     ylab = "Precio (US$)")
# Análitico (Minimización de MSE, en R para la regresión)
# Optimización (miimizando MSE por métodos numéricos, 
# gradiente descente, Newton Raphson, Optimización cuadrático)

cor(datos$area, datos$price)
# Y es modelado por X: y ~ X
modelo <- lm(price ~ area, data = datos)
summary(modelo)
plot(datos$area, datos$price, pch = 20, xlab = "Área (mts2)",
     ylab = "Precio (US$)")
abline(modelo, col = "red")


# Por cada metro cuadrado de aumento aumentará en promedio el precio en 1448 dolares
# # NUll Modelo M_0
# mean(datos$price)
# plot(datos$area, datos$price, pch = 20, xlab = "Área (mts2)",
#      ylab = "Precio (US$)")
# abline(modelo, col = "red")
# abline(h = mean(datos$price), col = "blue")

# Pronostico para 200 mts cuadrados cuan el el valor del precio
predict(modelo, data.frame(area = 200))
modelo$coefficients[1] + modelo$coefficients[2]*200

# mse <- function(theta) {
#   mean((y - theta))^2
# }
# y <- datos$price
# opts = list("algorithm" = "NLOPT_LN_BOBYQA", "xtol_rel" = 1.0e-16, "maxeval" = 10000)
# optimizacion <- nloptr(x0 = c(700), eval_f = mse, eval_grad_f = NULL, opts = opts)
# optimizacion
# mean(datos$price)


# Ejercico: cargar Boston, pronosticar el precio mediano de la vivienda de un bloque:
# usar las variables propuestas, por ahora 1 solamente
# Pronosticar para un valor de su x (la definida por usted)
data("Boston")
summary(Boston)
?Boston

############## Regresión con una varaible categórica ###################
data(iris)
iris$Species

df_dummies <- fastDummies::dummy_cols(iris$Species)
df_dummies$.data <- NULL
names(df_dummies) <- c("X_setosa", "X_versicolor", "X_virginica")
iris <- cbind(iris, df_dummies)
head(iris)
# No apropiado
#modelo <- lm(Petal.Width ~ X_setosa + X_versicolor + X_virginica, data = iris)
#summary(modelo)

# Variable con variable explicativa categirica 
# Omitií setosa
modelo <- lm(Petal.Width ~  + X_versicolor + X_virginica, data = iris)
summary(modelo)

tapply(iris$Petal.Width, iris$Species, FUN = mean)

0.24600 # Promedio de setosa (variable omitida) y de beta0

1.08000 #   beta1 (versicolor)  
1.326  - 0.24600  # Promedio de versicolor - Promedio variable omitida (setosa)

1.78000 # beta 2 (virginica)
2.026 - 0.24600  # Promedio de virginica - Promedio variable omitida (setosa)
boxplot(Petal.Width ~  Species, data = iris)

# Alternativa 2
modelo2 <- lm(Petal.Width ~  X_setosa + X_versicolor + X_virginica - 1, data = iris)
summary(modelo2)
tapply(iris$Petal.Width, iris$Species, FUN = mean)

# En la practica 
modelo <- lm(Petal.Width ~ factor(Species), data = iris)


# Regresión linea multiple
# Y ~ X1 + X2 + ..

corr <- round(cor(Boston),2)
library(ggcorrplot)
ggcorrplot::ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 2,
           method="circle",
           colors = c("orange","white", "green"),
           title="Correlograma de Boston",
           ggtheme=theme_bw)

modelo1a <- lm(medv ~ lstat, data = Boston)
modelo1b <- lm(medv ~  rm, data = Boston)
modelo2 <- lm(medv ~ lstat + rm, data = Boston)

mean((Boston$medv - modelo1b$fitted.values)^2)  # \frac{1}{n}\sum(y_i - hat{y}_i)^2
mean((Boston$medv - modelo1a$fitted.values)^2)  # \frac{1}{n}\sum(y_i - hat{y}_i)^2
mean((Boston$medv - modelo2$fitted.values)^2)  # \frac{1}{n}\sum(y_i - hat{y}_i)^2

mse <- function(y, M){
  mean((y - M$fitted.values)^2)  
}

mse(Boston$medv,modelo2)


