
data(iris)

iris$y <- ifelse(iris$Species == "setosa", "setosa", "otras")
table(iris$y)
library(ggplot2)
ggplot(data = iris, aes(x = y, y = Sepal.Length)) + geom_boxplot()

iris$y <- ifelse(iris$Species == "setosa", 1, 0)
modelo <- glm(y ~ Sepal.Length, data = iris, family = "binomial")
summary(modelo)
coefficients(modelo)


exp(-5.175698 )
1 / exp(-5.175698 )


iris$y_otros <- ifelse(iris$Species == "setosa", 0, 1)
modelo <- glm(y_otros ~ Sepal.Length, data = iris, family = "binomial")
summary(modelo)
coefficients(modelo)

exp( 5.175698  )

# Por cada cm de aumento en la longitud del sepalo aumenta en 177 veces más
# el chance de ser de la especie otros (versicolor o virginica) con respecto
# a setosa

################# Interpretación de los odss
invodds <- function(x){
  1 / (1 + x^-1)
}
invodds(1.1)
# odds de 1.1
#p = 0.52, 1-p = 0.48

plot(invodds, xlim = c(0,4), xlab = "odds", ylab = "Prob de exito")



iris$y <- ifelse(iris$Species == "setosa", 1, 0)
modelo <- glm(y ~ Sepal.Length, data = iris, family = "binomial")
summary(modelo)
coefficients(modelo)

# Probabilidades 
iris$p_i <- modelo$fitted.values
iris$p_i_2 <- predict(modelo, data.frame(Sepal.Length = iris$Sepal.Length), type = "response")
iris$p_i_3 <- exp( 27.828521  + -5.175698 * iris$Sepal.Length) / (1 + exp(  27.828521 + -5.175698 *
                                                                              iris$Sepal.Length))
plot(iris$Sepal.Length, iris$p_i, xlab = "x", ylab = "p_i")
abline(h = 0.5, col = "red")


# Probabilidades 

iris$y <- ifelse(iris$Species == "setosa", 0, 1)
modelo <- glm(y ~ Sepal.Length, data = iris, family = "binomial")
coefficients(modelo)
iris$p_i <- modelo$fitted.values
iris$p_i_2 <- predict(modelo, data.frame(Sepal.Length = iris$Sepal.Length), type = "response")
iris$p_i_3 <- exp( -27.828521  + 5.175698 * iris$Sepal.Length) /
  (1 + exp( - 27.828521 + 5.175698 * iris$Sepal.Length))

View(iris)
plot(iris$Sepal.Length, iris$p_i, xlab = "x", ylab = "p_i")
abline(h = 0.5, col = "red")


# Donde clasificar 
# Si 
data(iris)
iris$y <- ifelse(iris$Species == "setosa", 1, 0)
modelo <- glm(y ~ Sepal.Length, data = iris, family = "binomial")
summary(modelo)
coefficients(modelo)
iris$p_i <- modelo$fitted.values
iris$yhat <- ifelse(iris$p_i  >= 0.5, 1, 0)
# Esto no es valido puesto que se debe en la muestra de test
table(iris$y, iris$yhat)


# Pronostico
data(iris)
iris$y <- ifelse(iris$Species == "setosa", 1, 0)
set.seed(19092020)
indica_train <- sample(nrow(iris), round(0.7 * nrow(iris)))
train <- iris[indica_train,]
test <- iris[-indica_train,]
M1 <- glm(y ~ Sepal.Length, data = train, family = "binomial")
summary(M1)

test$probs <- predict(M1, test, type = "response")

# Clasificar en setosa si probs >= 0,5
test$yhat <- ifelse(test$probs >= 0.5, 1, 0)

table(test$y, test$yhat)
mc <- table(test$y, test$yhat)
sum(diag(mc)) / sum(mc) # Accuracy

# Sensibilidad y la especificidad

# Sensibilidad
mc[2,2] / sum(mc[2,])
# Error tipo 1
mc[2,1] / sum(mc[2,])


# Especificidad
mc[1,1] / sum(mc[1,])
# Error tipo 2
mc[1,2] / sum(mc[1,])

# Estudio de caso
ggplot(data = insumo, aes(y = calidad_produc, x = factor(target ))) + 
  geom_boxplot()

# Estudio de caso
ggplot(data = insumo, aes(x = calidad_produc, color = factor(target) )) + 
  geom_density()


# Estudio de caso
ggplot(data = insumo, aes(fill = antigued, x = factor(target ))) + 
  geom_bar()
table(insumo$antigued)
# no es útil, todos lso clientes son antugos de más de dos años

# La región es un factor muy importante
ggplot(data = insumo, aes(x = region, fill = factor(target ))) + 
  geom_bar()
prop.table(table(insumo$region, insumo$target), 1) * 100
barplot(t(prop.table(table(insumo$region, insumo$target), 1) * 100))

ggplot(data = insumo, aes(x = uso_serv_cliente, fill = factor(target ))) + 
  geom_bar()

set.seed(17092020)
train_cel <- insumo[sample(280, 196),]
set.seed(17092020)
test_cel <- insumo[-sample(280, 196),]

M1 <- glm(target ~  calidad_produc + cant_cargas_m1, data = train_cel,
          family = "binomial")

test_cel$probs_churn <- predict(M1, test_cel)

mc_churn <- table(test_cel$target, 
                  as.numeric(test_cel$probs_churn >= 0.5))
mc_churn 
sum(diag(mc_churn)) / sum(mc_churn) *100


