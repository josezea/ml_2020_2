library(ggplot2)
library(fastDummies)
library(TeachingSampling)
library(ggplot2)
library(rsample)

data(iris)
plot(iris$Sepal.Length, iris$Petal.Length, pch = 20)
abline(lm(Petal.Length ~ Sepal.Length, data = iris), col = "red")

summary(lm(Petal.Length ~ Sepal.Length, data = iris))

ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length, colour = Species)) + xlim(0,10)+
  geom_point() +  geom_smooth(method = "lm")

df_dummies <- fastDummies::dummy_cols(iris$Species)[,3:4]
names(df_dummies) <- gsub(".data_", "", names(df_dummies))
iris <- cbind(iris, df_dummies)
iris$X_versic  <- iris$versicolor * iris$Sepal.Length
iris$X_virgin  <- iris$virginica * iris$Sepal.Length
modelo <- lm(Petal.Length ~ Sepal.Length + versicolor + virginica + X_versic + X_virgin, data = iris)
summary(modelo)
# O así más fácil:
modeloB <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
summary(modeloB)
modeloC <- lm(Petal.Length ~ Sepal.Length + factor(Species) + Sepal.Length:Species , data = iris)
summary(modeloC)

ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length, colour = Species)) + xlim(0,10)+
  geom_point() +  geom_smooth(method = "lm")

# Puede poner a interactuar variables continuas

modelo3 <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
summary(modelo3)
sqrt(mean((iris$Petal.Length - modelo3$fitted.values)^2))

modelo4 <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)
summary(modelo4)
sqrt(mean((iris$Petal.Length - modelo4$fitted.values)^2))
boxplot(Petal.Length~Species, data = iris)


# Selección de una muestra
data(Lucy)
set.seed(5112020)
indica_mue <- sample(nrow(Lucy), round(0.7*nrow(Lucy)))
train <- Lucy[indica_mue,]
test <- Lucy[-indica_mue,]
nrow(train)
nrow(test)

train$tipo <- "train"
test$tipo <- "test"
df <- bind_rows(train, test)
ggplot(data = df, aes(x = Income, colour = tipo)) + geom_density()

ggplot(data = df, aes(x = Employees, colour = tipo)) + geom_density()
tapply(df$Income, df$tipo, mean)

ggplot(data = df, aes(x = tipo, fill = Level)) + geom_bar()

# Esquema de selección estratificada
# Income

# Estrato Level
table(train$Level)

boxplot(Income~ Level, data = Lucy)


set.seed(5112020)
split_strat  <- initial_split(Lucy, prop = 0.7, 
                              strata = "Level")
train_strat  <- training(split_strat)
test_strat   <- testing(split_strat)
train_strat$tipo <- "train"
test_strat$tipo <- "test"

df2 <- bind_rows(train_strat, test_strat)
ggplot(data = df2, aes(x = Income, colour = tipo)) + geom_density()

table(train_strat$Level)


# Validación cruzada : kfold, leave one out, bootstrapp
# Como podemos usar estas técnicas

# En la practica en la concepción más simple
modelo <- lm(Income ~ Taxes , data = train_strat)
yhat_test <- predict(modelo, test_strat)

rmse_test <- sqrt(mean((test_strat$Income  - yhat_test)^2))


# En la practica en la concepción más simple
plot(train_strat$Income ~ train_strat$Taxes )
modelo2 <- lm(Income ~ Taxes + I(Taxes ^2) + I(Taxes ^3) + I(Taxes ^4) , data = train_strat)
yhat_test <- predict(modelo2, test_strat)
#1.701e+02  + 2.979e+01  * test_strat$Taxes + -3.129e-01 * test_strat$Taxes^2+--- 

plot(test_strat$Income ~ test_strat$Taxes, pch = 20 )
points(test_strat$Taxes, yhat_test, col = "red", pch = "*")
# Metrica
rmse_test2 <- sqrt(mean((test_strat$Income  - yhat_test)^2))
summary(modelo2)

modelo3 <- lm(Income ~ Taxes + I(Taxes ^2) + Employees , data = train_strat)
yhat_test <- predict(modelo3, test_strat)

rmse_test3 <- sqrt(mean((test_strat$Income  - yhat_test)^2))

# Hasta el momento el cusdrático solo con taxes


modelo4 <- lm(Income ~ Taxes + I(Taxes ^2) + Taxes:Employees , data = train_strat)
yhat_test <- predict(modelo4, test_strat)

rmse_test4 <- sqrt(mean((test_strat$Income  - yhat_test)^2))
# No jhay interacción entre Taxes y EAmployees


modelo5 <- lm(Income ~ Taxes + I(Taxes ^2) + factor(Zone) , data = train_strat)
yhat_test <- predict(modelo5, test_strat)

rmse_test5 <- sqrt(mean((test_strat$Income  - yhat_test)^2))
# No jhay interacción entre Taxes y EAmployees
