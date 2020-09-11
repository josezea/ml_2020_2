library(rsample)
library(nloptr) # Paquete para hacer optimziación no lineal
# Como diagnosticar si una variable categorica explica
# bien a la y (continua)
# ggplot(Lucy, aes(x = Income, col = Level)) + geom_density()
# 129744890  / (129744890  + 40965657)

library(TeachingSampling)
library(ggplot2)
data(Lucy)
ggplot(Lucy, aes(x = Income)) + geom_histogram()


boxplot(Income~ Level, data = Lucy)


set.seed(5112020)
split_strat  <- initial_split(Lucy, prop = 0.7, 
                              strata = "Level")
train_strat  <- training(split_strat)
test_strat   <- testing(split_strat)

# Verificar emplicamiento el balanceamiento de las variables observadas
train_strat$tipo <- "train"
test_strat$tipo <- "test"

df2 <- bind_rows(train_strat, test_strat)
ggplot(data = df2, aes(x = Income, colour = tipo)) + geom_density()
ggplot(data = df2, aes(x = Employees, colour = tipo)) + geom_density()

ggplot(data = df2, aes(x = tipo, fill = Zone)) + geom_bar()
table(df2$tipo, df2$Zone) %>% prop.table(margin = 1)


# Regresión logística
set.seed(10092020)
indica_mue <- sample(16,8)
# 4, 8, 9, 11, 12, 13, 14, 15
tomas <- c(0, 0, 0, 0, 1, 1, 0, 1 )
y <- tomas
suma_y <- sum(y)
n <- length(y)

logL <- function(p){
 suma_y * log(p) + (n - suma_y) * log(1-p) 
}

plot(logL, xlab = "p", ylab = "logL(p)")

prop.table(table(y))
logL(0.375)

# Hacer la optimziación por metodos numéricos}


menoslogL <- function(p){
  -(suma_y * log(p) + (n - suma_y) * log(1-p) )
}

opts = list("algorithm" = "NLOPT_LN_BOBYQA", "xtol_rel" = 1.0e-16,
            "maxeval" = 10000)
solucion <- nloptr(x0 = 0.003, eval_f = menoslogL, lb = 1.0e-16, ub = 1 - 1.0e-16,
       eval_grad_f = NULL, opts = opts)
solucion


# Ejercicio practico
data(iris)
boxplot(Sepal.Length ~ Species, data = iris)

datos <- iris
datos$y <- ifelse(datos$Species == "setosa", 1, 0)


logit <- function(x){
  exp(x) / (1 + exp(x))
}

# Revisar!!!
x <-  datos$Sepal.Length
n <- nrow(datos)
y <- datos$y

menoslogL_logistica <- function( beta){
  p_i <- logit(beta[1] + beta[2] * x )
res <- sum(y*log(p_i)) + sum((1-y) * log(1-p_i))
-res  
}
# En clase no funcion por colocar un valor inicial inadecuado
# Cayo en un optimo local
opts = list("algorithm" = "NLOPT_LN_BOBYQA", "xtol_rel" = 1.0e-16,
            "maxeval" = 100000)
solucion <- nloptr(x0 = c(2, -1), 
                   eval_f = menoslogL_logistica,
                   eval_grad_f = NULL, opts = opts)
solucion

modelo <- glm(factor(datos$y) ~ datos$Sepal.Length, family = "binomial")


