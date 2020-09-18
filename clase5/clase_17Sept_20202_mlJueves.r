library(nloptr)

# Muestra de tamaño n = 8
set.seed(12092020)
sample(27, 8)
# c(7L, 5L, 19L, 4L, 13L, 25L, 24L, 23L)

y <- c(1,1, 1, 0, 0, 0, 1, 1 )

suma_y <- sum(y)
n <- length(y)

logLP <- function(p) {
  log(p) * suma_y + log(1-p) * (n-suma_y)   
}  


LP <- function(p) {
  exp(log(p) * suma_y + log(1-p) * (n-suma_y))   
}  

plot(logLP, xlab = "p", ylab = "log L(p)")  
abline(v = 5/8, col = "red")

exp(logLP(0.1))
exp(logLP(0.5))
exp(logLP(5/8))

plot(LP, xlab = "p", ylab = "L(p)")  
abline(v = 5/8, col = "red")

LP(0.625)
LP(0.5)

# Metodos numéricos (minimizar el - logL(p))


MenoslogLP <- function(p) {
  -(log(p) * suma_y + log(1-p) * (n-suma_y))   
}  

opts = list("algorithm" = "NLOPT_LN_BOBYQA", "xtol_rel" = 1.0e-16,
            "maxeval" = 10000)

solucion <- nloptr(x0 = 0.9, eval_f = MenoslogLP, lb = 1.0e-16, ub = 1 - 1.0e-16,
                   eval_grad_f = NULL, opts = opts)
solucion




MenosLP <- function(p) {
  -(exp((log(p) * suma_y + log(1-p) * (n-suma_y))))   
}  

opts = list("algorithm" = "NLOPT_LN_BOBYQA", "xtol_rel" = 1.0e-16,
            "maxeval" = 10000)

solucion <- nloptr(x0 = 0.9, eval_f = MenosLP, lb = 1.0e-16, ub = 1 - 1.0e-16,
                   eval_grad_f = NULL, opts = opts)
solucion



############################## regresión logística ####################
#c(7L, 5L, 19L, 4L, 13L, 25L, 24L, 23L)

y_i <- c(1,1, 1, 0, 0, 0, 1, 1 ) # tomo el ultimo año
x_i <- c(39, 30, 31, 26, 32, 31, 29, 50) # edad 
x_i <- (x_i - mean(x_i)) / sd(x_i)

# invlogit <- function(x){
#   exp(x) / (1+exp(x))
# }

invlogit <- function(x){
  1 / (1 + exp(-x))  
}

invlogit(0.8)

plot(invlogit, xlim = c(-10, 10))

MenoslogL_p <- function(betas){
  p_i <- invlogit(betas[1] + betas[2]*x_i)  
  res <- sum(y_i * log(p_i)) + sum((1 - y_i) * log(1 - p_i))   
  -res    # El algoritmo minimiza, entonces que minimice el -log L(p)
}

MenoslogL_p(c(0,1))

opts = list("algorithm" = "NLOPT_LN_BOBYQA", "xtol_rel" = 1.0e-16,
            "maxeval" = 10000)

# xo_ el valor inicial para beta_0 y beta_1
sol <- nloptr(x0 = c(0, 1), eval_f = MenoslogL_p,
              eval_grad_f = NULL, opts = opts)
sol

modelo <- glm(factor(y_i) ~ x_i, family = "binomial")
summary(modelo)
c(0.9872009, 1.9385) # Es el valor que maximiza la función de máxima verosimilitud
MenoslogL_p(c(0.9872009, 1.9385)) 
MenoslogL_p(c(1.6, 2.9385))


#Si es multivariado x_0: x_0 <- c(varlInic_bo, valinicial_b1, ...vali_inicial_bp)


# Ejercicio con iris
data(iris)
boxplot(Sepal.Length ~ Species,data = iris)

iris$yi <-  ifelse(iris$Species == "setosa", 1, 0)
iris$xi <- iris$Sepal.Length
iris$EspecieRecod <- ifelse(iris$yi == 1, "Setosa", "Otras")
tapply(iris$Sepal.Length, iris$EspecieRecod, FUN = mean)

modelo <- glm(factor(yi) ~ xi, data = iris, family = "binomial")
summary(modelo)

# Calcular para los 150 espacies, ¿Cuás es la probabilidad de ser de la especie setosa?
iris$prob <-  invlogit(27.8285 -5.1757 * iris$xi) 
#iris$prob <-  exp(27.8285 -5.1757 * iris$xi) / (1 + exp(27.8285 -5.1757 * iris$xi))
iris$prob2 <- modelo$fitted.values
iris$prob3 <- predict(modelo, iris, type = "response")
plot(iris$Sepal.Length,iris$prob2)

# Si una flor tiene una longitud del sepalo de 4.5
invlogit(27.8285 -5.1757 * 4.5) 

# Si una flor tiene una longitud del sepalo de 6
invlogit(27.8285 -5.1757 * 6) 

exp( -5.1757 * 0.1)


# Van a seleccionar el training y test
set.seed(12092020)
indica_mue <- sample(150, round(0.7 * 150))

data(iris)
iris$yi <-  ifelse(iris$Species == "setosa", 1, 0)

training <- iris[indica_mue,]
test <- iris[-indica_mue,]

mod_logSepal <- glm(yi~Sepal.Length, data = training, family = "binomial")
summary(mod_logSepal)
# Valido en la muestra test

invlogit <- function(x){
  1 / (1 + exp(-x))  
}

test$probs <- invlogit(mod_logSepal$coefficients[1] + mod_logSepal$coefficients[2] * test$Sepal.Length) 

# Punto corte SI las probs >0.5 voy a clasificar a la flor en que es de setosa
test$yhat <- ifelse(test$probs >= 0.5, 1, 0)
table(test$yi, test$yhat)
accuracy_test <- (30 + 12) / (30 + 3 + 0 + 12)

mc_test <- table(test$yi, test$yhat)

accuracy_test <- sum(diag(mc_test)) / sum(mc_test)
accuracy_test

# Ejercicio hacer cuatro modelos logisticos, en donde calculen el acrruacy en la muestra test para cad
# regresión logistica simple (solo metar una única variable continua en cuada uno de lso 4 modelos)
# Calcular la métrica accuracy para lso cuatro modelos y escoger el mejor

library(ggplot2)
load("C:/Users/Home/Downloads/logistica (1) (1).RData")

# Recodificar la variable de interés
# 1 si el cliente se va, 0 si el cliente permanece
# Churn analysis


# EDA
ggplot(data = insumo, aes(x = calidad_produc, y = factor(target))) +
  geom_boxplot()

ggplot(data = insumo, aes(x = calif_voz, y = factor(target))) +
  geom_boxplot()

ggplot(data = insumo, aes(x = senal_voz, y = factor(target))) +
  geom_boxplot()
# Esta no parecería que sirve
ggplot(data = insumo, aes(x = recharges_month_a, y = factor(target))) +
  geom_boxplot()

ggplot(data = insumo, aes(x = nr_recharges_month_a, y = factor(target))) +
  geom_boxplot()

set.seed(17092020)
indica_mue <- sample(nrow(insumo), round(0.7 * nrow(insumo)))

training <- insumo[indica_mue,] 
test <- insumo[-indica_mue,]

modelo <- glm(target ~ 1 + calidad_produc + nr_recharges_month_a,
              data = training, family = "binomial")
summary(modelo)

# hay que evaluar en la muestra TEST

# Primero calculemos las probabilidades
test$probs <- predict(modelo, 
                      test[c("calidad_produc", "nr_recharges_month_a")], 
                      type = "response")

test$yhat <- as.numeric(test$probs >= 0.5) 
matrix_conf <- table(test$target, test$yhat)

100 * sum(diag(matrix_conf)) / sum(matrix_conf)

# Sensibilidad
matrix_conf 
matrix_conf[2,2] / sum(matrix_conf[2,]) 
# 67 % de sensibilidad

# Especificicdad
matrix_conf[1,1] / sum(matrix_conf[1,]) 
# 92.68 de especificidad

# Curva ROCO y el ROC