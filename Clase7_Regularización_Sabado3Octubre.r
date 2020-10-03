library(nloptr)
library(glmnet)
library(ISLR) # Introduction to statistical Learning in R
library(fastDummies)
data(Hitters)

#dim(Hitters)

var_continuas <- which(sapply(Hitters, is.numeric))
datos <- Hitters[,var_continuas]
summary(datos)
datos <- na.omit(datos)

X <- model.matrix(Salary ~ ., data = datos)[,-1]
beta <- as.matrix(1:16) * 2
as.numeric(X %*%  beta)
dim(X)
dim(beta)
(X %*%  beta)[1,]
sum(beta * X[1,])
# Todos los betas son iguales para todos los individuos
# b1 X11 + b2 * X12 + ..b16 * X1,16
# b1 X322,1 + b2 * X322,2 + ..b17 * X322,17
sum(as.numeric(beta) * as.numeric(X[1,]))

# y = beta_0 + beta_1 X1 + ...beta_20 X20
X <- model.matrix(Salary ~ ., data = datos)[,-1]
y <- datos$Salary
lambda <- 0
# beta es un vector de 17 entradas (incluyendo interc)
# beta <- 2*(1:17)
# beta[1] <- 1

mse <- function(beta){
    beta <- as.matrix(beta)
      0.5 * mean( (y - beta[1,1] - as.numeric(X %*%  as.matrix(beta[-1,])))^2 ) +
         lambda * sum(abs(as.numeric(beta[-1,1]))) 
}

#y - beta[1] - beta[2] * datos[,1] + beta[3] * datos[,2] + ..
opts = list("algorithm" = "NLOPT_LN_BOBYQA", "xtol_rel" = 1.0e-16,
           "maxeval" = 100000)

beta_inicial <-  rep(1, 17)
optimiza <- nloptr(x0 = beta_inicial, eval_f = mse,  eval_grad_f = NULL, 
                   opts = opts)
optimiza$solution
optimiza$objective
coef(lm(Salary ~ ., data = datos))


# Modelo 2
# AHora corramos con lambda de un 10000
lambda <- 500

mse <- function(beta){
  beta <- as.matrix(beta)
  0.5 * mean( (y - beta[1,1] - as.numeric(X %*%  as.matrix(beta[-1,])))^2 ) +
    lambda * sum(abs(as.numeric(beta[-1,1]))) 
}
optimiza <- nloptr(x0 = beta_inicial, eval_f = mse,  eval_grad_f = NULL, 
                   opts = opts)
optimiza$solution
#optimiza$objective
beta_lasso1 <- optimiza$solution
names(beta_lasso1) <- c("Interceto", colnames(datos)[-17])
beta_lasso1[beta_lasso1 > 0.0001]

betas <- matrix(optimiza$solution)
X <- model.matrix(Salary ~ ., data = datos)
yhat2 <- as.numeric(X %*% betas) # pronostico
plot(datos$Salary, yhat2, pch = 20, main = "modelo2")
abline(a = 0, b = 1, col = "red")
R2_modelo2 <- var(yhat2) / var(datos$Salary)
betas[betas > 0.0001]

