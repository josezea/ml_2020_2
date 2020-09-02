# Repaso de minimización del MSE
set.seed(12345)
P <- runif(n = 1000, 800, 1000)
hist(P)
# Q = alpha * P ^beta
# alpah = 0.3
# beta = 8
set.seed(12345)
Q <- 0.3 * P^8 + rnorm(n = 1000, mean = 0, sd = 1) 
plot(P, Q, pch = 20)


# Para encontrar los parámetro
# theta = c(alpha, beta)
plot(P, Q)

mse <- function(theta){
  mean((Q - theta[1] * P^theta[2])^2)
}

library(nloptr)
opts = list("algorithm" = "NLOPT_LN_BOBYQA", "xtol_rel" = 1.0e-16, "maxeval" = 1000000)
optimizacion <- nloptr(x0 = c(3, 15.2), eval_f = mse, eval_grad_f = NULL, 
                       lb = c(0, 0), ub = c(Inf, Inf), opts = opts)
sol <- optimizacion$solution
sol
plot(P, Q)
points(P, sol[1]*P^sol[2], col = "red", pch =20)

lm(log(Q)~log(P))


############################## Modelos polinómicos ###########
setwd("/home/jose/Documentos/datos/")
library(data.table)
s11 <- fread("s11_2019II.txt", sep = "|")
muestra <- s11[sample(nrow(s11), 1000),]
plot(muestra$PUNT_INGLES, muestra$PUNT_MATEMATICAS, pch = 20)

# Datos faltantes en Matematicas o ingles
summary(s11[,c("PUNT_MATEMATICAS", "PUNT_INGLES")])

s11 <- na.omit((s11[,c("PUNT_MATEMATICAS", "PUNT_INGLES", "DESEMP_INGLES")]))
# modelo nulo
M_0 <- lm(PUNT_MATEMATICAS ~ 1, data = s11)
plot(muestra$PUNT_INGLES, muestra$PUNT_MATEMATICAS, pch = 20)# Modelo lineal
abline(M_0, col = "red")

mse_m0 <- mean((s11$PUNT_MATEMATICAS - M_0$fitted.values) ^2)

# Modelo con una variable categórica (Desmepño ingreso)
M_1 <- lm(PUNT_MATEMATICAS ~ DESEMP_INGLES, data = s11)
summary(M_1)
tapply(s11$PUNT_INGLES, s11$DESEMP_INGLES, mean)
plot(muestra$PUNT_INGLES, muestra$PUNT_MATEMATICAS, pch = 20)# Modelo lineal
abline(h = 38.246002 , col = "red") # A-
abline(h = 38.246002 +  14.188768 , col = "orange") # A1
abline(h = 38.246002 +  23.395206  , col = "yellow") # A1
abline(h = 38.246002 +  33.933810  , col = "green") # A1
abline(h = 38.246002 +  46.058737   , col = "blue") # B+

mse_m1 <- mean((s11$PUNT_MATEMATICAS - M_1$fitted.values) ^2)


# Modelo lineal 
M_2 <- lm(PUNT_MATEMATICAS ~ PUNT_INGLES, data = s11)
summary(M_2)
plot(muestra$PUNT_INGLES, muestra$PUNT_MATEMATICAS, pch = 20)# Modelo lineal
abline(M_2 , col = "red") # A-

mse_m2 <- mean((s11$PUNT_MATEMATICAS - M_2$fitted.values) ^2)
mse_m2 

# Modelo cuadrático
M_3 <- lm(PUNT_MATEMATICAS ~ PUNT_INGLES + I(PUNT_INGLES^2), data = s11)
summary(M_3)
plot(muestra$PUNT_INGLES, muestra$PUNT_MATEMATICAS, pch = 20)# Modelo lineal
points(muestra$PUNT_INGLES, predict(M_2, data.frame(PUNT_INGLES = muestra$PUNT_INGLES)), 
       col = "red")
mse_m3 <- mean((s11$PUNT_MATEMATICAS - M_3$fitted.values) ^2)
mse_m3 

############ Objetivo: pronosticar de la mejor manera el puntaje en matematicas.
# Entrenar con un 70 % de los datos.
# Validar el modelo con el 30% de los datos

setwd("/home/jose/Documentos/datos/")
library(data.table)
s11 <- fread("s11_2019II.txt", sep = "|")
s11 <- s11[!is.na(s11$PUNT_INGLES),]

table(s11$FAMI_TIENEINTERNET, useNA = "always")
s11$FAMI_TIENEINTERNET <- ifelse(s11$FAMI_TIENEINTERNET == "-" | is.na(s11$FAMI_TIENEINTERNET),
                                 "No informa", s11$FAMI_TIENEINTERNET)
table(s11$FAMI_TIENEINTERNET, useNA = "always")


n_train <- round(0.7 * nrow(s11))
n_test <-  round(0.3 * nrow(s11))

set.seed(290820)
indica_train <- sample(nrow(s11), n_train)
set.seed(290820)
indica_test <- sample(nrow(s11), n_test)

muestra_train <- s11[indica_train,]
muestra_test <- s11[indica_test,]

saveRDS(muestra_train, "muestra_train.rds")
saveRDS(muestra_test, "muestra_test.rds")

m1 <- lm(PUNT_MATEMATICAS ~ PUNT_INGLES + PUNT_C_NATURALES, data = muestra_train)
summary(m1)
summary(s11$PUNT_C_NATURALES)

###################### La metrica se calcula sobre la muestra de prueba ####
y_pron <- predict(m1, muestra_test)
mse_test <- mean((muestra_test$PUNT_MATEMATICAS - y_pron)^2)
