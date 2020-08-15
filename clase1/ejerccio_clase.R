n = 7

# datos <- read.delim("clipboard")
# set.seed(12345)
# datos <- datos[sample(26,7),]
# dput(datos)
datos <- data.frame(Nombres = c("XAVIER ALBERTO", "LEYDI LILIANA", 
                                "SERGIO ANDRES", "LEIDY KATHERINE", 
                                "XIOMY JINETH", "ILVAR DARIO", 
                                "NORMA YULIANA"), 
                    Apellidos = c("NOBOA CALDERON", "TOQUICA LOZANO", 
                      "MARTIN RINCON", "BUITRAGO RAMIREZ", "DIAZ MORALES", "SANABRIA GONZALEZ", 
                      "CALA MARTINEZ"))
datos$peso <- c(95, 53, 68, 70, 65, 83, 67)
datos$estatura <- c(179, 163, 174, 169, 170, 179, 160)
plot(datos$estatura, datos$peso, pch = 20)

y <- datos$peso
x <- datos$estatura
y 

mse <- function(beta){
  sum((y - beta[1] - beta[2] * x)^2)  
}

mse(c(30, 0.5))
mse(c(20, 0.5))
mse(c(20, 0.9))
mse(c(20, 0.2)) # va ganando
mse(c(20, 0.1))

# Sin restricción

opts = list("algorithm" = "NLOPT_LN_BOBYQA", "xtol_rel" = 1.0e-16,
            "maxeval" = 10000)
beta_inicial <- c(20, 0.2)
nloptr(x0 = beta_inicial, eval_f = mse, 
       eval_grad_f = NULL, opts = opts)
mse(c( -178.0763,  1.463596))

nloptr(x0 = c(1000, 10000), eval_f = mse, 
       eval_grad_f = NULL, opts = opts)
plot(datos$estatura, datos$peso, pch = 20, ylim = c(-200, 90),xlim = c(0, 200))
abline(lm(peso ~ estatura, data = datos))



mae <- function(beta){
  sum(abs(y - beta[1] - beta[2] * x))  
}


nloptr(x0 = c(-200, 0.4), eval_f = mae, 
       eval_grad_f = NULL, opts = opts)
plot(datos$estatura, datos$peso, pch = 20,)
abline(a = 3.26, 0.39)


# Usemos el modelo
# Supongamos que entrenamos con la regresión cuantílica (usa el mae)

#datos_pron <- read.delim("clipboard")
#dput(datos_pron)
datos_prono <- data.frame(Nombres = c("LEONARDO ESNEIDER", "JAIRO IVAN", 
                           "ANGELA CRISTINA", "JHON FREDDY", "JORGE EDUARDO", "CAMILO ANDRES", 
                           "CINDY CAROLINA", "DAVID ALEJANDRO", "JONATHAN ORLANDO"), 
Apellidos = c("RUBIO SALCEDO",  "ORDOÑEZ ERAZO", "VILLATE MORENO", "PUENTES NUÑEZ", "GOMEZ FORERO", 
                                 "ROJAS REYES", "NARANJO QUIROGA", "CASTRO MORENO", "FUYA TARAZONA"),
Estatura = c(173L, 185L, 151L, 185L, 175L, 175L, 155L, 175L,    168L))

ygorro <- 3.264321 +  0.3948857 * datos_prono$Estatura
ygorro

