ls()
head(ing_gastos)
head(a)

head(b)
a <- a[c("directorio", "estrato")]
c$c16 <- c[c("directorio_hog", "directorio", "c16", "c18")] # cuartos, # num personas
d <- d[c("directorio_hog", "directorio", "d27")] # d27 internet

datos <- inner_join(a, c)
datos <- inner_join(datos, d)
datos <- inner_join(datos, ing_gastos)

# Hacer el modelo con estrato, ingreso, cuartos por persona 
# Y Gasto

library(rsample)
set.seed(12345)
indica_estrato <- rsample::initial_split(data = datos, prop = 0.7, strata = estrato)
train <- training(indica_estrato)
test <- assessment(indica_estrato)

modelo <- rpart(gasto ~ ingreso + d27 + c18 + factor(estrato),
                data = train, method = "anova")
summary(modelo)

modeloReg <- lm(gasto ~ ingreso + d27 + c18 + factor(estrato),
                data = train)


# X11()
prp(modelo)

pronostico <- predict(modelo, newdata = test)
pronosticoReg <- predict(modeloReg, test)

sqrt(mean((test$gasto - pronostico)^2, na.rm = T))
sqrt(mean((test$gasto - pronosticoReg)^2, na.rm = T))

