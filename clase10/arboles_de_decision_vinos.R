library(plotly)
library(knitr)
library(rpart)
library(rpart.plot) 

#Información sobre vinos de portugal

url  <-"http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
wine <- read.table(file=url, header = T, sep=";")

#para mostrar los resultados como cuadro en el documento a imprimir en rmarkdown
kable(head(wine),caption = "Resumen de datos") 

#mirar la estructura de los datos

str(wine)

#Graficar 
plot_ly(wine,x = ~quality, type= "histogram")

# el objetivo es predecir la calidad del vino dadas las características del mismo (acidez, azucar, sulfuro...)

# Explorar la variable quality en terminos del alcohol

wine2 <- wine
wine2$qualitychar <- ifelse( wine2$quality == 3, "a_tres"
                             , ifelse(wine2$quality == 4, "b_Cuatro"
                                      , ifelse(wine2$quality == 5, "c_cinco"
                                               , ifelse(wine2$quality == 6, "d_Seis"
                                                        , ifelse(wine2$quality == 7, "e_Siete"
                                                                 , ifelse(wine2$quality == 8, "f_Ocho"
                                                                          , "g_Nueve"))) )))

plot_ly(data = wine2, x = ~qualitychar, y = ~alcohol
        , color = ~qualitychar
        , type = "box"
        , colors = "Dark2"
)
# hay mejor calidad del vino entre 11 y 13 grados de alcohol

#Explorar en terminos de la densidad
plot_ly(data = wine2, x = ~qualitychar, y = ~density
        , color = ~qualitychar, type = "box", colors = "Set1")

# Entre menos denso mejor la calidad.

#Para el pronóstico vamos a tomar aleatoriamente el 75% de los datos para entrenar y el 25% de los datos para el conjunto test
# training set
set.seed(pi)
itrain     <- sample( 1:4898, size=3750, replace = FALSE)
wine_train <- wine[itrain, ]
nrow(wine_train)
wine_test  <- wine[-itrain, ]

#Entrenamiento del modelo

# Se realiza con el paquete de partición recursiva rpart de la libreria rpárt
m.rpart <- rpart(quality ~. , data = wine_train)
m.rpart 

#visualización del árbol con la libreria rpart.plot
rpart.plot(m.rpart)

# evaluar el modelo
#comparamos el conjunto de datos de predicción y el conjunto de prueba y podemmos observar que los valores son muy similares
p.rpart <- predict( m.rpart, wine_test )
summary(p.rpart)

summary( wine_test$quality )

# Error medio absoluto
MAE <- function(actual, predicted){
  mean(abs (actual - predicted))
}

MAE(wine_test$quality, p.rpart)
# un MAE DEL 59% es aceptable 

# Pronosticar la calidad del vino con ciertas caracteŕisticas dadas

test <- data.frame(fixed.acidity = 8.5, volatile.acidity = 0.33
                   , citric.acid = 0.42, residual.sugar = 10.5
                   , chlorides = 0.065, free.sulfur.dioxide = 47
                   , total.sulfur.dioxide = 186, density = 0.9955
                   , pH = 3.10, sulphates = 0.40, alcohol = 9.9)

test_pred <- predict(m.rpart, test)

#Dadas esas caractrísticas el vino tendrá una calidad de 5.36


test_pred
