library(h2o)
library(xgboost)
library(gbm)
library(AER)
library(rsample)
library(Metrics)
data("CollegeDistance")
set.seed(123)
college_split <- initial_split(CollegeDistance, prop = .8)
college_train <- training(college_split)
college_test  <- testing(college_split)

set.seed(123)

college_gbm1 <- gbm(
  formula = score ~ ., # score se modela mediante el resto de variables
  data = college_train,
  distribution = "gaussian",  # Función de pérdida cuadrática
  n.trees = 1000, # Número de árboles
  shrinkage = 0.1, # Learning rate
  interaction.depth = 3, # Profundidad de los árboles
  n.minobsinnode = 10, # Número mínimo de obs. x nodo
  cv.folds = 10
)

print(ames_gbm1) ### características del modelo

# Número de árboles con menor error de validación cruzada

best <- which.min(college_gbm1$cv.error)

# Calcular MSE

sqrt(college_gbm1$cv.error[best])

### Número óptimo de árboles 

gbm.perf(college_gbm1, method = "cv")
perf_gbm1 = gbm.perf(college_gbm1, method = "cv")

#### Ver qué variables influyen más

summary(college_gbm1)

#### Realizar predicciones con los datos de validación

college_prediccion_1 <- stats::predict(
  # El modelo
  object = college_gbm1, 
  # Datos de validación
  newdata = college_test,
  # Número de árboles (calculado con la muestra de entrenamiento)
  n.trees = perf_gbm1)
rmse_fit1 <- Metrics::rmse(actual = college_test$score, 
                           predicted = college_prediccion_1)
print(rmse_fit1)

### ver relaciones entre variables y variable respuesta, var=2->columna #2

gbm::plot.gbm(college_gbm1, i.var = 2)

### también se pueden ver interacciones entre variables independientes

gbm::plot.gbm(college_gbm1, i.var = c(5, 11))


