library(h2o)
library(tidyverse)
library(ggpubr)


ruta <- "C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/Semestre2/MachineLearning/Clase15/"


setwd(ruta)
datos <- read_csv("adult_custom.csv")

h2o.init(
  ip = "localhost",
  # -1 indica que se empleen todos los cores disponibles.
  nthreads = -1,
  # Máxima memoria disponible para el cluster.
  max_mem_size = "6g"
)



datos_h2o <- h2o.importFile(
  path   = paste0(ruta, "adult_custom.csv"),
  header = TRUE,
  sep    = ",",
  destination_frame = "datos_h2o"
)

# Si ya se habían leido por otro medio la función as.h2o puede ayudar


h2o.dim(datos_h2o)


# Columnas que son numéricas
# Índices
indices <- h2o.columns_by_type(object = datos_h2o, coltype = "numeric")
indices
h2o.colnames(x = datos_h2o)[indices]


h2o.describe(datos_h2o)






# Se eliminan los datos del cluster por si ya había sido iniciado.
h2o.removeAll()
h2o.colnames(datos_h2o)


indices <- h2o.columns_by_type(object = datos_h2o, coltype = "numeric")
h2o.cor(x = datos_h2o[, indices], y = NULL, method = "Pearson", na.rm = TRUE)

# Se crea una tabla con el número de observaciones de cada tipo.
h2o.table(datos_h2o$salario)
tabla_muestra <- as.data.frame(h2o.table(datos_h2o$salario))
tabla_muestra


# Una vez creada la tabla, se carga en el entorno de R para poder graficar.
ggplot(
  data = tabla_muestra,
  aes(x = salario, y = Count, fill = salario)) +
  geom_col() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  theme_bw() +
  labs(
    x = "Salario", y = "Número de observaciones",
    title = "Distribución de la variable Salario") +
  theme(legend.position = "none")



# Separaciones

# Separación de las observaciones en conjunto de entrenamiento y test.
# En los ejemplos de GBM y deep learning se repetirá la separación, pero en 
# tres conjuntos en lugar de dos.
particiones     <- h2o.splitFrame(data = datos_h2o, ratios = c(0.8), seed = 123)
datos_train_h2o <- h2o.assign(data = particiones[[1]], key = "datos_train_H2O")
datos_test_h2o  <- h2o.assign(data = particiones[[2]], key = "datos_test_H2O")


# Separación de las observaciones en conjunto de entrenamiento y test.
# En los ejemplos de GBM y deep learning se repetirá la separación, pero en 
# tres conjuntos en lugar de dos.
particiones     <- h2o.splitFrame(data = datos_h2o, ratios = c(0.8), seed = 123)
datos_train_h2o <- h2o.assign(data = particiones[[1]], key = "datos_train_H2O")
datos_test_h2o  <- h2o.assign(data = particiones[[2]], key = "datos_test_H2O")


h2o.table(datos_train_h2o$salario)


h2o.table(datos_test_h2o$salario)


# En porcentaje
h2o.table(datos_train_h2o$salario)/h2o.nrow(datos_train_h2o)


h2o.table(datos_test_h2o$salario)/h2o.nrow(datos_test_h2o)



# Se comprueba que la variable respuesta es de tipo factor.
datos_train_h2o$salario <- h2o.asfactor(datos_train_h2o$salario)
datos_test_h2o$salario  <- h2o.asfactor(datos_test_h2o$salario)
h2o.isfactor(datos_train_h2o$salario)


# Se comprueba que la variable respuesta es de tipo factor.
# Se define la variable respuesta y los predictores.
var_respuesta <- "salario"
# Para este modelo se emplean todos los predictores disponibles.
predictores   <- setdiff(h2o.colnames(datos_h2o), var_respuesta)



modelo_glm <- h2o.glm(
  y = var_respuesta,
  x = predictores,
  training_frame = datos_train_h2o,
  family = "binomial",
  link   = "logit",
  standardize  = TRUE,
  balance_classes   = FALSE,
  ignore_const_cols = TRUE,
  # Se especifica que hacer con observaciones incompletas
  missing_values_handling = "Skip",
  # Se hace una búsqueda del hiperparámetro lamba
  lambda_search = TRUE,
  # Selección automática del solver adecuado
  solver = "AUTO",
  alpha  = 0.95,
  # Validación cruzada de 5 folds para estimar el error
  # del modelo.
  seed = 123,
  nfolds = 5,
  # Reparto estratificado de las observaciones en la creación
  # de las particiones.
  fold_assignment = "Stratified",
  keep_cross_validation_predictions = FALSE,
  model_id = "modelo_glm"
)



as.data.frame(modelo_glm@model$coefficients_table) %>% head()

# Predictores incluidos.
names(modelo_glm@model$coefficients[modelo_glm@model$coefficients != 0])



# Importancias de las variables en un modelo líneal generalizado

coeficientes <- as.data.frame(modelo_glm@model$coefficients_table)

# Se excluye el intercept.
coeficientes <- coeficientes %>% filter(names != "Intercept")


# Se calcula el valor absoluto.
coeficientes <- coeficientes %>%
  mutate(abs_stand_coef = abs(standardized_coefficients))

# Se añade una variable con el signo del coeficiente.
coeficientes <- coeficientes %>%
  mutate(signo = if_else(standardized_coefficients > 0,
                         "Positivo",
                         "Negativo"))

ggplot(data = coeficientes,
       aes(x = reorder(names, abs_stand_coef),
           y = abs_stand_coef,
           fill = signo)) +
  geom_col() +
  coord_flip() +
  labs(title = "Importancia de los predictores en el modelo GLM",
       x = "Predictor",
       y = "Valor absoluto coeficiente estandarizado") +
  theme_bw() +
  theme(legend.position = "bottom")



# Metricas en el conjunto train

h2o.auc(modelo_glm, train = TRUE)

# Mean Squared Error
h2o.mse(modelo_glm, train = TRUE)
# R2
h2o.r2(modelo_glm, train = TRUE)
# LogLoss
h2o.logloss(modelo_glm, train = TRUE)
# Coeficiente de Gini
h2o.giniCoef(modelo_glm, train = TRUE)
# Desviance del modelo nulo
h2o.null_deviance(modelo_glm, train = TRUE)
# Desviance del modelo final
h2o.residual_deviance(modelo_glm, train = TRUE)
# AIC
h2o.aic(modelo_glm, train = TRUE)


h2o.performance(model = modelo_glm, train = TRUE)


# Para obtener metricas de la validación cruzada

# Área bajo la curva
h2o.auc(modelo_glm, xval = TRUE)


# todas las metricas bajo validación cruzada

h2o.performance(model = modelo_glm, xval = TRUE)



# Predecir con un nuevo conjutn de la base de prueba
predicciones <- h2o.predict(object = modelo_glm, newdata = datos_test_h2o)
predicciones



h2o.performance(model = modelo_glm, newdata = datos_test_h2o)


# Cálculo manual del accuracy
mean(as.vector(predicciones$predict) == as.vector(datos_test_h2o$salario))

