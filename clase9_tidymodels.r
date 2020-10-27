rm(list = ls())

library(rsample)
library(recipes) # Preprocesamiento
library(parsnip) # Modelos (interface unificada, glm , stanglm, glmnet)
library(yardstick) # Métricas

data(iris)

iris$z1 <- (iris$Sepal.Length - mean(iris$Sepal.Length)) / sd(iris$Sepal.Length)
summary(iris$z1)
sd(iris$z1)
data(iris)
summary(scale(iris[,-5]))
sapply(as.data.frame(scale(iris[,-c(5)])), sd)
       


# Los tres pasos cuando hay solo train y test

data(iris)
iris$y <- factor(ifelse(iris$Species == "setosa", 1, 0))
# iris$Species <- NULL

# Realizar la partición en train y test
set.seed(12345)
train_test_split_iris <- rsample::initial_split(data = iris, prop = 0.80, strata = "Species")
train_iris <- train_test_split_iris %>% training() 
test_iris  <- train_test_split_iris %>% testing()

# f(x, y): x %>% f(y)

# Argumento, todas las variables y ~ .
# recipe(formula, data, pasos, pasos, pasos,... )
receta_iris <- recipe(y ~ . , data = iris) %>% # outcome ~ explanatory variables  # Establecer los y, los x
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())  %>% 
             step_rm(contains("Species")) # step_center(Petal.Length )

# Tomar los pasos de preprocesamiento y calcular los insumos a partir de la muestra train (promedio y la desviación para realizar la estandarización)
iris_prep <- prep(receta_iris, train_iris)


# Bake: aplicar lo anterior en los datos
iris_train_baked <- bake(iris_prep, train_iris)
#(train_iris$Sepal.Length[1] - mean(train_iris$Sepal.Length)) / sd(train_iris$Sepal.Length) 

iris_test_baked <- bake(iris_prep, test_iris)
#(test_iris$Sepal.Length[1] - mean(train_iris$Sepal.Length)) / sd(train_iris$Sepal.Length) 



reglogistica <- logistic_reg(mode = "classification") %>%
  set_engine("glm")  %>% # stan, spark, stan, h2o
  fit(y ~  Sepal.Width , data = iris_train_baked)

# predecir en la muestra de entrenamiento y sacar las métricas

# threshold (umbral) diferente a 0.5
df_predict <- data.frame(y = iris_test_baked$y, 
                         yhat = predict(reglogistica, new_data = iris_test_baked))

matriz_confusion <- yardstick::conf_mat(df_predict, y, .pred_class)
summary(matriz_confusion)


# Validación cruzada

############################# Validación cruzada ###################################
rm(list = ls())

data(iris)
iris$y <- factor(ifelse(iris$Species == "setosa", 1, 0))
# Realizar la partición en train y test
# poner semilla
train_test_split_iris <- rsample::initial_split(data = iris, prop = 0.80)
train_iris <- train_test_split_iris %>% training() 
test_iris  <- train_test_split_iris %>% testing()

# Estandarizar, quitar las variables categóricas con muy poca variabilidad, 
# Aplicar el feature engineering sobre el training
# Con estas instrucciones los promedios y desviaciones estándar para organizar se sacan de la muestra de entrenamiento
receta_iris <- recipe(y ~ ., data = iris) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())

# Aplicar instrucciones en muestra de entrenameniento
# train_iris_baked <- bake(receta_preparada_iris, new_data = train_iris)
# test_iris_baked  <- bake(receta_preparada_iris, new_data = test_iris)

cross_val_iris <- vfold_cv(train_iris, v = 10)

# cross_val_iris$splits %>%   pluck(1)
# cross_val_iris$splits[[1]]

lista_particiones <- cross_val_iris$splits

# Función para una sóla partición
# particion <- cross_val_iris$splits[[1]]
f_preparacionCV <- function(particion){
  analysis_set <- analysis(particion)
  analysis_prepped <- prep(receta_iris, analysis_set)
  analysis_baked <- bake(analysis_prepped, new_data = analysis_set)
  
  reglogistica <- logistic_reg(mode = "classification") %>%
    set_engine("glm")  %>%
    fit(y ~  Sepal.Width , data = analysis_baked)
  
  
  assessment_set <- assessment(particion)
  assessment_prepped  <- prep(receta_iris, assessment_set)
  assessment_baked <- bake(assessment_prepped, new_data = assessment_set)
  
  salida <- data.frame(y = assessment_baked$y, yhat = predict(reglogistica, new_data = assessment_baked))
  names(salida) <- c("y", "yhat")
  #table(salida$y, salida$yhat)
  #salida2 <- list(salida, reglogistica)
  #salida2
  salida
}

# f_preparacionCV(cross_val_iris$splits[[1]])
lista_pron <- lapply(lista_particiones, f_preparacionCV) # f(particion1), f(particion2),...f(particion10)
#lista_pron <- purrr::map(lista_particiones, f_preparacionCV)

# Métricas con el paquete yardstick
f_pronosticos <- function(lista_DFYPRON){
  matriz_confusion <- conf_mat(lista_DFYPRON, y, yhat) 
  salida <- summary(matriz_confusion) %>% filter(.metric %in%
              c("accuracy", "sens", "spec", "f_meas"))
  salida 
}


resultados <- bind_rows(lapply(lista_pron, f_pronosticos))
resultados <- resultados %>% group_by(.metric) %>% summarise(prom_metrica = mean(.estimate))
resultados
# f_pronosticos(lista_pron[[1]])

