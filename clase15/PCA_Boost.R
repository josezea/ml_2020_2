library(tidyverse, warn.conflicts = FALSE)
library(tidymodels, warn.conflicts = FALSE)


load(file = './cancer.RData')

# Particion
set.seed(123)
c_train <- initial_split(cancer, 0.8) %>% training()
set.seed(123)
c_test <- initial_split(cancer, 0.8) %>% testing()

# Umbral a partir del cual se retienen los componentes
umbral <- 0.89

# Receta
pca_recipe <- recipe(~., data = c_train) %>% 
  update_role(Clase, new_role = 'outcome') %>%
  update_role(Sample.code.number, new_role = "id") %>%
  update_role(-Clase, -Sample.code.number, new_role = 'predictor') %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors(), threshold = umbral)

verifica <- pca_recipe %>% summary()

# Roles de las variables
table(verifica$role, useNA='always')

# Preprocesado prep()
pca_preprocesado <- pca_recipe %>% prep()

# Porcentaje de variacion retenido:
sdev <- pca_preprocesado$steps[[3]]$res$sdev
percent_variation <- sdev^2 / sum(sdev^2)
percent_variation
cumsum(percent_variation)

# Plot
juice(pca_preprocesado) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = factor(c_train$Clase, levels = c(2,4), 
              labels = c('Benigno', 'Maligno'))), alpha = 0.7, size = 2) +
  labs(title="PCA desde tidymodels", color='Tumor')

# A modelar

library(xgboost)

c_train$Clase <- as.numeric(c_train$Clase)
c_train$Clase[c_train$Clase ==1] = 0
c_train$Clase[c_train$Clase == 2] = 1

# Validacion cruzada
cancer_cv_folds <- recipes::bake(pca_preprocesado, new_data = c_train) %>%  
  rsample::vfold_cv(v = 5)

# Definir el modelo
xgboost_model <- parsnip::boost_tree(mode = "regression",
    trees = 1000, 
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()) %>%
    set_engine("xgboost", objective = "reg:squarederror")

# parametros grilla
xgboost_params <- dials::parameters(
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction())
parameters(xgboost_model)

# Workflow
xgboost_wf <- workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(Clase ~ .)

# Grilla
xgboost_grid <- dials::grid_max_entropy(xgboost_params, 
    size = 12) # Puede aumentarse a 60, pero implica mucho más tiempo
head(xgboost_grid)

# eXtreme Gradient Boosting
inicia <- Sys.time()
xgboost_tuned <- tune::tune_grid(object = xgboost_wf,
  resamples = cancer_cv_folds,
  grid = xgboost_grid,
  metrics = yardstick::metric_set(rmse, rsq, mae),
  control = tune::control_grid(verbose = FALSE))
termina <- Sys.time()
(tiempo <- termina - inicia)

# Tuning
xgboost_tuned %>% 
  tune::show_best(metric = "rmse")

# Mejor set de parámetros
xgboost_best_params <- xgboost_tuned %>%
  tune::select_best("rmse")
xgboost_best_params

# Modelo final' 
xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)

# Prediccion
train_procesado <- bake(pca_preprocesado,  new_data = c_train)
train_prediccion <- xgboost_model_final %>%
  fit(formula = Clase ~ ., data = train_procesado) %>%
      predict(new_data = train_procesado) %>%
      bind_cols(c_train)
xgboost_score_train <- train_prediccion %>%
  yardstick::metrics(Clase, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
xgboost_score_train

# Receta sobre el test pata evitar el leakage
pca_recipe <- recipe(~., data = c_test) %>% 
  update_role(Clase, new_role = 'outcome') %>%
  update_role(Sample.code.number, new_role = "id") %>%
  update_role(-Clase, -Sample.code.number, new_role = 'predictor') %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors(), threshold = umbral)

pca_preprocesado <- pca_recipe %>% prep()

test_procesado  <- bake(pca_preprocesado, new_data = c_test)
test_prediccion <- xgboost_model_final %>%
    fit(formula = Clase ~ ., 
    data = train_procesado) %>%
    predict(new_data = test_procesado) %>%
    bind_cols(c_test)
xgboost_score <- test_prediccion %>%
  yardstick::metrics(Clase, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
xgboost_score


