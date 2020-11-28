library(tidyverse, warn.conflicts = FALSE)
library(tidymodels, warn.conflicts = FALSE)
rm(list = ls())
load(file = './cancer.RData')

# Particion
set.seed(123)
c_train <- initial_split(cancer, 0.8) %>% training()
set.seed(123)
c_test <- initial_split(cancer, 0.8) %>% testing()

# A modelar
library(xgboost)

# xgboost no admite que en clasificación el outcome sea diferente a '0' - '1'
c_train$Clase[c_train$Clase ==2] = '0'
c_train$Clase[c_train$Clase == 4] = '1'
# Elimino la variable Sample.code.number que no s util para el modelo
c_train$Sample.code.number <- NULL

# Umbral a partir del cual se retienen los componentes
umbral <- 0.89

# eXtreme Gradient Boosting
# Clasificación
xgboost_model <- parsnip::boost_tree(trees = tune(), 
                      min_n = tune(),
                      tree_depth = tune(),
                      learn_rate = tune(),
                      loss_reduction = tune()) %>%
  set_mode('classification') %>% 
  set_engine("xgboost")

# Receta
# Es necesario especificar roles para que el tuning corra
pca_recipe <- recipe(formula = Clase ~ ., data = c_train) %>% 
  update_role(Clase, new_role = 'outcome') %>%
  update_role(-Clase, new_role = 'predictor') %>%
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors(), threshold = umbral) 

verifica <- pca_recipe %>% summary()

# Roles de las variables
table(verifica$role, useNA='always')

# Preprocesado prep()
pca_preprocesado <- pca_recipe %>% prep()

# Porcentaje de variacion retenido:
sdev <- pca_preprocesado$steps[[2]]$res$sdev
percent_variation <- sdev^2 / sum(sdev^2)
percent_variation
cumsum(percent_variation)

# Plot
juice(pca_preprocesado) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = factor(c_train$Clase, levels = c('0', '1'), 
            labels = c('Benigno', 'Maligno'))), alpha = 0.7, size = 2) +
  labs(title="PCA desde tidymodels", color='Tumor')

# Validacion cruzada
cancer_cv_folds <- recipes::bake(pca_preprocesado, new_data = c_train) %>%  
  rsample::vfold_cv(v = 2)

# parametros grilla 
xgboost_params <- dials::parameters(trees(),
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction())
parameters(xgboost_model)

# Workflow
xgboost_wf <- workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(formula = Clase ~ .)

# Grilla
xgboost_grid <- dials::grid_regular(xgboost_params)
dim(xgboost_grid)
head(xgboost_grid)

# tuning
xgboost_tuned <- tune::tune_grid(object = xgboost_wf,
  resamples = cancer_cv_folds,
  grid = xgboost_grid,
  metrics = yardstick::metric_set(accuracy, kap, roc_auc, recall),
  control = tune::control_grid(verbose = FALSE))

xgboost_tuned %>% tune::show_best(metric = "accuracy")
xgboost_tuned %>% tune::show_best(metric = "roc_auc")
xgboost_tuned %>% tune::show_best(metric = "kap")
xgboost_tuned %>% tune::show_best(metric = "recall")

# Mejor set de parámetros
xgboost_best_params <- xgboost_tuned %>%
  tune::select_best("roc_auc")
xgboost_best_params

# Modelo final' 
xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)

# Prediccion sobre el train
train_procesado <- bake(pca_preprocesado,  new_data = c_train)
train_prediccion <- xgboost_model_final %>%
  fit(formula = Clase ~ ., data = train_procesado) %>%
      predict(new_data = train_procesado) %>%
      bind_cols(train_procesado)
xgboost_score_train <- train_prediccion %>%
  yardstick::metrics(Clase, .pred_class) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
xgboost_score_train

# Prerparo el test
c_test$Clase[c_test$Clase ==2] = '0'
c_test$Clase[c_test$Clase == 4] = '1'
c_test$Sample.code.number <- NULL

# Receta sobre el test pata evitar el leakage
pca_recipe <- recipe(formula = Clase ~ ., data = c_test) %>% 
  update_role(Clase, new_role = 'outcome') %>%
  update_role(-Clase, new_role = 'predictor') %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), threshold = umbral)

pca_preprocesado <- pca_recipe %>% prep()

test_procesado  <- bake(pca_preprocesado, new_data = c_test)
# Predigo sobre el test
test_prediccion <- xgboost_model_final %>%
    fit(formula = Clase ~ ., data = test_procesado) %>%
    predict(new_data = test_procesado) %>%
    bind_cols(test_procesado)
xgboost_score <- test_prediccion %>%
  yardstick::metrics(Clase, .pred_class) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
xgboost_score


