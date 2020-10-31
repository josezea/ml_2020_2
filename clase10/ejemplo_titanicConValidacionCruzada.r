library("rpart")
library("rpart.plot")
library(rsample)
library(tidymodels)

setwd("C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/Semestre2/MachineLearning/Clase 10")
titanic <- read.csv("titanic.csv")

titanic = titanic[, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Fare")]

titanic = titanic[complete.cases(titanic), ]
titanic$Pclass = as.numeric(titanic$Pclass)

#Es una visión descriptiva, no srive para mucho.
tree = rpart( Survived ~ Pclass + Sex + Age + SibSp, data = titanic, method = "class", control = list( minsplit = 90 ) )

rpart.plot(tree)

particion = initial_split(titanic, prop = 0.7, strata = Survived)
titanic_training = particion %>% training()
titanic_test= particion %>% testing() 

#the minimum number of observations that must exist in a node in order for a split to be attempted.
treev2 = rpart( Survived ~ Pclass + Sex + Age + SibSp, data = titanic_training, method = "class", control = list( minsplit = 60 ) )

reglog = glm( Survived ~ Pclass + Sex + Age + SibSp, data = titanic_training, family = "binomial")

rpart.plot(treev2)

matriz_confusion = function(data, model, dependent){
  Predicted = predict(model, type="class",newdata=data)
  Real = data[, dependent]
  conf = table(Real, Predicted)
  accuracy = mean(ifelse(Predicted == Real, 1, 0))
  return(list("ConfusionMatrix"= conf, "Accuracy" = accuracy))
}


matriz_confusion(titanic_test, treev2, "Survived")


table(titanic_test$Survived, predict(reglog, titanic_test, type = "response")>0.5 )


# COn tidymodels
titanic_training$Survived <- factor(titanic_training$Survived)
modelo_tree <- decision_tree(mode = "classification") %>%
  set_engine(engine = "rpart")


# Entrenamiento empleando fórmula
modelo_tree_fit <- modelo_tree %>%
  fit(
    formula = Survived ~ Pclass + Sex + Age + SibSp,
    data    = titanic_training
  )


set.seed(1234)
cv_folds <- vfold_cv(
  data    = titanic_training,
  v       = 10,
  #repeats = 10,
  strata  = "Survived")



# validación cruzada

validacion_fit <- fit_resamples(
  object       = modelo_tree,
  # El objeto recipe no tiene que estar entrenado
  preprocessor = recipe(
    formula = Survived ~ Pclass + Sex + Age + SibSp, 
    data =  titanic_training),
    resamples    = cv_folds,
   metrics      = metric_set(roc_auc, pr_auc, accuracy),
  control      = control_resamples(save_pred = TRUE)
)
validacion_fit


validacion_fit %>% collect_metrics(summarize = TRUE)

# Predcción en el train no hace sentido
#validacion_fit %>% collect_predictions(summarize = TRUE) %>% head()



predicciones <- modelo_tree_fit %>%
  predict(
    new_data = titanic_test,
    #new_data = bake(transformer_fit, titanic_test)
    #type = "numeric"
  )
predicciones %>% head()

mc <- table(y = titanic_test$Survived, yhat = predicciones$.pred_class)
sum(diag(mc)) / sum(mc)
 