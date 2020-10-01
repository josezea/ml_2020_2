rm(list = ls())
library(rsample)
library(dplyr)


data("iris")
iris$ID <- 1:150
iris <- iris[c("ID", "Sepal.Length", "Sepal.Width", 
               "Petal.Length", "Petal.Width", 
               "Species")]
iris$y <- as.numeric(iris$Species == "setosa")

set.seed(26092020)
particion <- initial_split(iris, prop = 0.7)
class(particion)
train <- particion %>% training() #  training(particion);  f(x, y)  <=>  x %>% f(y)
test <- particion %>% testing()

# La validación cruzada se hace sobre la muestra tain
set.seed(26092020)
cv_5fold <- vfold_cv(train, v = 5)
cv_5fold
cv_5fold$splits

assessment1 <- cv_5fold$splits[[1]] %>% assessment() 
assessment2 <- cv_5fold$splits[[2]] %>% assessment() 
assessment3 <- cv_5fold$splits[[3]] %>% assessment() 
assessment4 <- cv_5fold$splits[[4]] %>% assessment() 
assessment5 <- cv_5fold$splits[[5]] %>% assessment() 

# intersect(assessment1$ID , assessment2$ID)
# intersect(assessment3$ID , assessment4$ID)
sort(c(assessment1$ID, assessment2$ID, assessment3$ID, assessment4$ID,
       assessment5$ID, assessment5$ID))
# Sub training
analysis1 <- cv_5fold$splits[[1]] %>% analysis() 
analysis2 <- cv_5fold$splits[[2]] %>% analysis() 
analysis3 <- cv_5fold$splits[[3]] %>% analysis() 
analysis4 <- cv_5fold$splits[[4]] %>% analysis() 
analysis5 <- cv_5fold$splits[[5]] %>% analysis() 


modelo <- glm(y ~ Sepal.Length , data = analysis1, family = "binomial")
test <- assessment1
test$yhat <- as.numeric(predict(modelo, test, type = "response") > 0.5) 
mc <- table(test$y, test$yhat) 
precision <- sum(diag(mc)) / sum(mc)
precision


modelo <- glm(y ~ Sepal.Length , data = analysis2, family = "binomial")
test <- assessment2
test$yhat <- as.numeric(predict(modelo, test, type = "response") > 0.5) 
mc <- table(test$y, test$yhat) 
precision <- sum(diag(mc)) / sum(mc)
precision



modelo_logistico <- function(particion){
  
  modelo <- glm(y ~ Sepal.Length , data = analysis(particion), family = "binomial")
  test <- assessment(particion)
  test$yhat <- as.numeric(predict(modelo, test, type = "response") > 0.5) 
  mc <- table(test$y, test$yhat) 
  precision <- sum(diag(mc)) / sum(mc)
  precision
}


modelo_logistico(cv_5fold$splits[[1]])
modelo_logistico(cv_5fold$splits[[2]])
modelo_logistico(cv_5fold$splits[[3]])
modelo_logistico(cv_5fold$splits[[4]])
modelo_logistico(cv_5fold$splits[[5]])

# Parenetesis 
lapply(list(a = c(1,2), b = c(4, 6)), mean)
sapply(list(a = c(1,2), b = c(4, 6)), mean)
data(iris)
lapply(list(mtcars, iris), dim)
lapply(list(mtcars, iris), nrow)
sapply(list(mtcars, iris), nrow)


iris[-5]

vct_accuracies <- sapply(cv_5fold$splits, modelo_logistico)
mean(vct_accuracies)
hist(vct_accuracies)

library(purrr)
vct_accuracies <- purrr::map_dbl(cv_5fold$splits, modelo_logistico)
vct_accuracies


############################# Estratificado ################################
cv_5fold_estratificado <- vfold_cv(train, v = 5, strata = "y")
datos1_analysis <- cv_5fold_estratificado$splits[[1]] %>% analysis()
datos1_assesment <- cv_5fold_estratificado$splits[[1]] %>% assessment()

table(datos1_analysis$y)
table(datos1_assesment$y)
dim(train)

#105 / 5 #(el subtest (assesment) es de tamaño 21)
# 105 - 21 # El subtrain (analysys) es de tamaño 84


cv_5fold_mas <- vfold_cv(train, v = 5)
datos1_analysis <- cv_5fold_mas$splits[[1]] %>% analysis()
datos1_assesment <- cv_5fold_mas$splits[[1]] %>% assessment()

table(datos1_analysis$y)
table(datos1_assesment$y)

