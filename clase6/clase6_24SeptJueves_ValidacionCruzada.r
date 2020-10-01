library(rsample)
library(dplyr)


data("iris")
iris$ID <- 1:150
iris <- iris[c("ID", "Sepal.Length", "Sepal.Width", 
               "Petal.Length", "Petal.Width", 
               "Species")]
iris$y <- as.numeric(iris$Species == "setosa")
cv_5fold <- vfold_cv(iris, v = 5)

iris1 <- cv_5fold$splits[[1]] %>% as.data.frame() 
iris1_analysis <- cv_5fold$splits[[1]] %>% analysis() 

iris1_assessment <- cv_5fold$splits[[1]] %>% assessment() 

dim(iris1_analysis)
dim(iris1_assessment)

iris2 <- cv_5fold$splits[[2]] %>% as.data.frame() 
iris3 <- cv_5fold$splits[[3]] %>% as.data.frame() 
iris4 <- cv_5fold$splits[[4]] %>% as.data.frame() 
iris5 <- cv_5fold$splits[[5]] %>% as.data.frame() 
# Muestra test 1

id1 <- iris$ID[!(iris$ID %in% iris1$ID)]
id1 <- (cv_5fold$splits[[1]] %>% assessment())$ID

id2 <- iris$ID[!(iris$ID %in% iris2$ID)]
id3 <- iris$ID[!(iris$ID %in% iris3$ID)]
id4 <- iris$ID[!(iris$ID %in% iris4$ID)]
id5 <- iris$ID[!(iris$ID %in% iris5$ID)]

# intersect(id1, id2)
# intersect(id1, id3)
# intersect(id1, id4)
# intersect(id1, id5)
# intersect(id2, id3)
# intersect(id2, id4)
# intersect(id2, id5)
# intersect(id3, id4)
# intersect(id3, id5)
# intersect(id4, id5)

id1
id2
id3
id4
id5
sort(c(id1, id2, id3, id4, id5))


library(rsample)
library(dplyr)

data("iris")
iris$ID <- 1:150
iris <- iris[c("ID", "Sepal.Length", "Sepal.Width", 
               "Petal.Length", "Petal.Width", 
               "Species")]
iris$y <- as.numeric(iris$Species == "setosa")
set.seed(12345)
trainig <- iris[sample(nrow(iris), round(0.8*nrow(iris))),]

cv_10fold <- vfold_cv(trainig, v = 10)

# analysis_set2 <- cv_5fold$splits[[1]] %>% analysis()
# assesment_set2 <- cv_5fold$splits[[1]] %>% assessment()

modelo_logistico <- function(particion){
  
  modelo <- glm(y ~ Sepal.Length , data = analysis(particion), family = "binomial")
  test <- assessment(particion)
  test$yhat <- as.numeric(predict(modelo, test, type = "response") > 0.5) 
  mc <- table(test$y, test$yhat) 
  precision <- sum(diag(mc)) / sum(mc)
  precision
}


modelo_logistico(cv_10fold$splits[[1]])
modelo_logistico(cv_10fold$splits[[2]])
modelo_logistico(cv_10fold$splits[[3]])
modelo_logistico(cv_10fold$splits[[9]])
modelo_logistico(cv_10fold$splits[[10]])


vct_accuracies <- sapply(cv_10fold$splits, modelo_logistico)
mean(vct_accuracies)
hist(vct_accuracies)

lista <- list(V1 =  data.frame(ID = c(1:4), y= rnorm(4)), V2 = data.frame(ID2 = c(1:9), y = rnorm(9)))
lapply(lista, dim)
lapply(lista, nrow)
sapply(lista, nrow)



############################ Repeated Cross Validation #######################
cv_5fold_repetead <- vfold_cv(iris, v = 5, repeats = 2)
iris1_rep1 <- cv_5fold_repetead$splits[[1]] %>% as.data.frame() 
iris2_rep1 <- cv_5fold_repetead$splits[[2]] %>% as.data.frame() 
iris3_rep1 <- cv_5fold_repetead$splits[[3]] %>% as.data.frame() 
iris4_rep1 <- cv_5fold_repetead$splits[[4]] %>% as.data.frame() 
iris5_rep1 <- cv_5fold_repetead$splits[[5]] %>% as.data.frame() 
iris1_rep2 <- cv_5fold_repetead$splits[[6]] %>% as.data.frame() 
iris2_rep2 <- cv_5fold_repetead$splits[[7]] %>% as.data.frame() 
iris3_rep2 <- cv_5fold_repetead$splits[[8]] %>% as.data.frame() 
iris4_rep2 <- cv_5fold_repetead$splits[[9]] %>% as.data.frame() 
iris5_rep2 <- cv_5fold_repetead$splits[[10]] %>% as.data.frame() 


# La primera repetición
id1_rep1 <- iris$ID[!(iris$ID %in% iris1_rep1$ID)]
id2_rep1 <- iris$ID[!(iris$ID %in% iris2_rep1$ID)]
id3_rep1 <- iris$ID[!(iris$ID %in% iris3_rep1$ID)]
id4_rep1 <- iris$ID[!(iris$ID %in% iris4_rep1$ID)]
id5_rep1 <- iris$ID[!(iris$ID %in% iris5_rep1$ID)]

intersect(id1_rep1, id2_rep1)
intersect(id1_rep1, id3_rep1)
intersect(id1_rep1, id4_rep1)
intersect(id1_rep1, id5_rep1)
intersect(id2_rep1, id3_rep1)
intersect(id2_rep1, id4_rep1)
intersect(id2_rep1, id5_rep1)
intersect(id3_rep1, id4_rep1)
intersect(id3_rep1, id5_rep1)
intersect(id4_rep1, id5_rep1)

id1_rep1
id2_rep1
id3_rep1
id4_rep1
id5_rep1
sort(c(id1_rep1, id2_rep1, id3_rep1, id4_rep1, id5_rep1))

# La segunda repetición
id1_rep2 <- iris$ID[!(iris$ID %in% iris1_rep2$ID)]
id2_rep2 <- iris$ID[!(iris$ID %in% iris2_rep2$ID)]
id3_rep2 <- iris$ID[!(iris$ID %in% iris3_rep2$ID)]
id4_rep2 <- iris$ID[!(iris$ID %in% iris4_rep2$ID)]
id5_rep2 <- iris$ID[!(iris$ID %in% iris5_rep2$ID)]

intersect(id1_rep2, id2_rep2)
intersect(id1_rep2, id3_rep2)
intersect(id1_rep2, id4_rep2)
intersect(id1_rep2, id5_rep2)
intersect(id2_rep2, id3_rep2)
intersect(id2_rep2, id4_rep2)
intersect(id2_rep2, id5_rep2)
intersect(id3_rep2, id4_rep2)
intersect(id3_rep2, id5_rep2)
intersect(id4_rep2, id5_rep2)

id1_rep2
id2_rep2
id3_rep2
id4_rep2
id5_rep2
sort(c(id1_rep2, id2_rep2, id3_rep2, id4_rep2, id5_rep2))



############################# Estratificado ################################
cv_5fold_estratificado <- vfold_cv(iris, v = 5, strata = "Species")
datos <- cv_5fold_estratificado$splits[[1]] %>% as.data.frame()
150/5
# Seleccionar 120 en la muestra, se reparti de forma proporcional al tamaño en los tres estratos.
table(datos$Species)

datos2 <- cv_5fold$splits[[1]] %>% as.data.frame()
150/5
table(datos2$Species)


################### Bootstraps ##########################################
bootstraps <- bootstraps(iris, times = 10)
bootstraps$splits[[1]] %>% as.data.frame()
