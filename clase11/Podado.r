library(dplyr)
library(rpart)
library(rsample)
library(rpart.plot) #rpart plot graphics
library(readr)
# Proceso de podado

setwd("C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/Semestre2/MachineLearning/Clase11")
dir()
hr <- read_csv("hr.csv")

set.seed(12345)
particion <- rsample::initial_split(hr, 0.7)
train <- training(particion)
test <- testing(particion) 


# El parámetro CP es usado para controlar el crecimiento del árbol.
# SI el consto de agregar una variable es más alta el árbol para.

modelo1 <- rpart(left ~ ., data = train, method = "class", control = rpart.control(cp = 0))
#plot(modelo1)
rpart.plot::rpart.plot(modelo1)
printcp(modelo1)
plotcp(modelo1)

# Elegir el que tenga un menor error en la validación cruzada
0.00015139 # 17 árboles

# Hacer el podado con esta función 
# modelo2 <- rpart(left ~ ., data = train, method = "class", control = rpart.control(cp = 0.00015139))

modelo2 <- rpart::prune(modelo1, cp = 0.00015139)
rpart.plot(modelo2)


accuracy_modelo1 <- mean(predict(modelo1, test, type = "class") == test$left) # 37 particiones
accuracy_modelo2 <- mean(predict(modelo2, test, type = "class") == test$left) # 17 pariciones

1-0.094873 * 0.2359



