
setwd("/home/jose/Documentos/ML/")
load("churn_celulares.RData")
set.seed(17092020)
train_cel <- insumo[sample(280, 196),]
set.seed(17092020)
test_cel <- insumo[-sample(280, 196),]

M1 <- glm(target ~  calidad_produc + cant_cargas_m1, data = train_cel,
          family = "binomial")

test_cel$probs_churn <- predict(M1, test_cel, type = "response")

mc_churn <- table(test_cel$target, 
                  as.numeric(test_cel$probs_churn >= 0.5))
mc_churn 
# Accuracy
sum(diag(mc_churn)) / sum(mc_churn) *100

sensibilidad <- mc_churn[2,2] / sum(mc_churn[2,])
especificidad <- mc_churn[1,1] / sum(mc_churn[1,])
plot(1-especificidad, sensibilidad, xlim = c(0,1), ylim = c(0,1), pch = 20)
abline(a = 0, b = 1, col = "red")
# Estimación del punto de la curva ROC asociado a p = 0.5

# Ejercico 2: calcular la curva ROC
probabilidades <- seq(0.05, 0.95, 0.05)
vctr_sensibilidad <- rep(NA, length(probabilidades))
vctr_especificicad <- rep(NA, length(probabilidades))
probs_churn <- predict(M1, test_cel, type = "response")

for(i in 1:length(probabilidades)){

mc_churn <- table(test_cel$target, 
                  as.numeric(probs_churn >= probabilidades[i]))
mc_churn <- as.matrix(mc_churn)

if(ncol(mc_churn) == 1 & colnames(mc_churn)[1] == "1"){
  mc_churn <- cbind(c(0,0), mc_churn)
  colnames(mc_churn)[1] <- "0" 
}

if(ncol(mc_churn) == 1 & colnames(mc_churn)[1] == "0"){
  mc_churn <- cbind(mc_churn, c(0,0))
  names(mc_churn)[1] <- "1" 
}
vctr_sensibilidad[i] <- mc_churn[2,2] / sum(mc_churn[2,])
vctr_especificicad[i] <- mc_churn[1,1] / sum(mc_churn[1,])
}

plot(1-vctr_especificicad, vctr_sensibilidad, pch = 20 , xlab = "1- Espec (Error tipo 2)", 
     ylab = "Sensibilidad", xlim = c(0,1), ylim = c(0,1))
lines(1-vctr_especificicad, vctr_sensibilidad, col = "blue" )
text(1-vctr_especificicad - 0.03, vctr_sensibilidad, labels = probabilidades, cex = 0.7)
abline(a = 0, b = 1, col = "red")
abline(v = 1 - 0.9512, col = "green")
abline(h = 0.6744, col = "green")



# Ejercico 3: calcular la curva ROC
probabilidades <- seq(0.01, 0.99, 0.01)
vctr_sensibilidad <- rep(NA, length(probabilidades))
vctr_especificicad <- rep(NA, length(probabilidades))
probs_churn <- predict(M1, test_cel, type = "response")

for(i in 1:length(probabilidades)){
  
  mc_churn <- table(test_cel$target, 
                    as.numeric(probs_churn >= probabilidades[i]))
  mc_churn <- as.matrix(mc_churn)
  
  if(ncol(mc_churn) == 1 & colnames(mc_churn)[1] == "1"){
    mc_churn <- cbind(c(0,0), mc_churn)
    colnames(mc_churn)[1] <- "0" 
  }
  
  if(ncol(mc_churn) == 1 & colnames(mc_churn)[1] == "0"){
    mc_churn <- cbind(mc_churn, c(0,0))
    names(mc_churn)[1] <- "1" 
  }
  vctr_sensibilidad[i] <- mc_churn[2,2] / sum(mc_churn[2,])
  vctr_especificicad[i] <- mc_churn[1,1] / sum(mc_churn[1,])
}

plot(1-vctr_especificicad, vctr_sensibilidad, pch = 20 , xlab = "1- Espec (Error tipo 2)", 
     ylab = "Sensibilidad", xlim = c(0,1), ylim = c(0,1))
lines(1-vctr_especificicad, vctr_sensibilidad, col = "blue" )
#text(1-vctr_especificicad - 0.03, vctr_sensibilidad, labels = probabilidades, cex = 0.7)
abline(a = 0, b = 1, col = "red")
abline(v = 1 - 0.9512, col = "green" , lty = 2)
abline(h = 0.6744, col = "green", lty = 2)


library(ROCR)
pred_ROCR <- prediction(test_cel$probs_churn, test_cel$target)
auc <- performance(pred_ROCR, "auc")
auc@y.values[[1]]
