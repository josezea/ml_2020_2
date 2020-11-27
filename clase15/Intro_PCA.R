library(FactoMineR)
library(TeachingSampling)
data(Lucy)
?FactoMineR::PCA()
class(Lucy$Employees)
sapply(Lucy, class) 
a <- data.frame(a = 1:4, b = 5:8, c = 9:12, d = 13:16)
a[,c(2,3)]
a[,c(F,T,T,F)]
df_cont <- Lucy[,sapply(Lucy, class)  %in% 
                  c("integer", "numeric")]
cor(df_cont)
plot(df_cont)

pca_lucy <- PCA(df_cont)
names(pca_lucy)
pca_lucy$eig # Varianza total V(X1) + V(X2) + V(X3) = lambda_1 + . lambda_3
2.48089501  / (2.48089501 + 0.43606093 + 0.08304406 )

head(pca_lucy$ind$coord)

head(pca_lucy$ind$coord[,1])
var(pca_lucy$ind$coord[,1])
var(pca_lucy$ind$coord[,2])
var(pca_lucy$ind$coord[,3])

# La varianza total es:
var(scale(df_cont)[,1]) + var(scale(df_cont)[,2]) +
  var(scale(df_cont)[,3])


pca_lucy$var$coord
#F1 <- 0.9475842  * Income + 0.8271643 * Employees +  0.9480392 * Taxes 

# COrrelación de los componentes con las variables originales
pca_lucy$var$cor

# Cosenos cuadrados:  Donde queda bien representada cada una de las variables en los nuevos ejes
pca_lucy$var$cos2
rowSums(pca_lucy$var$cos2)

# Cada eje nuevo por que variables es explicada 
pca_lucy$var$contrib
colSums(pca_lucy$var$contrib)


df_cont2 <- cbind(df_cont, as.data.frame(pca_lucy$ind$coord))
cor(df_cont2)

df_cont2$cluster_infoCompleta <- kmeans(df_cont2[c("Income", "Employees", "Taxes")], 
                                        centers = 3, iter.max = 100)$cluster
table(df_cont2$cluster_infoCompleta)

df_cont2$cluster_ACP<- kmeans(df_cont2[c("Dim.1", "Dim.2")], 
                                        centers = 3, iter.max = 100)$cluster

table(df_cont2$cluster_infoCompleta, df_cont2$cluster_ACP)

sum(diag(table(df_cont2$cluster_infoCompleta, df_cont2$cluster_ACP)
)) / sum(table(df_cont2$cluster_infoCompleta, df_cont2$cluster_ACP)
) * 100


df_cont2 <- cbind(df_cont2, data.frame(Level = Lucy$Level))
modelo <- nnet::nnet(Level~Dim.1 + Dim.2, data = df_cont2, size = 10)
table(Lucy$Level, predict(modelo, df_cont2, type = "class") )

