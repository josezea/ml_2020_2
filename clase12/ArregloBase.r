library(dplyr)
setwd("C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/MachineLearning/Clase8")
dir()

datosF <- readRDS("datosF.rds")


# Anonimización cédulas
df_cedulas <- data.frame(Documento_Identidad = unique(datosF$Documento_Identidad),
                         stringsAsFactors = F)
df_cedulas <- arrange(df_cedulas, Documento_Identidad)
df_cedulas$ID <- 1:nrow(df_cedulas)

datosF <- left_join(datosF, df_cedulas, by = "Documento_Identidad")
datosF$Documento_Identidad <- NULL

# Anonimización plantas
df_plantas <- data.frame(UbicacionR = unique(datosF$UbicacionR),
                         stringsAsFactors = F)
df_plantas <- arrange(df_plantas, UbicacionR)

df_plantas$id_ubicacion <- LETTERS[1:6]


datosF <- left_join(datosF, df_plantas, by = "UbicacionR")
datosF$UbicacionR <- NULL
datosF$Ubicación <- NULL
datosF$`FECHA FIN CONTRATO` <- NULL
datosF$AntigüedadAños <- NULL
datosF$duracion_contratoFinalizado <- NULL
datosF$duracion_contratoAnnosFinalizado <- NULL
datosF$sindicalizado <- NULL
datosF$ranking <- NULL
datosF$`Fecha ingreso` <- NULL
datosF$`Fecha inicio contrato` <- NULL
datosF$estado_civil <- NULL


datosF <- datosF[c("ID", "id_ubicacion", "Fecha", "Salario", "sexo", "Edad",
                   "Tipo_nominaR", 
  "Antig", "CASADO_UNION", "TipocargoRec", 
  "sindicalizadoR", "Retiro")]

names(datosF) <- c("ID", "id_ubicacion", "fecha", "salario", "sexo", "edad",
                   "Tipo_nomina", 
                   "Antiguedad",  "casado_unionlibre", 
                   "tipocargo", 
                   "sindicalizado", "retiro")

writexl::write_xlsx(datosF, "datosChurn.xlsx")
