################################################################################
################################################################################
############################### BENEFICIARIOS ##################################

################################################################################
### LIBRERIAS

library(tidyverse)
library(readxl)
library(openxlsx)
library(readr)
library(skimr)
library(forcats)

################################################################################
### RUTA DE ARCHIVOS
getwd()
#setwd("~/COMPU_IEI/1_JAVERIANA_IEI/MINISTERIO DE AGRICULTURA")

################################################################################
### CARGUE DE DATOS

Beneficiarios  <- read_excel("./code/AGRICULTURA/carguemasivo_beneficiario_estado_Anoni.xlsx", 
                        col_types = c("numeric", "text", "date", 
                         "text", "text", "numeric", "text", 
                         "text", "text", "text", "numeric", 
                         "date", "date", "text", "text", 
                         "text", "numeric", "text", "text", 
                         "text", "numeric", "text", "numeric", 
                         "numeric", "text", "text", "numeric", 
                         "text", "text", "text", "text", "numeric", 
                         "numeric", "numeric", "numeric", 
                         "numeric", "numeric", "text", "numeric", 
                         "numeric", "numeric", "text", "numeric"))

Beneficiarios <- as.data.frame(Beneficiarios)

Dep <- read_delim("Datos productores beneficiarios/Dep.csv", 
                  delim = ";", escape_double = FALSE, col_names = FALSE, 
                  col_types = cols(X1 = col_skip(), X3 = col_double()), 
                  locale = locale(encoding = "ISO-8859-1"), 
                  trim_ws = TRUE)

Dep <- as.data.frame(Dep)

Mun <- read_delim("Datos productores beneficiarios/Mun.csv", 
                  delim = ";", escape_double = FALSE, col_types = cols(Mun1 = col_skip(), 
                                                                       CodigoMunicipio = col_character(), 
                                                                       CodMun = col_double(), Mun2 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1"), 
                  trim_ws = TRUE)

Mun <- as.data.frame(Mun)

#write_csv2(Ben,"Ben.csv")

################################################################################
### HALLAZGOS DE REVISION DE DATOS BENEFICIARIOS
# PRIMERO
# Revisando el set de datos se puede observar Id faltantes, por lo que se precede a borrar la fila completa donde esto ocurra
# Lo anterior es porque esas filas suelen estar vacias y con datos desordenados respecto a las variables que se desean analizar,
# Haciendo esto se espera consolidar un set bastante solido con los registros que se tienen.

names(Beneficiarios); class (Beneficiarios)

################################################################################
## FUNCIONES

###
check_duplicates <- function(dataframe, column_name) {
  if(any(duplicated(dataframe[[column_name]]))) {
    return(paste("La variable", column_name, "tiene duplicados."))
  } else {
    return(paste("La variable", column_name, "no tiene duplicados."))
  }
}

###
check_na <- function(dataframe, column_name) {
  if(any(is.na(dataframe[[column_name]]))) {
    return(paste("La variable", column_name, "contiene valores NA"))
  } else {
    return(paste("La variable", column_name, "no contiene valores NA"))
  }
}

###
reemplazar_valores <- function(df, columnas) {
  for (columna in columnas) {
    df[[columna]] <- ifelse(df[[columna]] == 1, "SI", "NO")
  }
  return(df)  # Asegúrate de devolver el dataframe modificado
}

###
convert_to_upper <- function(df, column_names) {
  for (column_name in column_names) {
    if (column_name %in% colnames(df)) {
      df[[column_name]] <- toupper(df[[column_name]])
    } else {
      print(paste("La columna", column_name, "no existe en el dataframe"))
    }
  }
  return(df)
}


### Para quitar las filas completas si encuentra faltantes en una columna


# "Id"                       "Nro"                      "FechaCorte"              
# "NombreEntidad"            "Programa"                 "MarcoEmergenciaSanitaria"
# "AreaResponsable"          "ProyectoInversion"        "NoConvenio"              
# "DescripcionConvenio"      "Anio"                     "FechaInicio"             
# "FechaFin"                 "NitContratista"           "RazonSocialContratista"  
# "TipoBeneficio"            "Edad"                     "Sexo"                    
# "Genero"                   "Correo"                   "TelefonoContacto"        
# "NombrePredio"             "Departamento"             "Municipio"               
# "Vereda"                   "Direccion"                "Producto"                
# "NoHectareaPredio"         "NoHectareaCultivo"        "MunicipioPDET"           
# "PertenenciaEtnica"        "Discapacidad"             "Victima"                 
# "Desplazados"              "MadreCabezaFamilia"       "ValorBeneficio"          
# "Asociado"                 "Observaciones"            "fila"                    
# "CargueId"                 "FechaCreacion"            "Estado"   


##################################################################################
### VARIABLES A ELIMINAR

#Beneficiarios$Nro
#Beneficiarios$Producto
#Beneficiarios$fila
#Beneficiarios$CargueId
#Beneficiarios$FechaCreacion
#Beneficiarios$Estado
#Beneficiarios$crc

# Identificar las columnas a eliminar
columnas_a_eliminar <- c("Nro", "Producto", "fila", "CargueId", "FechaCreacion", "Estado", "crc")

# Eliminar las columnas
Ben <- Beneficiarios[, !colnames(Beneficiarios) %in% columnas_a_eliminar]

# Visualizar el dataframe
Ben

# Fechas a caracter
Ben$FechaCorte <- as.character(Ben$FechaCorte)
Ben$FechaInicio <- as.character(Ben$FechaInicio)
Ben$FechaFin <- as.character(Ben$FechaFin)

#NULL A NA
# Reemplaza 'NULL' con NA en todas las columnas
Ben[] <- lapply(Ben, function(x) ifelse(x == "NULL", NA, x))

# Reemplaza 'NA' con NA en todas las columnas
Ben[] <- lapply(Ben, function(x) ifelse(x == "NA", NA, x))

# Reemplaza 'NO REGISTRA' con NA en todas las columnas
Ben[] <- lapply(Ben, function(x) ifelse(x == "NO REGISTRA", NA, x))

#################################################################################
### Revisión de NA

check_na(Ben, "Id")
check_na(Ben, "Departamento")

##################################################################################
### Filas a eliminar

Ben <- drop_na(Ben, Id)
Ben <- drop_na(Ben, Departamento)

##################################################################################
### TRANSFORMACION DE VARIABLES

### TRANSFORMACIÓN VARIABLE SEXO
Ben$Sexo <- Ben$Sexo %>%
  fct_collapse(Mujer = c("F", "Mujer"), Hombre = c("M", "Hombre")) %>%
  as.character() %>%
  ifelse(. %in% c("SI", "ND"), NA, .) %>%  toupper()
table(Ben$Sexo, useNA = "ifany")

### TRANSFORMACIÓN VARIABLE GENERO
Ben$Genero <- Ben$Genero %>%
  fct_collapse(Mujer = c("F", "Mujer"), Hombre = c("M", "Hombre")) %>%
  as.character() %>%
  ifelse(. %in% c("NULL", "ND"), NA, .) %>%  toupper()
table(Ben$Genero, useNA = "ifany")




# Hay datos en la variable genero que pueden mejorar la calidad de la variable sexo
# Se decide crear una nueva variable que una ambas
Ben$Sexo2 <- ifelse(is.na(Ben$Sexo), Ben$Genero, Ben$Sexo)

### PASAN DE  0 y 1 a SI y NO
varsino <- c("Discapacidad", "Victima", "Desplazados", "MadreCabezaFamilia", "Asociado", "MarcoEmergenciaSanitaria" )

# Aplicar la función
Ben <- reemplazar_valores(Ben, varsino)

###

### TRANSFORMACIÓN VARIABLE DEPARTAMENTO
Ben <- merge(Ben, Dep[, c("X2", "X3")], by.x = "Departamento", by.y = "X3")
# Renombrar la variable X2 a Nombre_departamento
names(Ben)[names(Ben) == "X2"] <- "Nombre_departamento"

table(Ben$Nombre_departamento, useNA = "ifany")

# Cambiar la codificación
#Ben$Nombre_departamento <- iconv(Ben$Nombre_departamento, from = "latin1", to = "UTF-8")
#Ben$Nombre_departamento <- iconv(Ben$Nombre_departamento, from = "ISO-8859-1", to = "UTF-8")

table(Ben$Departamento, useNA = "ifany") 
table(Ben$Nombre_departamento, useNA = "ifany") 

### TRANSFORMACIÓN VARIABLE MUNICIPIO
# Combinar los conjuntos de datos Ben y Mun por las variables que contienen los códigos numéricos
Ben <- merge(Ben, Mun[, c("Muni", "CodigoMunicipio")], by.x = "Municipio", by.y = "CodigoMunicipio")

# Renombrar la variable Muni a Nombre_municipio
names(Ben)[names(Ben) == "Muni"] <- "Nombre_municipio"
table(Ben$Nombre_municipio, useNA = "ifany")

# Cambiar la codificación
#Ben$Nombre_municipio <- iconv(Ben$Nombre_municipio, from = "ISO-8859-1", to = "UTF-8")

# Crear una nueva variable que concatene los valores de Nombre_departamento y Nombre_municipio
Ben$dep_mun <- paste(Ben$Nombre_departamento, Ben$Nombre_municipio, sep = " - ")

# Convertir el resultado de la función table() en un data frame
tabla <- as.data.frame(table(Ben$dep_mun))


# TRANSFORMACION VARIABLE RAZON SOCIAL CONTRATISTA
Ben$RazonSocialContratista <- Ben$RazonSocialContratista %>%
  fct_collapse(
    `FONDO PARA EL FINANCIAMIENTO DEL SECTOR AGROPECUARIO - FINAGRO` = c("Fondo para el Financiamiento del Sector Agropecuario - FINAGRO"),
    `SOCIEDAD FIDUCIARIA DE DESARROLLO AGROPECUARIO S.A. - FIDUAGRARIA S.A.` = c("SOCIEDAD FIDUCIARIA DE DESARROLLO AGROPECUARIO SA FIDUAGRARIA SA"),
    `INSTITUTO COLOMBIANO DE CREDITO EDUCATIVO Y ESTUDIOS TECNICOS EN EL EXTERIOR` = c("INSTITUTO COLOMBIANO DE CREDITO EDUCATIVO Y ESTUDIOS TECNICOS EN EL EXTERIOR"),
    `BANCO DE COMERCIO EXTERIOR DE COLOMBIA S.A.` = c("Banco de Comercio Exterior de Colombia S.A."),
    `FEDERACION NACIONAL DE CACAOTEROS` = c("FEDERACION NACIONAL DE CACAOTEROS", "Federación Nacional de Cacaoteros"),
    `ASOCIACION HORTIFRUTICOLA DE COLOMBIA - ASOHOFRUCOL` = c("ASOCIACION HORTIFRUTICOLA DE COLOMBIA- ASOHOFRUCOL", "La Asociación Hortifruticola de Colombia - ASOHOFRUCOL", "ASOHOFRUCOL"),
    `COMITATO INTERNAZIONALE PER LO SVILUPPO DEI POPOLI - CISP` = c("COMITATO INTERNAZIONALE PER LO SVILUPPO DEI POPOLI - CISP", "COMITATO INTERNAZIONALE PER LO SVILUPPO DEI POPOLI- CISP"),
    `ASOCIACION DE INGENIEROS AGRONOMOS DE URABA - INAGRU` = c("Asociación de Ingenieros Agrónomos de Urabá - INAGRU", "ASOCIACION DE INGENIEROS AGRONOMOS DE URABA - INAGRU"),
    `FEDERACION NACIONAL DE PRODUCTORES DE PANELA - FEDEPANELA` = c("FEDERACION NACIONAL DE PRODUCTORES DE PANELA - FEDEPANELA"),
    `LA ASOCIACION DEPARTAMENTAL DE PRODUCTORES DE CACAO Y ESPECIES MADERABLES DEL CAQUETA - ACAMAFRUT` = c("LA ASOCIACION DEPARTAMENTAL DE PRODUCTORES DE CACAO Y ESPECIES MADERABLES DEL CAQUETA - ACAMAFRUT"),
    `ASOCIACION DE INGENIEROS AGRONOMOS DE URABA - INAGRU` = c("ASOCIACION DE INGENIEROS AGRONOMOS DE URABA - INAGRU","Asociación de Ingenieros Agrónomos de Urabá - INAGRU"),
    `BMC - BOLSA MERCANTIL DE COLOMBIA S.A.` = c("BMC - BOLSA MERCANTIL DE COLOMBIA S.A.","BMC - BOLSA MERCANTIL DE COLOMBIA S.A. - BMC EXCHANGE","Bolsa Mercantil de Colombia"),
    `CONFEDERACION COLOMBIANA DE ALGODON - CONALGODON` = c("CONFEDERACIÓN COLOMBIANA DE ALGODÓN - CONALGODÓN", "CONFEDERACION COLOMBIANA DEL ALGODÓN CONALGODON"),
    `FEDERACIÓN NACIONAL DE CULTIVADORES DE CEREALES, LEGUMINOSAS Y SOYA - FENALCE` = c("Federación Nacional de Cultivadores de Cereales, Leguminosas y Soya_FENALCE.", "FEDERACIÓN NACIONAL DE CULTIVADORES DE CEREALES, LEGUMINOSAS Y SOYA -FENALCE"),
    `SOCIEDAD FIDUCIARIA DE DESARROLLO AGROPECUARIO S.A. - FIDUAGRARIA` = c("FIDUAGRARIA S.A.", "SOCIEDAD FIDUCIARIA DE DESARROLLO AGROPECUARIO SA FIDUAGRARIA SA"),
    `FUNDACION PARA LA PROSPERIDAD DE LAS COMUNIDADES MAS VULNERABLES` = c("FUNDACION PARA LA PROSPERIDAD DE LAS COMUNIDADES MAS VULNERABLES", "FUNDACION PARA LA PROSPERDAD DE LAS COMUNIDADES MAS VULNERABLES")
  ) %>% as.character() %>%  toupper()

############ Pasar todo a mayúscula
# Bucle a través de cada columna en el dataframe
for (column in names(Ben)) {
  # Verificar si la columna es de tipo carácter
  if (is.character(Ben[[column]])) {
    # Si es así, convertir todas las palabras a mayúsculas
    Ben[[column]] <- toupper(Ben[[column]])
  }
}

### REEMPLAZAR F POR NA
Ben$NoConvenio <-Ben$NoConvenio %>%
  as.character() %>%
  ifelse(. %in% c("F"), NA, .) %>%  toupper()

### LIMITAR EDADES DE 0 A 118 AÑOS, LAS DEMAS CONVERTIRLAS A NA

Ben$Edad[Ben$Edad>118|Ben$Edad <0] <- NA

#### REMPLAZAR N/A en NA en la columna Númerodehectareascultivo
Ben$NoHectareaCultivo <-Ben$NoHectareaCultivo %>%
  as.numeric() %>%
  ifelse(. %in% c("N/A"), NA, .) %>%  toupper()

### CONVertir variables de hectareas a una variable a númerica

Ben$NoHectareaCultivo <- as.numeric(Ben$NoHectareaCultivo)
Ben$NoHectareaPredio <- as.numeric(Ben$NoHectareaPredio)
Ben$NoHectareaCultivo <- ifelse(Ben$NoHectareaCultivo > 900000, Ben$NoHectareaCultivo / 10000, Ben$NoHectareaCultivo)
Ben$NoHectareaPredio <- ifelse(Ben$NoHectareaPredio > 1000000, Ben$NoHectareaPredio / 10000, Ben$NoHectareaPredio)

####### descriptivos para la variables valor de beneficio ####### 
options(scipen=999)
summary(Ben$ValorBeneficio)

p <- seq(0.01, 0.99, 0.25)
percentiles <- quantile(Ben$ValorBeneficio, p,na.rm=TRUE)
head(percentiles)

######## HISTOGRAMA 
hist(Ben$ValorBeneficio, 
     main = "Histogram con todos los valores",
     xlab = "Values",
     ylab = "Frequency",
     col = "skyblue",
     border = "black",
     breaks = 20)

#### Variable con rango de  valor beneficios 
Ben$RangoBeneficio <- ifelse(Ben$ValorBeneficio> 5000000000,"Mas grandes beneficios","Beneficios hasta 5000 millones")

table(Ben$RangoBeneficio, useNA = "ifany")

#### Histograma valores menores a 10 millones
filtered_bene10M <- Ben[Ben$ValorBeneficio<=10000000, ]

summary(filtered_bene10M$ValorBeneficio)

p <- seq(0.01, 0.99, 0.25)
percentiles <- quantile(filtered_bene10M$ValorBeneficio, p,na.rm=TRUE)
head(percentiles)

hist(filtered_bene500M$ValorBeneficio, 
     main = "Histograma con beneficios menores a 10.000.000",
     xlab = "Values",
     ylab = "Frequency",
     col = "skyblue",
     border = "black",
     breaks = 10)


#### 
Ben$RangoBeneficio1 <- cut(Ben$ValorBeneficio,c(-1, 500000, 1000000, 3000000,5000000, 10000000, 20000000, 40000000, 100000000, 300000000, 500000000, 5000000000, Inf),
                                    labels = c( "0 a 500.000", "500.000 a 1.000.000", "1.000.000 a 3.000.000","3.000.000 a 5.000.000", "5.000.000 a 10.000.000", "10.000.000 a 20.000.000", "20.000.000 a 40.000.000",
                                                "40.000.000 a 100.000.000", "100.000.000 a 300.000.000", "300.000.000 a 500.000.000", "500.000.000 a 5000.000.000", "Mayor a 5.000.000.000"))

table_counts<-table(Ben$RangoBeneficio1, useNA = "ifany")
# Convert counts to percentages
table_percentages <- prop.table(table_counts) * 100


############################### Agrupar información de columna Observaciones #########################################

### eliminar tildes de la columna observaciones
Ben$Observaciones<- iconv(Ben$Observaciones, to = "ASCII//TRANSLIT")



# Listado de palabras clave

palabras_clave <- c("AGUA POTABLE", "AGUACATE", "AVICOLA","CACAO", "LULO", 
                    "PRODUCCION Y COMERCIALIZACION DE YUCA Y MAIZ","ESTABLECIMEINTO Y COMERCIALIZACION DE CUTIVOS ","PLATANO BUENAVISTA MUJERES CAFETERAS",	"TIENDA MOVIL TIPO CAFETERIA","CAFETERIA","CAFE INTERNET Y SERVICIO FOTOGRAFICO"," CAFETERIA ",
                    "SIEMBRA YUCA, PLATANO Y MAIZ","CAÑA","SACHA INCHI","TRUCHA","TOMATE","PANELA","PAPA","LECHE","LIMÓN","PORCINOS","PORCICOLA"," PORCICULTURA",
                    "MAIZ","PRODUCCION Y COMERCIALIZACION DE YUCA Y MAIZ","MARACUYA","MIEL","MODISTERIA","MORA","YUCA","VIVERO","VENTA DE ANIMAL DE ABASTO","UCHUVA","TURISMO","TRUCHA","TRILLADORA ARROZ","TRAPICHE","YUCA","CAFE","TRANSFORMACION DE PLATANO"," TILAPIA","BELLEZA"," RESTAURANTE","HARINA DE BANANO Y TE DE CASCARA DE CAFE")

# Función para buscar la palabra clave en el texto
buscar_palabra_clave <- function(texto, palabras_clave) {
  for (palabra in palabras_clave) {
    if (grepl(palabra, texto, ignore.case = TRUE)) {
      return(palabra)
    }
  }
  return(texto)  # Devuelve el texto original si no encuentra ninguna palabra clave
}

# Aplicar la función a la columna del data frame y crear una nueva columna
Ben$Categorias <- sapply(Ben$Observaciones, buscar_palabra_clave, palabras_clave)

table(Ben$Categorias, useNA = "ifany") 

table(Ben$Observaciones, useNA = "ifany") 


## VARIABLES A MEJORAR
# Programa (Se pueden ubicar valores comunes) (Poner en Mayúsculas) (ok)
# ProyectoInversion cambiar NULL por datos faltantes (OK)
# NoConvenio (Cambiar F y Null por NA ) (OK)
# DescripcionConvenio (pasar todo a Mayusculas) (ok)
# NitContratista (cambiar Null a NA) (ok)
# RazonSocialContratista Pasar a mayúscula y unificar terminos (Cambiar Null por NA) (OK)
# Edad (dejar solo las personas entre 10 y 100 años, el resto pasar a NA) (OK)
# Correo (Pasar a Mayúsculas, dejar solo los objetos con @) (pendiente)
# TelefonoContacto (Dejar solo los números de 7 y 10 dígitos) (pendiente)
# NombrePredio (Poner en Mayúsculas) (OK)
# MunicipioPDET (NULL y NA poner como dato faltante) (OK)
# Observaciones (Quitar NULL, poner todo en mayúscula quitar acentos, revisar si algo se puede agrupar)(en proceso)
## Númerodehectareascultivo (remover valores atipicos de hectareas, quitar valores N/A, los valores superiores a 900 mil están en mt2 comparando hectareas predio convertir/ 100.000) (OK)
## NoHectareaPredio (transforma los valores mayores a un millon que posiblemente estén digitados en mt2s y transformarlo en hectareas, hay dos valores atipicos en 6400 hectareas se dejan tal cual) (OK)
#### Valor de beneficio (revisar la distribución de los datos y tratar de eliminar valores atipicos que no tengan sentido, no se elimina ningún dato por ahora) (OK)




# DATA SET FINAL
write.csv2(Ben, "Ben.csv", fileEncoding = "ISO-8859-1") ## El que mejores resultados da, hasta ahora.
