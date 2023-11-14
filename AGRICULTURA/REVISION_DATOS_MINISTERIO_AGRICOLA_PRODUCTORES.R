################################################################################
################################################################################
############################### PRODUCTORES ####################################

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
setwd("~/COMPU_IEI/1_JAVERIANA_IEI/MINISTERIO DE AGRICULTURA")

################################################################################
### CARGUE DE DATOS

Productores  <- read_excel("Datos productores beneficiarios/productores_productor_Anoni.xlsx", 
                              col_types = c("numeric", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "date", "date", "text", "text", "text", 
                                            "text", "text", "text", "text", "text"))
Productores <- as.data.frame(Productores)

Dep <- read_delim("Datos productores beneficiarios/Dep.csv", 
                  delim = ";", escape_double = FALSE, col_names = FALSE, 
                  col_types = cols(X1 = col_skip()), locale = locale(encoding = "ISO-8859-1"), 
                  trim_ws = TRUE)
Dep <- as.data.frame(Dep)

Mun <- read_delim("Datos productores beneficiarios/Mun.csv", 
                  delim = ";", escape_double = FALSE, col_types = cols(Mun1 = col_skip(), 
                                                                       CodigoMunicipio = col_character(), 
                                                                       CodMun = col_double(), Mun2 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1"), 
                  trim_ws = TRUE)
Mun <- as.data.frame(Mun)

################################################################################
### HALLAZGOS DE REVISION DE DATOS PRODUCTORES

names(Productores)

#####

unique(Productores$Id)
unique(Productores$UsuarioId) 
unique(Productores$TipoIdentificacionId)
unique(Productores$Documento)
unique(Productores$SexoId)
unique(Productores$EstadoCivilId) 
unique(Productores$CabezaFamilia)
unique(Productores$ParentescoId) 
unique(Productores$GrupoEtnicoId) 
unique(Productores$Registro) 
unique(Productores$EstadoId)
unique(Productores$TipoDocumento) 
unique(Productores$Valido) 
unique(Productores$FechaCreacion) 
unique(Productores$FechaModificacion) 
unique(Productores$FechaExpedicion)
unique(Productores$FechaNacimiento) 
unique(Productores$PerteneceFuerzasArmadas) 
unique(Productores$IdFuerzaArmada) 
unique(Productores$EnvioDeCorreos) 
unique(Productores$CarnetProductor) 
unique(Productores$carnetproductor.1) 

#####

table(Productores$Id)
table(Productores$UsuarioId) 
table(Productores$TipoIdentificacionId)
table(Productores$Documento)
table(Productores$SexoId)
table(Productores$EstadoCivilId) 
table(Productores$CabezaFamilia)
table(Productores$ParentescoId) 
table(Productores$GrupoEtnicoId) 
table(Productores$Registro) 
table(Productores$EstadoId)
table(Productores$TipoDocumento) 
table(Productores$Valido) 
table(Productores$FechaCreacion) 
table(Productores$FechaModificacion) 
table(Productores$FechaExpedicion)
table(Productores$FechaNacimiento) 
table(Productores$PerteneceFuerzasArmadas) 
table(Productores$IdFuerzaArmada) 
table(Productores$EnvioDeCorreos) 
table(Productores$CarnetProductor) 
table(Productores$carnetproductor.1) 

#####

names(Productores); class (Productores)

Pro <- drop_na(Productores, Id)

################################################################################

################################################################################
## FUNCIONES

check_duplicates <- function(dataframe, column_name) {
  if(any(duplicated(dataframe[[column_name]]))) {
    return(paste("La variable", column_name, "tiene duplicados"))
  } else {
    return(paste("La variable", column_name, "no tiene duplicados"))
  }
}


check_na <- function(dataframe, column_name) {
  if(any(is.na(dataframe[[column_name]]))) {
    return(paste("La variable", column_name, "contiene valores NA"))
  } else {
    return(paste("La variable", column_name, "no contiene valores NA"))
  }
}

check_duplicates(Productores,"Id") ## Para testear duplicados
check_na(Productores,"Id") #### Para comprobar si la variable contiene NA o no

##################################################################################
### VARIABLES A ELIMINAR

#Productores$UsuarioId
#Productores$TipoIdentificacionId
#Productores$Documento
#Productores$CarnetProductor
#Productores$imagen
#Productores$carnetproductor.1
#Valido
#EstadoId
#Registro

# Identificar las columnas a eliminar
columnas_a_eliminar <- c("UsuarioId", "TipoIdentificacionId", "Documento", "CargueId", "CarnetProductor", "imagen", "carnetproductor.1", "Valido", "EstadoId", "Registro")

# Eliminar las columnas
Pro <- Productores[, !colnames(Productores) %in% columnas_a_eliminar]

# Pasar todo a mayúscula
# Bucle a través de cada columna en el dataframe
for (column in names(Pro)) {
  # Verificar si la columna es de tipo carácter
  if (is.character(Pro[[column]])) {
    # Si es así, convertir todas las palabras a mayúsculas
    Pro[[column]] <- toupper(Pro[[column]])
  }
}

## Transformación de fechas a caracter
Pro$FechaCreacion <- as.character(Pro$FechaCreacion)
Pro$FechaModificacion <- as.character(Pro$FechaModificacion)

# Reemplaza 'NULL' con NA en todas las columnas
Pro[] <- lapply(Pro, function(x) ifelse(x == "NULL", NA, x))

# Reemplaza 'NULL' con NA en todas las columnas
Pro[] <- lapply(Pro, function(x) ifelse(x == "TRUE", "SI", x))
Pro[] <- lapply(Pro, function(x) ifelse(x == "FALSE", "NO", x))


# DATA SET FINAL
write.csv2(Pro, "Pro.csv", fileEncoding = "ISO-8859-1") ## El que mejores resultados da, hasta ahora.


