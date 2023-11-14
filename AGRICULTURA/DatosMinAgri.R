
### Librerias 
library(dplyr)
library(readxl)
library(openxlsx)


###### Cargue de datos de productores y beneficiarios 

Beneficiarios <-  read_excel("./code/AGRICULTURA/carguemasivo_beneficiario_estado_Anoni.xlsx")
Productores <-  read_excel("./code/AGRICULTURA/productores_productor_Anoni.xlsx")


source("./code/ANALISIS_TEXTO -CuestionariosCata.R")

