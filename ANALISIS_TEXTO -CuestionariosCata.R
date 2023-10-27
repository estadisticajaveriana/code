library(knitr)
library(rmdformats)
library(tidytext) #Analisis de datos textuales
library(ggplot2) #Graficas
library(htmlwidgets)
library("wordcloud")
library(wordcloud2) #Graficas datos textuales
library(tidyr)
library(igraph) #grafico de redes
library(ggraph) #grafico de redes
library("udpipe") #toquenizacion y diccionarios en espanol
library(stringr)
library(dplyr)
library(readxl)
library("devtools")
library("knitr")
library(kableExtra)
library(plotly)
#A continuacion de utiliza el concepto wordStem, este, lo que hace es colocar los diferencies variantes de una palabra, como para considerarla partes de una misma palabras.
library(SnowballC)
library(tibble)
library(readxl)
library(openxlsx)
library(ggrepel)

### FUNCION

text.mining <- function(vector.text.mining, stop_words1_add = NULL, n_uni = 5, n_bi = 5) {
  
  # Convertir vector a tibble
  text_df <- tibble(line = 1:length(vector.text.mining), text = vector.text.mining)
  
  # Tokenizar el texto y quitar las palabras comunes stop words
  custom_stop_words <- bind_rows(stop_words, tibble(word = c(tm::stopwords("spanish"), stop_words1_add), lexicon = "custom"))
  text_df_L <- text_df %>%
    unnest_tokens(word, text) %>%
    anti_join(custom_stop_words)
  
  # Contar la frecuencia de las palabras
  frecuencias_palabras <- text_df_L %>%
    count(word, sort = TRUE)
  
  # Graficar las frecuencias de las palabras
  grafico_frec_palabras <- frecuencias_palabras %>%
    filter(n > n_uni) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()
  
  # Tokenizar el texto y contar pares de palabras
  bigrams <- tibble(text = vector.text.mining) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% custom_stop_words$word) %>%
    filter(!word2 %in% custom_stop_words$word) %>%
    count(word1, word2, sort = TRUE)
  
  # Graficar las frecuencias de los pares de palabras
  grafico_frec_bigrams <- bigrams %>%
    filter(n >= n_bi) %>%
    mutate(bigram = reorder(paste(word1, word2), n)) %>%
    ggplot(aes(bigram, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()
  
  # Graficar red de pares de palabras
  bigrams_no_na <- bigrams %>%
    filter(n >= n_bi) %>%
    drop_na()  # Añade esta línea para eliminar las filas con NA
  
  red_bigrams <- bigrams_no_na %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr", 
           # Ajustar el area del grafico y la distancia de repulsion entre los nodos
           # Puedes experimentar con diferentes valores para estos parametros
           # hasta encontrar una disposicion que evite la superposicion de nodos
           niter=5000) +
    geom_edge_link() +
    geom_node_point() + 
    geom_label_repel(aes(x = x, y = y, label = name), max.overlaps = Inf)
  
  # Crear objeto con los resultados
  resultados <- list(
    tablas = list(frecuencias = frecuencias_palabras, frecuencias_bigrams = bigrams),
    graficos = list(frecuencias = grafico_frec_palabras, frecuencias_bigrams = grafico_frec_bigrams, red_bigrams = red_bigrams)
  )
  
  return(resultados)
}

######################################################################################################
######################################################################################################

#UNIDAD: CONTEXTO
#Cargar base de datos de cada unidad de analisis:
Unidad_contexto <-  read_excel("./code/CATATUMBO/Percepción PDET en el Catatumbo(1-55).xlsx")

#Instancia de la funcion con el vector de datos a analizar
SubBD=Unidad_contexto


## PARA FILTRAR (SOLO SI SE NECESITA)
#SubBD <- SubBD %>% filter(NOMBRE=="EDWIN JOSE BESAILE FAYAD")

#Función  Para quitar números antes de hacer la nube de palabras y exportar el conteo por cada pregunta
Num<-as.character(1:9999)

# Define the variables
variables <- c("¿Que obstáculos impiden el desarrollo de este pilar? (Obstáculo 1)",
               "¿Que obstáculos impiden el desarrollo de este pilar? (Obstáculo 2)",
               "¿Que obstáculos impiden el desarrollo de este pilar? (Obstáculo 3)",
               "¿Cuáles son las principales razones por las que es más o menos difícil resolver los obstáculos mencionados anteriormente?  \r\n\r\n",
               "¿Cómo resolvería los obstáculos del pilar que mencionó anteriormente? ( Solución 1)\r\n\r\n",
               "¿Cómo resolvería los obstáculos del pilar que mencionó anteriormente? ( Solución 2)\r\n\r\n",
               "¿Cómo resolvería los obstáculos del pilar que mencionó anteriormente? ( Solución 3)\r\n\r\n",
               "¿Cuáles son los principales beneficios obtenidos si se superaran los obstáculos anteriores? (Beneficio 1)\r\n\r\n",
               "¿Cuáles son los principales beneficios obtenidos si se superaran los obstáculos anteriores? (Beneficio 2)\r\n\r\n",
               "¿Cuáles son los principales beneficios obtenidos si se superaran los obstáculos anteriores? (Beneficio 3)\r\n\r\n")

# Define a function to perform text mining and write results to Excel
evaluate_and_write_to_excel <- function(variable_name, file_name) {
  text_mining_result = text.mining(vector.text.mining = SubBD[[variable_name]] %>% na.omit(),
                                   stop_words1_add = c(Num, "00", "01", "02", "03", "04", "05", "06", "07", "08", "09"),
                                   n_uni = 5,
                                   n_bi = 3)
 
  write.xlsx(text_mining_result$tablas$frecuencias, file_name)
}

# Loop through the variables and apply the function
for (i in 1:length(variables)) {
  file_name <- paste0("./code/RESULTADOS/CUESTIONARIO/Categoria", i, ".xlsx")
  evaluate_and_write_to_excel(variables[i], file_name)
}


##### función para visualizar el conteo de palabras individuales por cada variable analizada

evaluate_function1 <- function(variable_name) {
  text_mining_result = text.mining(vector.text.mining = SubBD[[variable_name]] %>% na.omit(),
                                   stop_words1_add = c(Num, "00", "01", "02", "03", "04", "05", "06", "07", "08", "09"),
                                   n_uni = 5,
                                   n_bi = 3)
  
  individ_word<-text_mining_result$tablas$frecuencias %>% 
    head(20) %>% 
    kable(align = "c") %>% 
    kable_styling(full_width = F)
  
  # Imprimir la tabla de palabras individuales
  print(individ_word)

}

#evaluate_function(variables[1])
#evaluate_function(variables[2])
#evaluate_function(variables[3])
#evaluate_function(variables[4])
#evaluate_function(variables[5])
#evaluate_function(variables[6])
#evaluate_function(variables[7])
#evaluate_function(variables[8])
#evaluate_function(variables[9])
#evaluate_function(variables[10])

#####  función para visualizar el conteo de  pares de palabras por cada variable analizada

evaluate_function2 <- function(variable_name) {
  text_mining_result = text.mining(vector.text.mining = SubBD[[variable_name]] %>% na.omit(),
                                   stop_words1_add = c(Num, "00", "01", "02", "03", "04", "05", "06", "07", "08", "09"),
                                   n_uni = 5,
                                   n_bi = 3)
  
  #Tabla de pares de palabras
  parespalabras<-text_mining_result$tablas$frecuencias_bigrams %>% 
    head(20) %>% 
    kable(align = "c") %>% 
    kable_styling(full_width = F)
  
  # Imprimir la tabla de palabras individuales
  print(parespalabras)
  
}

#evaluate_function(variables[1])
#evaluate_function(variables[2])
#evaluate_function(variables[3])
#evaluate_function(variables[4])
#evaluate_function(variables[5])
#evaluate_function(variables[6])
#evaluate_function(variables[7])
#evaluate_function(variables[8])
#evaluate_function(variables[9])
#evaluate_function(variables[10])

#####  función para visualizar la nube de palabras  por cada variable analizada

evaluate_function3 <- function(variable_name) {
  text_mining_result = text.mining(vector.text.mining = SubBD[[variable_name]] %>% na.omit(),
                                   stop_words1_add = c(Num, "00", "01", "02", "03", "04", "05", "06", "07", "08", "09"),
                                   n_uni = 5,
                                   n_bi = 3)
  
  #Tabla de pares de palabras
  x11()
  nubepal<-wordcloud(words =  text_mining_result$tablas$frecuencias$word, freq =  text_mining_result$tablas$frecuencias$n, min.freq = 2,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=RColorBrewer::brewer.pal(8, "Dark2"))
  
  # Imprimir la tabla de palabras individuales
  print(nubepal)
  
}

#evaluate_function3(variables[1])
#evaluate_function3(variables[2])
#evaluate_function3(variables[3])
#evaluate_function3(variables[4])
#evaluate_function3(variables[5])
#evaluate_function3(variables[6])
#evaluate_function3(variables[7])
#evaluate_function3(variables[8])
#evaluate_function3(variables[9])
#evaluate_function3(variables[10])

### función para visualizar las redes de palabras

evaluate_function4 <- function(variable_name) {
  text_mining_result = text.mining(vector.text.mining = SubBD[[variable_name]] %>% na.omit(),
                                   stop_words1_add = c(Num, "00", "01", "02", "03", "04", "05", "06", "07", "08", "09"),
                                   n_uni = 5,
                                   n_bi = 3)
  
  #Tabla de pares de palabras
  #Grafico de pares de palabras
  x11()
  redes<-text_mining_result$graficos$red_bigrams
  
  # Imprimir la tabla de palabras individuales
  print(redes)
  
}

#evaluate_function4(variables[1])
#evaluate_function4(variables[2])
#evaluate_function4(variables[3])
#evaluate_function4(variables[4])
#evaluate_function4(variables[5])
#evaluate_function4(variables[6])
#evaluate_function4(variables[7])
#evaluate_function4(variables[8])
#evaluate_function4(variables[9])
#evaluate_function4(variables[10])
