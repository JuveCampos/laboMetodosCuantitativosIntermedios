# Librerias
library(readr)
library(dplyr)

# Abrimos Archivo de Datos de personas
root <- "enigh_ncv_2014_csv"
subroot <- "viviendas_enigh2014ncv"
df <- read_csv("/Users/admin/Desktop/Proyectos/Laboratorio\ Salvador/Sesion\ 4.\ Encuesta/enigh_ncv_2014_csv/poblacion_enigh2014ncv/conjunto_de_datos/poblacion.csv") %>% 
  mutate(folioviv = as.character(folioviv), 
         foliohog = as.character(foliohog), 
         numren = as.character(numren)) %>% 
  mutate(id = paste0(folioviv, foliohog, numren))

# Datos de Ingreso 
df_ingreso <- read_csv("/Users/admin/Desktop/Proyectos/Laboratorio\ Salvador/Sesion\ 4.\ Encuesta/enigh_ncv_2014_csv/ingresos_enigh2014ncv/conjunto_de_datos/ingresos.csv")
df_ingreso <- df_ingreso %>% 
  mutate(folioviv = as.character(folioviv), 
         foliohog = as.character(foliohog), 
         numren = as.character(numren)) %>% 
  mutate(id = paste0(folioviv, foliohog, numren)) %>% 
  select(id, ing_tri)

# Generamos Base de Datos para clase 
df <- df %>% 
  select(id, folioviv, foliohog, numren, parentesco, 
         sexo, edad, edo_conyug, gradoaprob, nivelaprob) 

# Creamos la base de datos
datos <- merge(df, df_ingreso, by = "id")

# Exploramos la base 
head(datos)

# Guardamos la base
write.csv(datos, "ingresosEnigh2014.csv", fileEncoding = "UTF-8")
