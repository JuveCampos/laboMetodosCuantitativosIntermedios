# Librerias y funciones
library(sjmisc)
library(sjlabelled)
library(tidyverse)
library(readr)
niveles <- function(x) levels(as.factor(x))

# Leemos datos #
datos <- read.csv("ingresosEnigh2014.csv", encoding = "UTF-8")

# Exploramos datos
names(datos)
head(datos)
lapply(datos, class)

# Hacemos reconfiguracion 
# Para ello, tenemos que abrir el diccionario de datos

# Variable 1! Sexo
niveles(datos$sexo) # Tiene variables 1 y 2... que significan? 
# Viendo el diccionario vemos que 1 significa Hombre y 2 Significa Mujer
# Recodificamos entonces 

datos2 <- datos %>% 
  mutate(sexo = sjmisc::rec(sexo, rec = "1=Hombre; 2=Mujer") %>% as.factor(), 
         sexo2 = sjmisc::rec(sexo, rec = "1=1 [HOMBRE]; 2=2 [MUJER]")
         )

# Varible 2! edad
# Haremos una reclasificacion con grupos de edad...
# De 0 a 12 anios diremos que es un infante
# De 13 a 17 que es un adolescente
# De 18 a 27 que es un adulto joven
# De 28 a 60 que es un adulto maduro
# De 60 en adelante que es un adulto mayor
niveles(datos2$edad)

datos2 <- datos %>% 
  mutate(grupoEdad = sjmisc::rec(edad, rec = "0:12=Infante; 13:17=Adolescente; 
                                 18:27=Adulto Joven; 28:60 = Adulto Maduro; 
                                 else = Adulto Mayor", as.num = F))
#class(datos2$grupoEdad)

# Variable 3! edo_conyug

# SIGNIFICADO
# 1 Vive con su pareja o en unión libre 
# 2 Está casado(a)
# 3 Está separado(a)
# 4 Está divorciado(a)
# 5 Es viudo(a) 
# 6 Está soltero(a)

niveles(datos2$edo_conyug)

datos2 <- datos %>% 
  mutate(Edo_civil = rec(datos$edo_conyug, rec = "1=1 [Vive con su pareja]; 
                         2 = 2 [Casado]; 
                         3 = 3 [Separado];
                         4 = 4 [Divorciado]; 
                         5 = 5 [Viudo]; 
                         6 = 6 [Soltero]"))

# class(datos2$Edo_civil)
sjmisc::frq(datos2$Edo_civil)

# Ultima Variable: nivelaprob
# Del diccionario #
# 0 Ninguno
# 1 Preescolar
# 2 Primaria
# 3 Secundaria
# 4 Preparatoria o bachillerato 
# 5 Normal
# 6 Carrera técnica o comercial 
# 7 Profesional
# 8 Maestría
# 9 Doctorado

# Niveles! 
niveles(datos$nivelaprob)

datos2 <- datos %>% 
  mutate(escolaridad = rec(gradoaprob, rec = "0=1 [Ninguno]; 
                            1=1 [Preescolar];
                            2=2 [Primaria];
                            3=3 [Secundaria];
                            4=4 [Preparatoria o bachillerato]; 
                            5=5 [Normal];
                            6=6 [Carrera técnica o comercial]; 
                            7=7 [Profesional];
                            8=8 [Maestría];
                            9=9 [Doctorado]", as.num = F))
# Checamos la clase
class(datos2$escolaridad)
niveles(datos2$escolaridad)
frq(datos2$escolaridad)
table(is.na(datos2$escolaridad))

# Eliminar NA

# Hacer una regresion con esto 
fmla <- ing_tri ~ gradoaprob
lm(fmla, datos2)
