# SESION 6. PRUEBA DE HETEROSCEDASTICIDAD

# Probamos heteroscedasticidad
library(tidyverse)
library(lmtest)
library(robustbase)

# Leemos datos
# 1. Abrimos la base
press <- readxl::read_xls("bloodPressure.xls")
press <- read_csv("bloodPressure.csv")

# 2. Exploramos los datos
head(press)
lapply(press, class)

# 3. Definimos el modelo de regresion 
fmla1 <- pressure ~ age + weight
model5 <- lm(fmla1, data = press)
summary(model5)

# Checamos si se presenta heteroscedasticidad
bptest(model5) # Se acepta la hipotesis nula; Por lo tanto no se presenta heteroscedasticidad


############################
# Datos de Ingreso / gasto #
############################

Ing <- read.csv(file = "DatosIngreso.csv", fileEncoding = "UTF-8")
names(Ing)

# Modelo de regresion
fmla <- Gasto ~ ingreso
modelo <- lm(fmla, Ing)

summary(modelo)

# Graficamos la informacion y los puntos
ing$predict <- predict(modelo)
# Codigo de la grafica
ggplot(ing, aes(x = ingreso, y = Gasto)) + 
  geom_point() + 
  geom_line(aes(x = ing$ingreso, y = ing$predict), colour = "red")

# Hacemos el test Breusch-Pagan
bptest(modelo) # Se rechaza H0, por lo tanto hay Heteroscedasticidad!!!

# Utilizamos un m'etodo robusto
modeloRobusto <- lmrob(fmla, Ing)
summary(modeloRobusto)

