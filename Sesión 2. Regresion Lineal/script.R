# Libreria para abrir los datos
library(readxl)
library(ggplot2)

# Abrimos los datos
datosPresion <- readxl::read_xls("datos/bloodPresureData.xls")

#############
# REGRESIÓN # 
#############

# Paso 1. Guardamos la fórmula
fmla <- sistolicBloodPressure ~ ageInYears + weightInPounds

# Paso 2. Corremos la regresión 
modelo_regresion <- lm(fmla, datosPresion)
modelo_regresion # Imprimimos
summary(modelo_regresion) # Mostramos el sumario del modelo

# Paso 3. Predecimos
datosPresion$prediccion <- predict(modelo_regresion)

# Paso 4. Graficamos
ggplot(datosPresion, aes(x = prediccion, y = sistolicBloodPressure)) + 
  geom_point() + 
  geom_abline(color = "blue")

# Grafica mas bonita
ggplot(datosPresion, aes(x = prediccion, y = sistolicBloodPressure)) + 
  theme_bw() + # Fondo blanco con rayas grises
  theme(plot.title = element_text(family = "EBGaramond",  size = 20, hjust = 0.5), 
        plot.subtitle = element_text(family = "EBGaramond", size = 15, hjust = 0.5), 
        axis.title = element_text(family = "EBGaramond", size = 15, hjust = 0.5),
        axis.text = element_text(family = "EBGaramond", size = 15, hjust = 0.5)
        ) +
  geom_point(colour = "red") + 
  geom_abline(color = "blue") + 
  labs(title = "Regresión Lineal. Presión sanguínea",
       subtitle = "Variables explicativas: Edad (años) y  Peso (Libras)",
       x = "Predicción del modelo", 
       y = "Presión sanguínea sistólica") 

