# LABORATORIO 2 #
################################
# 1. CALCULO DE PROBABILIDADES #
###############################
library(dplyr)

# Creamos muestra

# Altura promedio estado A
popA <- rnorm(1000, mean = 1.8, sd = 0.3) %>% round(3)
popA <- cbind(popA, "A") %>% as.data.frame()
names(popA) <- c("Altura", "Estado")

# Altura promedio estado B
popB <- rnorm(1000, mean = 1.6, sd = 0.5) %>% round(3)
popB <- cbind(popB, "B") %>% as.data.frame()
names(popB) <- c("Altura", "Estado")

# Altura promedio estado C
popC <- rnorm(1000, mean = 1.7, sd = 0.4) %>% round(3)
popC <- cbind(popC, "C") %>% as.data.frame()
names(popC) <- c("Altura", "Estado")

# Altura promedio estado D
popD <- rnorm(1000, mean = 1.5, sd = 0.3) %>% round(3)
popD <- cbind(popD, "D") %>% as.data.frame()
names(popD) <- c("Altura", "Estado")

# Poblacion Final
POP <- rbind(popA, popB, popC, popD) %>% 
  mutate(Altura = as.numeric(as.character(Altura)))

# Muestra Rara
pop <- rbind(popA[1:sample(1000)[1],], popB[1:sample(1000)[1],], popC[1:sample(1000)[1],], popD[1:sample(1000)[1],])
pop <- pop[sample(1:nrow(pop)),]
rownames(pop) <- seq(1,nrow(pop), by = 1)
pop$Altura <- as.numeric(as.character(pop$Altura))

write.csv(POP, "Poblacion.csv")
write.csv(pop, "Muestra.csv")

# Probabilidad de seleccionar uno del estado A al azar!
table(pop$Estado)["A"] / nrow(pop)
table(pop$Estado)["B"] / nrow(pop)
table(pop$Estado)["C"] / nrow(pop)
table(pop$Estado)["D"] / nrow(pop)

# Valor Esperado de las alturas de la poblacion A, B, C, D
pop %>%
  group_by(Estado) %>%
  summarize(media = mean(Altura))

# Visualizamos el TLC
n <- 100
a <- POP[sample(1:4000)[1:n],]
a %>%
  group_by(Estado) %>%
  summarise(media = mean(Altura), 
            desv.est = sd(Altura), 
            error.estandar = sd(Altura)/sqrt(n)) 

almacen_de_medias <- data.frame()
names(almacen_de_medias) <- c("A", "B", "C", "D")
  #matrix(ncol = 4, nrow = 1000)
for (i in 1:10000){
  n <- 100
  a <- POP[sample(1:4000)[1:n],]
  estadisticas <- a %>%
    group_by(Estado) %>%
    summarise(media = mean(Altura), 
              desv.est = sd(Altura), 
              error.estandar = sd(Altura)/sqrt(n)) 
  
  almacen_de_medias <- rbind(almacen_de_medias, estadisticas$media)
}
almacen_de_medias

# Medias
mean(almacen_de_medias$A)
mean(almacen_de_medias$B)
mean(almacen_de_medias$C)
mean(almacen_de_medias$D)

# Ploteamos las distribuciones
d <- density(almacen_de_medias$A)
plot(d)
