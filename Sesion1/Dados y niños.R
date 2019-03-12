# Monedas y dados

#############################
# 1. Programamos una moneda #
############################
# 1. Resultados posibles
moneda <- c("aguila", "sol")

# 2. Lanzamiento de la moneda
sample(moneda, 1)

# 3. Creamos un vector de exitos 
exitos <- c() # Este vector lo vamos a ir llenando 

# 4. Hacemos 100 lanzamientos
for (i in 1:100){
  exitos[i] <- sample(moneda, 1)
}
exitos # Mostramos la informacion

# 6. Checamos los resultados
table(exitos)

# 7. Cual es la probabilidad de obtener aguila
prob_aguila <- table(exitos)['aguila'] / 100
prob_aguila

prob_sol <- table(exitos)['sol'] / 100
prob_sol

# 8. Valor esperado de este experimento?
## Para sacar este valor esperado tenemos que recodificar la variable 
exitos2 <- ifelse(exitos == "aguila", 1, 0)
mean(exitos2)

### LOS RESULTADOS SON INDEPENDIENTES?? ##

#######################
# 2. FUNCION BINOMIAL #
######################

# Sea nuestra variable aleatoria X el numero de hijos de cierto sexo 
# Prob(H) = 0.5; Prob(M) = 0.5

# Obtenemos la probabilidad de tener 0 hijos varones con tres intentos
dbinom(x = 0, size = 3, prob = 0.5)
