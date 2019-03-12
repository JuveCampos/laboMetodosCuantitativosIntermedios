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

#################################
# 2. C A S O    D E L   D A D O #  
################################

# Programamos un dado!
dado <- 1:6

#lanzar la moenda
sample(dado, 1)

# vector de exitos
exitos <- c()

# Hacemos 100 lanzamientos
for(i in 1:1000){
  exitos[i] <- sample(dado, 1)
}
exitos

table(exitos)

# Probabilidades
table(exitos)[1]/1000
table(exitos)[2]/1000
table(exitos)[3]/1000
table(exitos)[4]/1000
table(exitos)[5]/1000
table(exitos)[6]/1000

# Exitos 
exitos2 <- ifelse(exitos =="aguila", 1, 0)
exitos2

# Media de exitos == Valor Esperado
mean(exitos2)


#######################
# 3. FUNCION BINOMIAL #
######################

# Sea nuestra variable aleatoria X el numero de hijos de cierto sexo 
# Prob(H) = 0.5; Prob(M) = 0.5

# Obtenemos la probabilidad de tener 0 hijos varones con tres intentos
dbinom(x = 0, size = 3, prob = 0.5)
