#DANTE SIBOLDI SOSA ÁLVAREZ 1843694
#MAESTRÍA EN RESTAURACIÓN ECOLÓGICA
#UNIDAD DE APRENDIZAJE
#EVIDENICA 1

# Importación de datos ----------------------------------------------------

conjunto <- read.csv("cuadro1.csv", header = T)
head(conjunto)
mean(conjunto$Altura)

# Selección de datos a partir de un subconjunto ---------------------------

# VARIABLE ALTURA
h.media <- subset(conjunto, conjunto$Altura <= mean(conjunto$Altura))
head(h.media)
# Tenemos un total de 24 individuos que presentan una altura igual o inferior
# a la media, que en el caso de la altura es de 13.94 m


h.16 <- subset(conjunto, conjunto$Altura < 16.5 )
head(h.16)
# En este caso tenemos un total de 39 observaciones de individuos con alturas
# inferiores a 16.5 m


# VARIABLE VECINO 

vecinos.3 <- subset(conjunto, conjunto$Vecinos <= 3)
head(vecinos.3)
# Tenemos un total de 26 observaciones donde los individuos un número de vecinos
# inferior o igual a 3

vecinos.4 <- subset(conjunto, conjunto$Vecinos > 4)
head(vecinos.4)
# En este ejemplo tenemos un total de 11 observaciones de individuos 
# que presentan más de 4 vecinos


# VARIABLE DIÁMETRO

dbh.media <- subset(conjunto, conjunto$Diametro < mean(conjunto$Diametro))
mean(conjunto$Diametro)
#tenemos un total de 25 individuos que tienen un diámetro inferior de la media, 
# que en este caso el diámetro medio es de 15.79 cm 


dbh.16 <- subset(conjunto, conjunto$Diametro > 16)
# En este ejemplo presentamos 24 observaciones de individuos con diámetros
# diámetros superiores a 16 cm


# VARIABLE ESPECIE 
cedro <- subset(conjunto, conjunto$Especie == "C")
# El subconjunto nos indica que tenemos un total de 22 observaciones o 
# individuos de cedro rojo

tsuga.douglasia <- subset(conjunto, conjunto$Especie != "C")
# FInalmente tenemos 28 observaciones de las especies Tsuga y Douglas 




# Determinación de observaciones ------------------------------------------


dbh16.9 <- subset(conjunto, conjunto$Diametro <= 16.9)
#Para este caso tenemos 31 observaciones de individuos con diámetros inferior o 
# o igual a 16.9 cm

h.18 <- subset(conjunto, conjunto$Altura > 18.5)
# Finalmente tenemos 2 observaciones de dos individuos cuya altura superan
# 18.5 m




# Visualización de datos --------------------------------------------------
# Mediante histogramas de frecuencias en función de la variable altura

hist(conjunto$Altura, xlab = "Altura", ylab = "Frecuencia", 
     main = "Histograma de frecuencias de alturas",
     xlim = c(5,25), ylim = c(0, 15), col = "yellow")


hist(h.media$Altura, xlab = "Altura", ylab = "Frecuencia", 
     main = "Histograma de frecuencias de altura media",
     xlim = c(6,17), ylim = c(0, 10), col = "blue" )


hist(h.16$Altura, xlab = "Altura", ylab = "Frecuencia", 
     main = "Alturas inferiores a 16.5 m",
     xlim = c(7,18), ylim = c(0, 12), col = "brown" )



# Histogramas de frecuencia en función de la variable Vecinos

hist(conjunto$Vecinos,xlab = "Vecinos", ylab = "Frecuencia", 
     main = "Vecinos cercanos",
     xlim = c(0,6), ylim = c(0, 15), col = "brown")

hist(vecinos.3$Vecinos, xlab = "Vecinos", ylab = "Frecuencia", 
     main = "Número de vecinos igual o inferior a 3",
     xlim = c(0,4), ylim = c(0, 12), col = "red")

hist(vecinos.4$Vecinos, xlab = "Vecinos", ylab = "Frecuencia", 
     main = "Número de vecinos mayor a 4",
     xlim = c(5,6), ylim = c(0, 7), col = "pink")


# Histogramas de frecuencia en función de la variable diámetro

hist(conjunto$Diametro, xlab = "Diámetro", ylab = "Frecuencia", 
     main = "Categorías diamétricas",
     xlim = c(5,25), ylim = c(0, 15), col = "green")

hist(dbh.media$Diametro, xlab = "Diámetro", ylab = "Frecuencia", 
     main = "Categorías diamétricas menores a la media",
     xlim = c(5,18), ylim = c(0, 8), col = "blue" )

hist(dbh.16$Diametro, xlab = "Diámetro", ylab = "Frecuencia", 
     main = "Diámetros inferiores a 16.5 cm",
     xlim = c(15,25), ylim = c(0, 10), col = "red")



# Estadísticas básicas ----------------------------------------------------

# Media y desviación estandar para variales altura

mean(conjunto$Altura)
# La media de la variable altura es de 13.94 m
sd(conjunto$Altura)
# con una desviación estandar de 2.90



mean(h.media$Altura)
# En este caso, la media de la variable altura media (<= 16.5 m) es de 11.53 m
sd(h.media$Altura)
# Con una desviación estandar de 1.746


mean(h.16$Altura)
# La media de la variable altura para individuos inferiores a 16.5 m de altura
# es de 12.85 m
sd(h.16$Altura)
# Con una desviación estandar de 2.21 


#Media y desviación estandar para variable vecinos 


mean(conjunto$Vecinos)
# La media de la variable vecinos es de 3.34 vecinos por individuos
sd(conjunto$Vecinos)
# Con una variación estandar de 1.59

mean(vecinos.3$Vecinos)
# La media de individuos del subconjunto con 3 vecinos o menos es de 2.11
sd(vecinos.3$Vecinos)
# Con una desviación estandar de 1.07

mean(vecinos.4$Vecinos)
# La media de individuos con un numéro de vecinos mayor a 4 es de 5.45 vecinos
sd(vecinos.4$Vecinos)
# Con una desviación estandar de 0.52


# Media y desviación estandar de la variable diámetro


mean(conjunto$Diametro)
# La media de la variable diámetro corresponde a 15.79 cm 
sd(conjunto$Diametro)
# Con una variación estandar de 3.22 


mean(dbh.media$Diametro)
# El diámetro medio de los datos inferiores a la media (<15.79 cm) es de 13.25 cm
sd(dbh.media$Diametro)
# Con una desviación estandar de 2.09


mean(dbh.16$Diametro)
# La media de los datos mayores a 16 cm es de 18.43
sd(dbh.16$Diametro)
# Con una desviación estandar de 1.81








