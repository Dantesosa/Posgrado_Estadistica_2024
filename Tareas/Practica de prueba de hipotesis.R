#Ejercicio datos met

met.url<- "https://raw.githubusercontent.com/mgtagle/Met_Est_2024/main/Datos_Madera_MET.csv"

madera <- read.csv(met.url, header = T)


# Establecimiento de hipótesis --------------------------------------------
# H0 no existe diferencia entre la media del peso entre especies 
# H1 existe diferencia entre la media del peso entre especies 
  
gavia<- subset(madera, madera$Especie =="Gavia")
barreta<- subset(madera, madera$Especie =="Barreta")



# Inspección gráfica  -----------------------------------------------------

boxplot(madera$Peso_g~madera$Especie)

# Estadística descriptiva -------------------------------------------------
tapply(madera$Peso_g, madera$Especie, mean)
tapply(madera$Peso_g, madera$Especie, var)
## tapply es para comparar dos niveles de factor 
#La desviación estandar de la barreta es casi el doble que de la gavia

# Niveles de factor -------------------------------------------------------

#Existen dos niveles de factor

# Normalidad --------------------------------------------------------------

shapiro.test(madera$Peso_g)
hist(madera$Peso_g)

# p-value es de 0.1548, como es mayor a 0.05 podemos asumir que son datos paramétricos
# es decir, presenta una distribución normal



# Homogeneidad ------------------------------------------------------------

bartlett.test(madera$Peso_g,madera$Especie)
# pvalue = 0.0005, es decir que la homogeneidad de las varianzas no son iguales 

madera$peso_t<- log(madera$Peso_g+1)


# Prueba t  ---------------------------------------------------------------

t.test(barreta$Peso_g , gavia$Peso_g, var.equal = F)

# var.equal = F es porque tienen mucha varianza, como lo muestra la prueba de bartlet
#por eso se =F por false

#pvalue = 0.001469, es inferior a nuestro nivel de confianza, esto quiere decir que
# no existen diferencias entre la media del peso de la madera entre especies



# Aceptación de hipótesis -------------------------------------------------

#Aceptamos la H1 ya que existe diferencia entre el peso de la madera entre especies





