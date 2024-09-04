prof_url <- "http://www.profepa.gob.mx/innovaportal/file/7635/1/accionesInspeccionfoanp.csv"
profepa<- read.csv(prof_url, encoding = "latin1")
head(profepa)


# El enconding = "latin1" es para detectar acentos en Español

mean(profepa$Inspección)  

#Mean se refiere a la media, $ significa la selección de la variables


summary(profepa)


ins <-subset(profepa, profepa$Inspección >= mean(profepa$Inspección))


# codigo subset es una función para hacer un subconjunto de un conjunto de datos

bajo <- subset(profepa, profepa$Inspección <= mean(profepa$Inspección))


ceroins <- subset(profepa,profepa$Inspección == 0)


# Descarga de datos seguro

library(repmis)
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")

conjunto$Especie <- as.factor(conjunto$Especie)
conjunto$Clase <- as.factor(conjunto$Clase)

# as.facor es el código para hacer que las caracteres se vuelvan tratamientos


summary(conjunto)


SpFH <- subset(conjunto,conjunto$Especie !="C")
tapply(SpFH$Diametro, SpFH$Especie, mean)

boxplot(SpFH$Diametro ~ SpFH$Especie,
        xlab = "Especies", 
        ylab = "Diámetro",
        col= "gray")

#Prueba de Normalidad 

shapiro.test(SpFH$Diametro)





#Prueba de homogeneidad

bartlett.test(SpFH$Diametro, SpFH$Especie)




#Prueba de t independientes

t.test(SpFH$Diametro ~ SpFH$Especie, var.equal = TRUE)



#Prueba t de una muestra 

t.test(conjunto$Diametro, mu = 16.4)


#el codigo mu = para aclarar que es una prueba de una muestra 




# Prueba para muestreas dependientes



prod <- read.csv("produccion.csv", header=T)


boxplot(prod$Kgsem ~ prod$Tiempo)


t.test(prod$Kgsem ~ prod$Tiempo, paired=T)

#El codigo paired =true se refiere a que es una prueba de t dependiente


tapply(prod$Kgsem , prod$Tiempo, mean)





