# CLASE ANOVA 

sitio<- read.csv("Datos_Rascon_Anova.csv")

tapply(sitio$DAP , sitio$Paraje, mean)
tapply(sitio$DAP , sitio$Paraje, var)

tapply(sitio$EDAD , sitio$Paraje, mean)
tapply(sitio$EDAD , sitio$Paraje, var)

shapiro.test(sitio$DAP)
#no son datos normales pvalue < 0.005

bartlett.test(sitio$DAP ~ sitio$Paraje)
#tiene una homogeneidad de varianza



sitio$DAP_t<- sqrt(sitio$DAP)
#Transformación de datos 

shapiro.test(sitio$DAP_t)


mean(sitio$DAP_t^2)
# Aqui la media la elevamos al cuadrado porque es nuestro método contrario que usamos 
# para la normalización de los datos



#ahora hacemos el ANOVA 

sit.aov <- aov(sitio$DAP_t~sitio$Paraje)
#Extraemos la información 
summary(sit.aov)

#Ahora buscamos donde están las diferencias significativas

TukeyHSD(sit.aov)
# ahora de manera visual 

plot(TukeyHSD(sit.aov))

