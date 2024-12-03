# Correlación de datos 

library(repmis)
edad <- source_data("https://www.dropbox.com/s/nxoijhgmutuho0s/datos_control_Rascon.csv?dl=1")

summary(edad)


plot(edad$DAP, edad$EDAD)

plot(edad$EDAD, edad$DAP, pch = 25, xlab = "Edad", ylab= "Diámetro (CM)", col = "Blue")


cor.test (edad$DAP, edad$EDAD)
#Correlación es significativa porque el valor de p es menor a 0.05, si es superior no es significativa
# Cuando es significativo podemos hacer una regresión lineal




edad.lm <- lm(edad$EDAD ~ edad$DAP)
#Solo obtener el intercepto (alfa) y beta

edad.lm
 # Para obtener la significacia aplico summary
summary(edad.lm)

plot(edad$DAP, edad$EDAD, pch = 25, xlab = "Diámetro", ylab= "EDAD", col = "Blue")
abline(edad.lm, col = "red")
text(10,120, "Y = -8.4 * 2.43 (X)")

#plot es para poner la distribución de los datos y abline es para agregar la línea central 
# Text es para agregar la fórmula de regresión 




edad.lm$coefficients
edad.lm$residuals
edad$res <- edad.lm$residuals
edad$edprim <- edad.lm$fitted.values
edad$com.res <- edad$EDAD - edad$edprim


# Estimar residuales 


sum(edad$res)^2
sum(edad$res^2)/58


# Estimar la edd (prima) para los valores de DAP: 15, 30, 45, 47 
valores <- c(15,30,45,47)
prima <- -8.4 + 2.4 *(valores)
prima



