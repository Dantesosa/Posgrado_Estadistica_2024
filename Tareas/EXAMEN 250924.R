#Examen 250924 Dante Siboldi Sosa Álvarez 1843694



set.seed(42)
n <- 30
altura <- rnorm(n, mean=170, sd = 10) 
altura
peso <- 0.5* altura + rnorm(n, mean = 0, sd = 5)
peso

t.test(altura,peso)
# Pvalue 2.2 e-16

cor.test(altura, peso)
# correlación de 0.71


# 1 -----------------------------------------------------------------------

set.seed(25)
n <- 40 
diam_arboles <- rnorm(n, mean = 20, sd=5)
altura_arboles <- 1.5 * diam_arboles + rnorm(n, mean = 0, sd = 3)


cor.test(diam_arboles , altura_arboles) 
#cor positiva de 0.94


datos <- data.frame(diam_arboles, altura_arboles)
datos

datoslm <- lm(datos)

summary(datoslm)
# alfa = 2.97 y beta = 0.56







