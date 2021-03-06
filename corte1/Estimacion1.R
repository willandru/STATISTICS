############################################################
# Velocidad del viento: ajustando una distribuci�n Weibull #
############################################################

rm(list=ls(all=TRUE))

# Funci�n con entrada, el par�metro de forma k y
# los datos para calcular
# (1/k)+ (1/n)*sum (log(xi)) +(1/alpha)sum xi^klog(xi)=0
# donde alpha= sum xi^k.

weibull.shape <- function(k, data)
{
  numer <- colSums(outer(data, k, "^") * log(data))
  denom <- colSums(outer(data, k, "^"))
  numer/denom - 1/k - mean(log(data))
}

#-----
# Funci�n con entrada, el par�metro de forma k y
# los datos para calcular
#  k^{th} root of (1/n) sum xi^k
# n=number of data values

weibull.scale <- function(k, data)
{
  mean(data^k)^(1/k)
}

##-----
# uniroot is a built-in R function which estimates the root
# of a function.
# Provide function, any arguments needed for function,
# and a guess of values two values around root.
# Function values must be opposite signs at lower
# and upper guess.

library(resampledata)
data(Turbine)

Turbine<-read.csv("C:\\Marisol\\UJaveriana\\CursosII2021\\EstadisticaMatematica\\Diapositivas\\Turbine.csv",h=T)

#Now, we do the data specific commands
wind <- Turbine$AveSpeed
#Alternativamente, wind <- subset(Turbine, select=AveSpeed, drop=TRUE)

#Estima el par�metro de forma k
uniroot(weibull.shape, data = wind, lower = 1,upper = 5)

# Con la estimaci�n del par�metro de forma, ahora estima
# el par�metro de escala lambda

weibull.scale(3.169, wind)

# Plot histogram with density curve overlap
# Opci�n prob=TRUE escales el histograma al area 1.


par(mfrow=c(1,2))
hist(wind, main = "Distribuci�n velocidades medias del viento",
    xlab = "meters/sec", prob = TRUE)
curve(dweibull(x, 3.169, 7.661), add = TRUE, col = "blue", lwd = 2)

#dev.new()
plot.ecdf(wind,main = "ECDF datos de viento")
curve(pweibull(x,3.169,7.661), add = TRUE, col = "blue", lwd = 2)

# Para la prueba de bondad de ajuste chi-cuadrado
# Se obtienen los deciles
q <- qweibull(seq(.1, .9, by = .1), 3.169, 7.661)

# Rango del viento
range(wind)

# Abarca el rango del viento
q <- c(0, q, 14)

# Obtiene los recuentos/las frecuencias en cada subintervalo. 
# Se tienen 10 subintervalos
# El comando plot = F suprime la gr�fica y proporciona estad�sticas
hist(wind, breaks = q, plot = F)$counts

# Repite lo anterior pero guardando la salida
count <- hist(wind,breaks = q, plot = F)$counts
expected <- length(wind)*.1
# Proporciona las frecuencias esperadas

# Calcula la estad�stica de prueba chi-cuadrado
# H0:Los datos siguen una distribuci�n Weibull
chistat<-sum((count-expected)^2/expected)

# Chi-cuadrado con 10-2-1=7 grados de libertad
# grados de libertad, c-m-1, c=#clases , m=n�mero par�metos
# p-valor
pvalor <- 1-pchisq(chistat,7)
pvalor

# La distribuci�n de los datos de velocidad del viento es
# consistente con una distribuci�n Weibull con
# k estimado= 3.169 y lambda estimado= 7.661.