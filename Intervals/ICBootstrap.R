rm(list=ls(all=TRUE))

##############################
#				     #	
# Ejemplo distribuci?n Gamma #
#				     #	
##############################

# Distribuci?n Gamma
par(mfrow=c(2,2)) 

# Distribuci?n de una Gamma (1,1/2)
x <- seq(0,8,length=16)
x
fx<- dgamma(x,1,rate=1/2)
plot(x,fx,type="l",main="Funci?n de densidad Gamma(1,1/2)",xlab="X",ylab="Densidad",col="blue")
abline(v=2,lty=2)
#linex<-rep(2,11)
#linex<-c(2,2,2,2,2,2,2,2,2,2,2)
#liney<-seq(0,0.19, length=11)
#lines(linex,liney,lty=2)

# Muestra de tama?o 16 de una Gamma (1,1/2)
my.sample <- rgamma(16, 1, 1/2)
fxb<-dgamma(my.sample,1,1/2)
hist(my.sample, main="Muestra n=16",xlab="x", ylab="Frecuencia")
abline(v=mean(my.sample),lty=2)

# Distribuci?n muestral te?rica de Xbar
x <- seq(0,8,length=100)
fxc<- dgamma(x,16,rate=8)
plot(x,fxc,type="l",main="Distribuci?n muestral Gamma(16,8)",xlab="Xbar",ylab="Densidad",col="blue")
abline(v=2,lty=2)

# Distribuci?n bootstrap para las Xbar de muestras de tama?o 16

# Selecciona una muestra de tama?o 16 de una Gamma(1,1/2)
my.sample <- rgamma(16, 1, 1/2)

# Simula la distribuci?n bootstrap basada en 10^5 (10000) remuestras

N <- 10^5
my.boot <- numeric(N)
for (i in 1:N)
 {
  x <- sample(my.sample, 16, replace = TRUE)  #draw resample
  my.boot[i] <- mean(x)                     #compute mean, store in my.boot
  }

 hist(my.boot,xlab="X bar",ylab="Frecuencia")  #bootstrap distribution of mean
 abline(v=mean(my.boot),lty=2)
 mean(my.boot)  #mean
 sd(my.boot)    #bootstrap SE
mean(my.sample)
sd(my.sample)

####################################################
#								   #
# Estimaci?n de la distribuci?n - Ejemplo Ars?nico #
#								   #
####################################################

rm(list=ls(all=TRUE))


Bangladesh<-read.csv("C:\\Marisol\\UJaveriana\\CursosII2020\\EstadisticaMatematica\\Diapositivas\\Bangladesh.csv",h=T)

head(Bangladesh)
mean(Bangladesh$Arsenic) # Nombre de la base $ nombre de la variable

#attach(Bangladesh) # Evita el uso de nombre de base y nombre de la variable

Arsenic <- Bangladesh$Arsenic
mean(Arsenic)
sd(Arsenic)
plot(Arsenic)
 
# Histograma y QQPlot de los niveles de ars?nico en 271 pozos de Bangladesh
hist(Arsenic) 
qqnorm(Arsenic)
qqline(Arsenic,col=2)

# Distribuci?n bootstrap

n <- length(Arsenic)
N <- 10^4

arsenic.mean <- numeric(N)
for (i in 1:N)
{
x <- sample(Arsenic, n, replace = TRUE)
arsenic.mean[i] <- mean(x)
}

# Histograma y QQPlot de la distribuci?n botstrap de la concentraci?n 
# media de ars?nico

hist(arsenic.mean, main = "Bootstrap distribution of means")
abline(v = mean(Arsenic), col = "blue", lty = 2)
# vertical line at observed mean
qqnorm(arsenic.mean)
qqline(arsenic.mean, col=2)

# La distribuci?n bootstrap parece bastante normal con un poco de asimetr?a
# Teorema Central del L?mite "en acci?n" cuando el tama?o de muestra es lo
# suficientemente grande. La distribuci?n muestral para la media es aprox.
# normal a?n si la poblaci?n no es normal.

mean(arsenic.mean) # media bootstrap, similar a la media muestral xbar=125.31 
# [1] 125.5375
mean(arsenic.mean)-mean(Arsenic) # bias - sesgo
# [1] 0.2175773
sd(arsenic.mean) # error est?ndar bootstrap - EE (SE)
# [1] 18.25759

# C?lculo de los puntos que est?n 1.96 EE a partir de la media de la
# distribuci?n bootstrap

125.5375-1.96*18.25759 # mark of 1.96SE from mean
# [1] 89.75262
125.5375+1.96*18.25759
# [1] 161.3224
sum(arsenic.mean > 161.3224)/N
# [1] 0.0337 aprox. 3.4%
sum(arsenic.mean < 89.75262)/N
# [1] 0.0153  aprox. 1.5%

# Para la distribuci?n normal, se sabe que los percentiles 0.025 y 0.975
# est?n en la media ?1.96 desviaciones est?ndar.
# Para esta distribuci?n bootstrap en particular se encontr? que
# 1.5% de las medias de remuestreo son menores que la media bootstrap
# -1.96 EE y 3.4% de las medias de remuestreo son mayores que la
# media bootstrap +1.96 EE.
# En este caso el TCL ser? inexacto.


# Percentiles 2.5 y 97.5 para un IC del 95%
quantile(arsenic.mean, c(0.25,0.975)) # IC (92.9515,164.4418)

#####################################################
#								    #
# Ejemplo. Skateboard - Comparaci?n de dos muestras #
#								    #
#####################################################


Skateboard <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Skateboard.csv")


testF <- subset(Skateboard, select = Testosterone, subset = Experimenter == "Female",
            drop = TRUE)
testM <- subset(Skateboard, select = Testosterone, subset = Experimenter == "Male",
            drop = TRUE)
observed <- mean(testF) - mean(testM)


nf <- length(testF)
nm <- length(testM)

N <- 10^4

TestMean <- numeric(N)

for (i in 1:N)
{
  sampleF <- sample(testF, nf, replace = TRUE)
  sampleM <- sample(testM, nm, replace = TRUE)
  TestMean[i] <- mean(sampleF) - mean(sampleM)
}

hist(TestMean, main = "Distribuci?n bootstrap de la diferencia de medias",
     xlab="Dif. Medias")
abline(v = observed , col = "blue", lty = 2) # dif. media observada

qqnorm(TestMean)
qqline(TestMean)

mean(testF) - mean(testM)
# [1] 83.64934
mean(TestMean)
# [1] 82.64934
sd(TestMean)
# [1] 29.37578

quantile(TestMean,c(0.025,0.975)) # IC para las diferencias
# [1] 24.40689 139.12154

# El IC con perc. bootstrap del 95\% para la diferencia
# de las medias (mujer-hombre) es (24.42,139.12).

# Con un 95% de confianza, los niveles de testosterona de 
# hombres patinadores frente a una mujer est?n en promedio 
# entre 24.42 y 139.12 ng/dl m?s altos que los niveles frente a un hombre.

mean(TestMean)- observed  #sesgo
# [1] -0.4198628

#-------
#######################################################
#									#
# Prueba de permutaci?n para las medias de Skateboard #
#									#
#######################################################	

testAll <- subset(Skateboard, select = Testosterone, drop = TRUE)
#testAll <- Skateboard$Testosterone

N <- 10^4 - 1  # n?mero de veces que se repite el proceso

#set.seed(99) 
result <- numeric(N) # espacio para salvar las diferencias aleatorias
for(i in 1:N)
  {
  index <- sample(71, size = nf, replace = FALSE) # muestra de n?meros entre 1:71
  result[i] <- mean(testAll[index]) - mean(testAll[-index])
}
(sum(result >= observed)+1)/(N + 1)  #P-value


hist(result, xlab = "xbar1 - xbar2",
main="Distribuci?n de permutaci?n para niveles de testosterona")
#lines(density(result))
abline(v = observed, col = "blue")

qqnorm(result)
qqline(result)

#################################
#					  #
# Datos pareados datos de buceo #
#					  #
#################################

# Se comparan las medias de los puntajes de la semi-final y final
# de 12 mujeres buzo que compitieron en el campeonato mundial de 2017

Diving2017 <- read.csv("http://sites.google.com/site/chiharahesterberg/data2/Diving2017.csv")
Diff <- Diving2017$Final - Diving2017$Semifinal
n <- length(Diff)

N <- 10^5
result <- numeric(N)

for (i in 1:N)
{
  dive.sample <- sample(Diff, n, replace = TRUE)
  result[i] <- mean(dive.sample)
}

hist(result)
quantile(result, c(0.025, 0.975)) #IC












