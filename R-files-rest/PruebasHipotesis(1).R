rm(list=ls(all=TRUE))

########################
# Pruebas de hipótesis #
########################

######################
# Para la proporción #
######################

x<-22
n<-250
p<-x/n
p0<-0.13
np0<-n*p0 # verificar que sea mayor o igual a 10
nq0<-n*(1-p0) # verificar que sea mayor o igual a 10

# Valor crítico, cambia de acuerdo a la dirección de la prueba
qnorm(0.05) # Es una prueba unilateral a izquierda

# Estadística de prueba
Zc<-(p-p0)/(sqrt(p0*(1-p0)/n))

# Valor-p
pnorm(Zc) # por ser unilateral a izquierda, H1:pi<0.13
1-pnorm(Zc) # si fuera unilateral a derecha, H1:pi>0.13
2*pnorm(Zc) # si fuera bilateral, H1:pi not= 0.13


# prop.test(x, n, p = NULL,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)
# correct, indicating whether Yates' continuity correction should be applied where possible.

prop.test(x = 22, n = 250, p = 0.13, alternative=c("less"),correct = F)

# Útil cuando n es pequeño y no se cumple el supuesto de normalidad
binom.test(x=22, n=250, p = 0.13, alternative = "less")

#######################################
# Para la media con varianza conocida #
#######################################

mu0<-216
n<-50
sigma2<-0.023^2
Xbar<-216.007

# Valor crítico, cambia de acuerdo a la dirección de la prueba
qnorm(0.95) # Es una prueba unilateral a derecha

# Estadística de prueba
Zc<-(Xbar-mu0)/(sqrt(sigma2/n))

# Valor-p
1-pnorm(Zc)


##########################################
# Para la media con varianza desconocida #
##########################################

t.test(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, ...)

x<-c(140, 140, 141, 145, 143, 144, 142, 140, 145, 143, 140, 140,
	141, 141, 137, 142, 143, 141, 142, 142, 143, 141, 138, 139)
t.test(x,mu=142,alternative=c("two.sided"))

# Valores críticos
qt(0.05,23)
qt(0.95,23)

####################
# Para la varianza #
####################

#install.packages("EnvStats")
library(EnvStats)

#varTest(x, alternative = "two.sided", conf.level = 0.95, 
    sigma.squared = 1, data.name = NULL)

y<- c(120, 143, 136, 126, 122, 140, 133, 133, 131, 131, 129, 128,
	 131, 123, 119, 135, 137, 134, 115, 122)
gl<-length(y)-1
varTest(y,alternative = "two.sided",sigma.squared=49)

# Valores críticos
qchisq(0.025,gl)
qchisq(0.975,gl)

PHV<-varTest(y,alternative = "two.sided",sigma.squared=49)
names(PHV)
PHV$statistic

# p-valor
2*(1-pchisq(PHV$statistic,gl)) 


######################################
# Para la diferencia de proporciones #
######################################

alpha<- 0.01
n1 <- 3033
n2 <- 3033
x1 <- 118
x2 <- 80
p1 <- x1/n1
p2 <- x2/n2
pbar<- (x1+x2)/(n1+n2)
n1p1<-n1*p1 # verificar que sea mayor o igual a 10
n1q1<-n1*(1-p1) # verificar que sea mayor o igual a 10
n2p2<-n2*p2 # verificar que sea mayor o igual a 10
n2q2<-n2*(1-p2) # verificar que sea mayor o igual a 10


# Valor crítico
qnorm(1 - alpha) # prueba unilateral a derecha

# Estadística de prueba
Zc<-(p1-p2)/(sqrt(pbar*(1-pbar)*(1/n1+1/n2)))

# p-valor
1-pnorm(Zc)


x <- c(118, 80)
n <- c(3033, 3033)

prop.test(x, n, alternative=c("greater"),correct = FALSE)


########################################################
# Para la diferencia de medias con varianzas conocidas #
########################################################

TAREA


#####################################################################
# Para la diferencia de medias con varianzas desconocidas e iguales #
#####################################################################

### Asumiendo independencia 
S<-c(446, 401, 476, 421, 459, 438, 481, 411, 456, 427, 459, 445) #Proceso estándar
N<-c(462, 448, 435, 465, 429, 472, 453, 459, 427, 468, 452, 447) #Proceso nuevo
t.test(S,N, var.equal=TRUE)

t.test(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, ...)


########################################################################
# Para la diferencia de medias con varianzas desconocidas y diferentes #
########################################################################

### Asumiendo independencia
S<-c(446, 401, 476, 421, 459, 438, 481, 411, 456, 427, 459, 445) #Proceso estándar
N<-c(462, 448, 435, 465, 429, 472, 453, 459, 427, 468, 452, 447) #Proceso nuevo
t.test(S,N,var.equal=FALSE)


#######################################################
# Para la diferencia de medias para muestras pareadas #
#######################################################

X1<-c(5500,1000,2500,7800,6400,8800,600,3300,4500,6500)
X2<-c(6000,900,2500,8300,6200,9400,500,3500,5200,6800)
t.test(X1,X2,paired = TRUE)


######################################
# Para la diferencia entre varianzas #
######################################

var.test(S, N)

