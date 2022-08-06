##########
# Newton #
##########

# Halla la raiz de Fx
Fx <- function(x) exp(-x) + x -2
F1x <- function(x) 1-exp(-x)

newton <- function(x) {
for(i in 1:20) {
x<-x-Fx(x)/F1x(x)
if (Fx(x) == 0) break
error<-abs(Fx(x)/F1x(x))
cat("X=",x,"\t","E=",error,"\n")
}
}
newton(1)


################################################################
#										   #
# Muestra aleatoria de tamaño n de una distribución Cauchy con #
# f(x,\theta)=1/(\pi(1+(x-\theta)^2)), -\infty<x<\infty,       #
# -\infty<\theta<\infty							   #
#									 	   #
################################################################

rm(list=ls(all=TRUE))

x <- c(1, 2, 2, 3)
g <- function(theta) sum(log(1 + (x-theta)^2))
optimize(g, interval = c(0, 4))

logL <- function(theta) sum(log(dcauchy(x, theta)))
optimize(logL, interval = c(0, 4), maximum = TRUE)

# La solución 2 es el estimador de \theta



################################################################
#										   #
# Muestra aleatoria de tamaño n de una distribución Bernoulli  #
# con P(x,\theta)=\theta^{x}(1-\theta)^{1-x}		         #
#									 	   #
################################################################

# n=20 y S=12, S=sum x_i

# Verosimilitud X_1,...,X_n ~ Bernoulli(theta)
L_bernoulli <- function(n, S){
    function(theta){
        theta ^ S * (1 - theta) ^ (n - S)
    }  
}
# log-verosimilitud
l_bernoulli <- function(n, S){
    function(theta){
        S * log(theta) + (n - S) * log(1 - theta)
    }  
}

library(ggplot2)

xy <- data.frame(x = 0:1, y = 0:1)
verosimilitud <- ggplot(xy, aes(x = x, y = y)) +
    stat_function(fun = L_bernoulli(n = 20, S = 12)) +
    xlab(expression(theta)) +
    ylab(expression(L(theta))) +
    ggtitle("Verosimilitud (n=20, S = 12)")

log_verosimilitud <- ggplot(xy, aes(x = x, y = y)) +
    stat_function(fun = l_bernoulli(n = 20, S = 12))+
    xlab(expression(theta)) +
    ylab(expression(l(theta))) +
    ggtitle("log-verosimilitud (n=20, S = 12)")

library(gridExtra)
grid.arrange(verosimilitud, log_verosimilitud, nrow = 1)  

# Con S=12 y n=20, \widehat{\theta}= media muestral=12/20=0.6

optimize(L_bernoulli(n = 20, S = 12), interval = c(0, 1), maximum = TRUE)
#> $maximum
#> [1] 0.6
#> 
#> $objective
#> [1] 1.43e-06
optimize(l_bernoulli(n = 20, S = 12), interval = c(0, 1), maximum = TRUE)
#> $maximum
#> [1] 0.6
#> 
#> $objective
#> [1] -13.5



##################################################
#		   						 #
#                Algoritmo EM 			 #
# X_1~Pois(lambda1), X_2~Pois(lambda2)	 	 #
# X_3~Pois(beta*lambda1), X_4~Pois(beta*lambda2) #
#								 #
##################################################

lambda1 <- 1 #initial value for lambda1

for (i in 1:20)
{
  beta <- 12/(lambda1 + 3)
  lambda1 <- (lambda1 + 5)/(beta + 1)
  lambda2 <- 10/(beta + 1)
}

beta
# [1] 2.333355

lambda1
#[1] 2.142829

lambda2
#[1] 2.999981


