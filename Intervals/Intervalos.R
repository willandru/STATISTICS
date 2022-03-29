
rm(list=ls(all=TRUE))

#####################################
# Percentiles de las distribuciones #
#####################################

qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
qchisq(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
qf(p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)

##############
# Intervalos #
##############

#######################################
# Para la media con varianza conocida #
#######################################

alpha<- 0.05
n <- 64
varianza <- 625
media <- 1012
cuantil<- qnorm(1 - alpha/2)

lim_inf<-media - cuantil * sqrt(varianza) / sqrt(n)
lim_inf 

lim_sup<- media + cuantil * sqrt(varianza) / sqrt(n)
lim_sup

cbind(lim_inf,lim_sup)

##########################################
# Para la media con varianza desconocida #
##########################################

t.test(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, ...)

x <- c(530, 450, 600, 570, 360, 550, 640, 490, 460, 550,
480, 440, 530, 470, 560, 500, 430, 640, 420, 530)

x.Mean <- mean(x)
t.test(x,mu=500,conf.level=0.90)

######################
# Para la proporci?n #
######################

# prop.test(x, n, p = NULL,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)
# correct, indicating whether Yates' continuity correction should be applied where possible.

prop.test(x = 24, n = 75, p = 0.2, correct = F)

alpha<- 0.05
n1 <- 75
x1 <- 24
p1 <- x1/n1
cuantil<- qnorm(1 - alpha/2)

lim_inf<- p1 - cuantil * sqrt(p1*(1-p1)/n1)
lim_inf 

lim_sup<- p1 + cuantil * sqrt(p1*(1-p1)/n1)
lim_sup

cbind(lim_inf,lim_sup)

####################
# Para la varianza #
####################

#install.packages("EnvStats")

library(EnvStats)

#varTest(x, alternative = "two.sided", conf.level = 0.95, 
    sigma.squared = 1, data.name = NULL)

y<- c(1, 0.9, 1.5, 2.8, 3.1, 3.2, 2.5, 1.9, 2)
length(y)
var(y)
varTest(y,alternative = "two.sided",sigma.squared=0.8, conf.level=0.99)
varTest(y,alternative = "two.sided",sigma.squared=0.8, conf.level=0.95)

IC (99%) 0.2691 = s2 = 4.4179.
IC (95%) 0.3383 = s2 = 2.7156.

lim_inf <- (length(y)-1)*var(y)/qchisq(0.975,length(y)-1)
lim_sup <- (length(y)-1)*var(y)/qchisq(0.025,length(y)-1)
cbind(lim_inf,lim_sup)


########################################################
# Para la diferencia de medias con varianzas conocidas #
########################################################

TAREA
x1<- c(1, 0.9, 1.5, 2.8, 3.1, 3.2, 2.5, 1.9, 2)
n1<-length(x1)-1
x2<- c(1, 0.6, 1.2, 1.4, 3.1, 1.1, 0.5, 1, 2)
n2<-length(x2)-1

gl <- n1+n2

qt(0.975, gl)



######################################################################
# Para la diferencia de medias con varianzas desconocidas  e iguales #
######################################################################

### Asumiendo independencia 
S<-c(446, 401, 476, 421, 459, 438, 481, 411, 456, 427, 459, 445) #Proceso est?ndar
N<-c(462, 448, 435, 465, 429, 472, 453, 459, 427, 468, 452, 447) #Proceso nuevo
t.test(S,N, var.equal=TRUE)

########################################################################
# Para la diferencia de medias con varianzas desconocidas y diferentes #
########################################################################

### Asumiendo independencia
S<-c(446, 401, 476, 421, 459, 438, 481, 411, 456, 427, 459, 445) #Proceso est?ndar
N<-c(462, 448, 435, 465, 429, 472, 453, 459, 427, 468, 452, 447) #Proceso nuevo
t.test(S,N) # IC del 95%
t.test(S,N,conf.level=0.99) # IC del 99%


#######################################################
# Para la diferencia de medias para muestras pareadas #
#######################################################

t.test(S,N,paired = TRUE)

######################################
# Para la diferencia de proporciones #
######################################

x <- c(40, 10)
n <- c(200, 100)

prop.test(x, n, conf.level = 0.95, correct = FALSE)

alpha<- 0.05
n1 <- 200
n2 <- 100
x1 <- 40
x2 <- 10
p1 <- x1/n1
p2 <- x2/n2
cuantil<- qnorm(1 - alpha/2)

lim_inf<- (p1-p2) - cuantil * sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
lim_inf 

lim_sup<- (p1-p2) + cuantil * sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
lim_sup

cbind(lim_inf,lim_sup)

##############################
# Para cociente de varianzas #
##############################

var.test(S, N)

###################################################
# Tama?o de muestra apropiado usando funci?n de R #
###################################################

#############################
# Para diferencia de medias #
#############################

power.t.test(n = NULL, delta = NULL, sd = 1, sig.level = 0.05,
             power = NULL,
             type = c("two.sample", "one.sample", "paired"),
             alternative = c("two.sided", "one.sided"),
             strict = FALSE, tol = .Machine$double.eps^0.25)

###Asumiendo diferencia cr?tica en las medias 0.5
###Asumiendo desviaci?n est?ndar de 0.25
###https://stat.ethz.ch/R-manual/R-devel/library/stats/html/power.t.test.html
power.t.test(delta = 0.5, sd = 0.25, power = 0.95)
power.t.test(delta = 0.5, sd = 0.25, power = 0.95, type="paired")

###################################
# Para diferencia de proporciones #
###################################

power.prop.test(n = NULL, p1 = NULL, p2 = NULL, sig.level = 0.05,
                power = NULL,
                alternative = c("two.sided", "one.sided"),
                strict = FALSE, tol = .Machine$double.eps^0.25)

# Si se busca el tama?o de muestra, n debe ser null

n1 <- 200
n2 <- 100
x1 <- 40
x2 <- 10

power.prop.test(p1=x1/n1,p2=x2/n2 ,power=0.95)
