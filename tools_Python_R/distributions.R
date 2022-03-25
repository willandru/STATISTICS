
#pf--> cummulative distribution fucntion of f-distribution
#		:The AREA TO THE LEFT

#f-distribution
#fd_numerator=3
#fd_denominator=14

pf(2.448, 3, 14)



#FIND QUANTILES: finde the F-VALUE
	#We have to give the area to the left
qf(.975, 3, 14)


qnorm()
qt()
qchisq()
qf()


# Para la media con varianza desconocida #
##########################################

t.test(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, ...)

x <- c(530, 450, 600, 570, 360, 550, 640, 490, 460, 550,
480, 440, 530, 470, 560, 500, 430, 640, 420, 530)

t.test(x,mu=500,conf.level=0.90)


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



qt(0.975)