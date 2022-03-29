# La vida útil de una bebida carbonatada es de interes. 
#Se seleccionan 10 botellas aleatoriamente para pruebas
#y se obtienen los siguientes resultados:

vida.util <- c(180, 138, 124, 163, 124, 159, 106, 134, 115, 139)

  #INTERVALO DE CONFIANZA PARA LA MEDIA:
  sample.mean <- mean(vida.util)
  sample.n <- length(vida.util)
  t.test(vida.util, conf.level=0.99)

  sample.sd <- sd(vida.util)
  
b= 1-0.99
 t1<- qt(b/2,sample.n-1)
e<- sample.sd/sqrt(sample.n)

limite.inf <- (sample.mean)-(t1*(e))
limite.sup <- (sample.mean)+(t1*(e))

limite.inf
limite.sup

# INSTRUMENTOS ELECTRÓNICOS

tiempo.reparacion <- c(159, 280, 101, 212, 224, 379, 179, 264, 222, 362
                       , 168, 250, 149, 260, 485, 170)
t.test(tiempo.reparacion, conf.level=0.95)

tiempo.10 <- c(1,2,1,3,5,1,5,2,3,5,3,6,5,3,2,1,6,8,2,3)
tiempo.20 <- c(7,6,8,9,5,5,9,7,5,4,8,6,6,8,4,5,6,8,7,7)

t.test(tiempo.10, tiempo.20, var.equal=TRUE)
t.test(tiempo.10, tiempo.20, var.equal=FALSE)
