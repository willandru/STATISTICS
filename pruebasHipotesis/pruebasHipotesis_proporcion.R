rm(list=ls (all=TRUE))

#FUNCION ESPECIAL PARA LA PROPORCION (prop.test -- chi²)

#prop.test(x, n, p=NULL, alternative = c("two sided", "less", "greater"),
# conf.level=0.95, correct=TRUE)

#hyperparameter   p: es p_0 (la tasa hsitorica de devoluciones 13% por ejemplo), si
#                 no se tiene (NULL) será 0

#hyperparameter  alternative :  indicar el tipo de prueba que tenemos, UNILATERAL A IZQUIERDA (LESS)
#       UNILATERAL DERECHA : "greater" , BILATERAL: "two sided"

#hyperparameter   correct: CORRECION DE CONTINUIDAD cuando pasamos de una discreta a una continua por ejemplo

prop.test(x=22, n=250, p=0.13, alternative=c("less"), correct = F)
#si se fija en el output, se utiliza la prueba chi-cuadrado, se indica el p-value
# y se entrega un intervalo de confianza que NO INCLUYE AL 0.13

#SI NO CONTIENE AL P_0=0.13 EN EL 95 percent confidence interval:[0.0000000 0.1220539] RECHAZO LA HIPOTESIS
# sample estimates p = x/n
