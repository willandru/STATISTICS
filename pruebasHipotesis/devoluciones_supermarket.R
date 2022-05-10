rm(list=ls (all=TRUE))
x<-22
n<-250
p<-x/n
p0<-0.13
#Verificar NORMALIDAD
np0<-n*p0
nq0<-n*(1-p0)

#PRUEBA UNILATERAL A IZQUIERDA
# :: estos dos valores se comparan :: SE RECHAZA H_0 SI Zc<qnorm(0.05)
#El valor crítico cambia de acuerdo a la dirección de la prueba
qnorm(0.05)


#ESTADISTICA DE PRUEBA
Zc <- (p-p0)/(sqrt(p0*(1-p0)/n))


#VALOR-P
pnorm(Zc)#Por ser unilateral a izquierda ,  (H1:pi<0.13)
1-pnorm(Zc)#Si fuera unilateral a derecha, (H1:pi>0.13)
2*pnorm(Zc) # Si fuera bilateral (H1:pi not 0.13)
