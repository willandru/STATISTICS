from scipy.stats import norm
import math 

# Se tiene la tasa historica de devoluciones = 13% de un 
# supermercado y se tiene que en la ultima semna en una
# de 250 personas hubieron 22 devoluciones. Se quiere
# saber si la tasa de




x = 22

n = 250

print(x,' devoluciones en una muestra de tamaño ', n)
p = x/n

print('La proporción ESTIMADA PI: ', p)
p0= 0.13
print('Tasa Historica de Devoluciones de un Supermercado: PI_0 : ', p0)



np0= n*p0
nq0 = n*(1-p0)

print('Verificando la condicion de normalidad :', np0, nq0)



print(norm.ppf(0.05))


Zc= (p-p0)/(math.sqrt(p0*(1-p0)/n))

print('Calculando la estadistica de prueba Zc :', Zc)