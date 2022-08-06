rm(list=ls(all=TRUE))

#############################################
# Prueba de bondad de ajuste - Datos Arveja #
#############################################

Frecuencias<- c( 315,108,101,32)
n<-sum(Frecuencias)
gl<-length(Frecuencias)-1
pi<-c(9/16,3/16,3/16,1/16)

Esperadas<-n*pi

Q<-sum((Frecuencias-Esperadas)^2/Esperadas)
# QA<-sum(Frecuencias^2/Esperadas)-n # Forma alternativa de la estadística
Q
test.result<-chisq.test(Frecuencias, p=pi)
pchisq(test.result$statistic, df=gl, lower.tail=F)
test.result$expected

# Valor crítico
qchisq(0.95,gl)

########################################################
# Prueba de bondad de ajuste - Distribución geométrica #
########################################################

# Se lanza una moneda hasta que sale una cara y se registra el número de 
# lanzamientos X. Después de repetir el experimento 256 veces, obtenemos
# los siguientes resultados:
# x 1    2  3  4 5 6 7 8
# f 136 60 34 12 9 1 3 1
# A un nivel de significancia de 0.05, pruebe la hipótesis de que la 
# distribución observada de X se puede ajustar a la distribución 
# geométrica g(x; 1=2), x = 1, 2, 3, ...
# 

Frecuencias<- c( 136, 60, 34, 12, 9, 1, 3, 1)
n<-sum(Frecuencias)
rango<-1:8
pi<-1/2
Probabilidades<-pi*(1-pi)^(rango-1)
Prob8mas<-1-sum(Probabilidades)
Probabilidades[[8]]<-Probabilidades[[8]]+Prob8mas
Esperadas<-n*Probabilidades
sum(Frecuencias^2/Esperadas)-n
test.result<-chisq.test(Frecuencias, p=Probabilidades)
pchisq(test.result$statistic, df=7, lower.tail=F)
test.result$expected


Frecuencias<- c( 136, 60, 34, 12, 9, 5)
n<-sum(Frecuencias)
rango<-1:6
pi<-1/2
Probabilidades<-pi*(1-pi)^(rango-1)
Prob6mas<-1-sum(Probabilidades)
Probabilidades[[6]]<-Probabilidades[[6]]+Prob6mas
Esperadas<-n*Probabilidades
sum(Frecuencias^2/Esperadas)-n
test.result<-chisq.test(Frecuencias, p=Probabilidades)
pchisq(test.result$statistic, df=7, lower.tail=F)
test.result$expected

#############################################################
# Prueba de bondad de ajuste - Menores y mayores de 25 años #
#############################################################

f1<-c(400, 100, 500)
f2<-c(600, 400, 500)

Filas<-2
Tabla<-matrix(c(f1, f2),
                nrow=Filas,
                byrow=TRUE)
rownames(Tabla) = c("<25", ">25") # Nombres de fila
colnames(Tabla) = c("En contra", "Indeciso", "A favor")   #  Nombres de columna 
test.result<-chisq.test(Tabla)
test.result$expected

#####################################################
# Prueba de bondad de ajuste - Distribución Poisson #
#####################################################

TablaP<-matrix(c(11,25,28,20,9,7,13,27,28,17,11,4),nrow=2, byrow=TRUE)
colnames(TablaP)<-c("0","1","2","3","4",">=5")
rownames(TablaP)<-c("Pop.1","Pop.2")
ncol<-6
rango<-0:5
lambda <-2.1
Probabilidades<-dpois(rango,lambda)
Prob5mas<-1-sum(Probabilidades)
Probabilidades[[6]]<-Probabilidades[[6]]+Prob5mas
n<-sum(TablaP[1,])
Esperadas<-n*Probabilidades
Esperadas<-matrix(c(Esperadas,Esperadas),nrow=2,byrow=T)
Q<-sum((TablaP-Esperadas)^2/Esperadas)
pchisq(test.resultP$statistic, df=2*(6-1), lower.tail=F)

test.resultP<-chisq.test(TablaP)
test.resultP


##############################
# Prueba de bondad de ajuste #
##############################

### Gráfico de una distribución chi-cuadrado
x <- rchisq(100, 5)
hist(x, prob=TRUE, main = "Distribución chi-cuadrado", ylab="Densidad")
#curve( dchisq(x, df=1), col='black', add=TRUE )
curve( dchisq(x, df=5), col='black', add=TRUE)
curve( dchisq(x, df=10), col='blue', add=TRUE )

euroemp<-read.csv("C:\\Marisol\\UJaveriana\\CursosII2020\\EstadisticaMatematica\\Diapositivas\\PruebasChicuadrado\\Euroemp.csv",header=TRUE, row.names=1)

# Porcentajes de personas empleadas en 9 sectores 
# industriales en Europa de 1989 a 1995.
# AGR, agricultura,florestal y pesca
# MIN, minería. MAN, manufactura/fabricación. 
# PS, energía y agua. CON, construcción
# SER, servicios. FIN, finanzas. 
# SPS, servicios sociales y personales
# TC, transporte y comunicaciones

hist(euroemp$AGR, labels = F)  # exp
hist(euroemp$MIN, labels = T)  # weibull, normal
hist(euroemp$MAN, labels = F)  # Normal
hist(euroemp$PS, labels = F)   # 
hist(euroemp$CON, labels = F)  # Normal

k<-1 + 2.2*(log(30))

library(EnvStats) 
#help("gofTest")
gofTest(euroemp$AGR, test = "chisq", distribution = "exp", n.classes = 9)
gofTest(euroemp$AGR, test = "chisq", distribution = "exp", n.classes = 6)
gofTest(euroemp$AGR, test = "chisq", distribution = "exp")


gofTest(euroemp$AGR, test = "chisq", distribution = "weibull", n.classes = 9)
gofTest(euroemp$AGR, test = "chisq", distribution = "norm", n.classes = 9)

gofTest(euroemp$MAN, test = "chisq", distribution = "norm", n.classes = 9)
shapiro.test(euroemp$MAN)
gofTest(euroemp$MAN, test = "chisq", distribution = "weibull", n.classes = 6)

gofTest(euroemp$PS, test = "chisq", distribution = "norm", n.classes = 9)
gofTest(euroemp$PS, test = "chisq", distribution = "norm", n.classes = 6)
gofTest(euroemp$PS, test = "chisq", distribution = "norm")
shapiro.test(euroemp$PS)
shapiro.test(euroemp$CON)

###########################
# Prueba de independencia #
###########################

f1<-c(56, 68, 35)
f2<-c(19, 19, 28)
f3<-c(6, 10, 16)
f4<-c(12, 9, 13)

Filas<-4
Tabla<-matrix(c(f1, f2, f3, f4),
                nrow=Filas,
                byrow=TRUE)
rownames(Tabla) = c("Principal", "Pedido", "Cliente", "Otra") # Nombres de fila
colnames(Tabla) = c("Francia", "UK", "USA")   #  Nombres de columna 
chisq.test(Tabla)  

#########################
# Con una base de datos #
#########################

library(MASS)       # Carga paquete MASS 
data(survey)
head(survey)
?survey

tbl = table(survey$Smoke, survey$Exer) # Creación de tabla de contingencia
tbl                 # Imprime tabla de contingencia
chi<-chisq.test(tbl)
names(chi)
chi$expected
# Prueba que simula los p-valores cuando hay frec. <5.
chi<-chisq.test(tbl, simulate.p.value=T, B=10000)
chi<-chisq.test(tbl, simulate.p.value=T)

survey$Exer2<-survey$Exer
survey$Exer2 <- as.character(survey$Exer2)
survey$Exer2[survey$Exer2 == "Some"] <- "N+S"
survey$Exer2[survey$Exer2 == "None"] <- "N+S"
survey$Exer2 <- as.factor(survey$Exer2)
tbl2 = table(survey$Smoke, survey$Exer2) # Creación de tabla de contingencia
chi2<-chisq.test(tbl2)
chi2$expected