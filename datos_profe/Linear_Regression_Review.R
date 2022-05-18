rm(list=ls(all=TRUE)) #Borra todo el contenido de la memoria

###http://r-statistics.co/Linear-Regression.html
###https://web.stanford.edu/class/stats191/notebooks/Diagnostics_for_multiple_regression.html
###https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html

###Cargando paquete de datos y seleccionando mtcars 
require(datasets)
data(mtcars)
head(mtcars)
help(mtcars)



###Review A.1
###Diagrama de dispersi?n
plot(mtcars$wt, mtcars$mpg, main="Diagrama de dispersi?n", xlab="Peso del veh?culo", ylab="Millas por gal?n", pch=19)

###Coeficiente de correlaci?n de Pearson (Individuales y data frame)
cor(mtcars$wt, mtcars$mpg, method="pearson")
cor.test(mtcars$wt, mtcars$mpg, method="pearson")
cor(mtcars, use="complete.obs", method="pearson")
###install.packages("Hmisc")
library(Hmisc)
rcorr(as.matrix(mtcars), type="pearson") 

practica formativa : Eddy Herrera Daza.. 100 creditos aprdbados
eherrera@javeriana
Estadistica descripitvia
  
###Review A.2
###
###
data (cars)
help(cars)
###Gr?fico de dispersi?n con tendencia
scatter.smooth(x=cars$speed, y=cars$dist, main="Distancia ~ Velocidad")
plot(cars$speed,cars$dist)
abline(lm(cars$dist ~ cars$speed))
###Boxplot para detecci?n temprana de outliers
par(mfrow=c(1, 2))  # divide ?rea de gr?fico en 2 columnas
boxplot(cars$speed, main="Velocidad", sub=paste("filas de outliers: ", boxplot.stats(cars$speed)$out))  # box plot para 'speed'
boxplot(cars$dist, main="Distancia", sub=paste("fila de outliers: ", boxplot.stats(cars$dist)$out))  # box plot para 'distance'
###An?lisis de correlaci?n
cor(cars$dist, cars$speed, method="pearson")
cor.test(cars$dist, cars$speed, method="pearson")
###Gr?ficos de densidad para an?lisis preliminares distribucionales
###install.packages("e1071")
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Gr?fico de densidad: Velocidad", ylab="Frecuencia", sub=paste("Asimetr?a:", round(e1071::skewness(cars$speed), 2)))  # Gr?fico de densidad para 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Gr?fico de densidad: Distancia", ylab="Frecuencia", sub=paste("Asimetr?a:", round(e1071::skewness(cars$dist), 2)))  # Gr?fico de densidad para 'dist'
polygon(density(cars$dist), col="red")
###Ajuste del modelo por m?nimos cuadrados ordinarios
ajuste<- lm(dist ~ speed, data=cars) 
print(ajuste)
summary(ajuste)
par(mfrow=c(1, 1))
plot(ajuste)
shapiro.test(residuals(ajuste))
###install.packages("car")
library(car)
ncvTest(ajuste, data=cars)
###install.packages("lmtest")
library(lmtest)
dwtest(ajuste)

###An?lisis residual por wallyplot
###Kozak and Piepho (2017)
###install.packages("MESS")
library(MESS)
plot(ajuste)
wallyplot(ajuste)
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...) ; abline(a=0, b=1) }
wallyplot(ajuste, FUN=qqnorm.wally, main="")


### ?OPCIONES?
require(MASS)
bc<-boxcox(ajuste)
(lambda <- bc$x[which.max(bc$y)])
###Box-Cox en R
###Y?=(Y^lam - 1)/ lam si lam=!0
###Y?=log(Y) si lam = 0
dist.Transfor<-((cars$dist^(lambda))-1)/lambda
summary(fm1 <- lm(dist.Transfor ~ cars$speed))
plot(fm1)
shapiro.test(residuals(fm1))
library(car)
ncvTest(fm1, data=cars)
library(lmtest)
dwtest(fm1)
###An?lisis residual por wallyplot
###Kozak and Piepho (2017)
###install.packages("MESS")
###library(MESS)
plot(fm1)
wallyplot(fm1)
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...) ; abline(a=0, b=1) }
wallyplot(fm1, FUN=qqnorm.wally, main="")



###Review A.3.1
###Estimates of Coefficients and Their Significance: Confidence Intervals and t Tests
data(faithful)
help(faithful)
head(faithful)
###TAREA: No olvidar hacer todos los an?lisis complementarios(descriptivo, correlaci?n, verificaci?n de supuestos etc.)
eruption.lm = lm(eruptions ~ waiting, data=faithful) 
summary(eruption.lm) 
###Creaci?n de intervalos de confianza para par?metros
confint(eruption.lm)
###Review A.3.2 Coeficiente de determinaci?n
names(summary(eruption.lm))
summary(eruption.lm)$r.squared
###Review A.3.3 Error est?ndar residual, ra?z del CME
summary(eruption.lm)$sigma 
###Review A.3.4 Prueba F
summary(eruption.lm)
#aov(eruption.lm)
#summary(aov(eruption.lm))
###Review A.3.5 R2 Ajustado
summary(eruption.lm)$adj.r.squared



###Review A.3.6 Cp de Mallows
###install.packages("leaps")
require(leaps)
data(mtcars)
head(mtcars)
help(mtcars)
leaps(x=mtcars[,c(3,4,6,7)],y=mtcars[,1], names=names(mtcars[,c(3,4,6,7)]), method="Cp") 
leaps(x=mtcars[,c(3,4,6,7)],y=mtcars[,1], names=names(mtcars[,c(3,4,6,7)]), method="Cp", nbest=1) 



###Review A.3.7 AIC y AICc Akaike
### BIC Bayesian Information Criterion
### Review A.3.8 
head(swiss)
help (swiss)
lm1.swiss<- lm(Fertility ~ . , data = swiss)
summary(lm1.swiss)
AIC(lm1.swiss)
BIC(lm1.swiss)
lm2.swiss <- update(lm1.swiss, . ~ . -Examination)
AIC(lm1.swiss, lm2.swiss)
BIC(lm1.swiss, lm2.swiss)
###library(sme)
library(qpcR)
###install.packages("sme") ; install.packages("qpcR")
AICc(lm1.swiss)
AICc(lm2.swiss)


###Review A.4
### M?todo "stepwise" para modelos de regresi?n lineal m?ltiple
### install.packages("faraway")
library(faraway)
help(prostate)
head(prostate)
###Pregunta para todos: ?lpsa como respuesta?
prostata<-prostate
prostata[,5]<-factor(prostata[,5])
modelo.nulo<-lm(lpsa~1,data=prostata)
modelo.completo<-lm(lpsa~.,data=prostata)
step(modelo.completo, data=prostata, direction="backward",trace=FALSE)  ### Backward: Eliminaci?n un paso atr?s
step(modelo.nulo, scope=list(lower=modelo.nulo, upper=modelo.completo),data=prostata, direction="forward",trace=FALSE) ### Inclusi?n un paso adelante
step(modelo.completo, data=prostata, direction="both",trace=FALSE) ### Stepwise: Paso a paso - Combinaci?n de Backward y Forward
step(modelo.completo, data=prostata, direction="both",trace=TRUE)
summary(lm(lpsa ~ lcavol + lweight + age + lbph + svi, data = prostata))
summary(lm(lpsa ~ lcavol + lweight + lbph + svi, data = prostata))
summary(lm(lpsa ~ lcavol + lweight + svi, data = prostata))
coef(lm(lpsa ~ lcavol + lweight + svi, data = prostata))



###Review A.6
###Bondad de ajuste
###An?lisis de residuales
X<-read.table("C:\\Marisol\\UJaveriana\\CursosII2020\\EstadisticaMatematica\\Diapositivas\\Precios_Casas.txt",h=T)

head(X)
plot(X)
cor(X, use="complete.obs", method="pearson")
library(Hmisc)
rcorr(as.matrix(X), type="pearson") 
completo.casas<-lm(Precio~.,data=X)
step(completo.casas, data=X, direction="backward",trace=FALSE)  ### Backward: Eliminaci?n un paso atr?s
ajuste1.casas<-lm(Precio ~ SqFt + LotSize + Baths, data = X)
summary(ajuste1.casas)
ajuste2.casas<-lm(Precio ~ SqFt + LotSize, data = X)
summary(ajuste2.casas)
plot(ajuste2.casas)
shapiro.test(residuals(ajuste2.casas))
###install.packages("car")
library(car)
ncvTest(ajuste2.casas, data=cars)
###install.packages("lmtest")
library(lmtest)
dwtest(ajuste2.casas)
###An?lisis residual por wallyplot
###Kozak and Piepho (2017)
###install.packages("MESS")
###library(MESS)
plot(ajuste2.casas)
wallyplot(ajuste2.casas)
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...) ; abline(a=0, b=1) }
wallyplot(ajuste2.casas, FUN=qqnorm.wally, main="")



###A.6.2 Intervalos de confianza
###A.6.3 Intervalos de predicci?n
#eruption.lm = lm(eruptions ~ waiting)
#newdata = data.frame(waiting=80)
#predict(eruption.lm, newdata, interval="confidence") 
nuevos.datos = data.frame(SqFt=2950, LotSize=21, Baths=3)
predict(ajuste1.casas, nuevos.datos, interval="confidence") ###655.7128 - 672.8194 
predict(ajuste1.casas, nuevos.datos, interval="predict") ### 621.6599 - 706.8722


###install.packages("caret")
###require(caret)
###https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
###https://www.quora.com/Is-Linear-Regression-applicable-to-cross-validation

### true regression ---------
set.seed(1)
x1 <- rnorm(100)
y <- 0.5 + 2*x1 + rnorm(100,0,5)
df <- data.frame(y=y, x1=x1)
lm.mod <- lm(y ~ ., data=df)
summary(lm.mod)

lm.overfit <- lm(y~poly(x1, 20), data=df)
summary(lm.overfit)

### cross-validation -----
 
library(caret)
controlConfig <- trainControl(method="cv", number=10, repeats=1)
set.seed(123)
simple.lm <- train(y ~ x1, data=df, method="lm", trControl=controlConfig)
# Error de validaci?  cruzada para regesi?n simple
set.seed(123)
juice.lm <- train(y ~ poly(x1, 20), data=df, method="lm", trControl=controlConfig)
# Error de validaci?  cruzada para regesi?n polinomial
 
## compare CV RMSE values
 
models <- resamples(list(simple=simple.lm, juice=juice.lm))
summary(models, metric="RMSE")

###Validaci?n cruzada en modelos "precios de casas"
controlConfig.casas<- trainControl(method="LOOCV")
cv.casas1<-train(Precio ~ SqFt + LotSize + Baths, data=X, method="lm", trControl=controlConfig.casas)
cv.casas2<-train(Precio ~ SqFt + LotSize, data=X, method="lm", trControl=controlConfig.casas)
print(cv.casas1)
print(cv.casas2)
cv.casas1$results
cv.casas2$results

###Regresi?n con predictores binarios
Xbin<-read.table("C:\\Marisol\\UJaveriana\\CursosII2020\\EstadisticaMatematica\\Diapositivas\\OakKnoll_R.txt", h=T)

head(Xbin)
summary(Xbin)
Xbin$OakKnoll<-as.factor(Xbin$OakKnoll)
modelo1.bin<-lm(Precio ~ Area + OakKnoll, data=Xbin) 
summary(modelo1.bin)
aov(modelo1.bin)
modelo2.bin<-lm(Precio ~ Area + Region, data=Xbin)
summary(modelo2.bin)
#Xbin$Region<-relevel(Xbin$Region, ref="OakKnoll")
#modelo3.bin<-lm(Precio ~ Area + Region, data=Xbin)
#summary(modelo3.bin)


###
###Diagn?sticos del modelo lineal - Temas adicionales (Su et al. 2012)
###

##########################################################################
# 				Autopartes							 #
#												 #
# El gerente de ventas de una empresa dedicada a la comercializaci?n de  #
# autopartes desea desarrollar un m?todo para pronosticar las ventas     #
# anuales de una regi?n. En apariencia varios factores est?n relacionados#
# con las ventas y de acuerdo con la opini?n del gerente las variables   #
# que deben intervenir en el an?lisis son 					 #
# 												 #
#      Y = Ventas anuales (en millones de d?lares)				 #
#	 X_1 = N?mero de tiendas de venta al menudeo				 #
#      X_2 = N?mero de autos registrados (en millones)			 #
#      X_3 = Ingreso personal (en millones de d?lares)			 #
# 	 X_4 = Antiguedad promedio de los autom?viles (en a?os)		 #
# 	 X_5 = N?mero de supervisores							 #
#												 #	
# La informaci?n correspondiente a diez regiones elegidas al azar se     #
# encuentra en el archivo Autopartes.						 #	
# Los diagn?sticos del modelo de regresi?n lineal m?ltiple               #
# estimado se muestran a continuaci?n.						 #
#												 #
##########################################################################

Autopartes <-read.table("C:\\Marisol\\UJaveriana\\CursosII2020\\EstadisticaMatematica\\Diapositivas\\Autopartes.txt", h=T)

names(Autopartes)
head(Autopartes)
#############################
# Regresi?n Lineal M?ltiple #
#############################
fit <- lm(Y ~ . - Region, data=Autopartes)
summary(fit)

# Residuales ordinarios - ri
ri <- residuals(fit)
ri
# fit$residuals

# Residuales estudentizados internamente / estandarizados - rsi
rsi <- rstandard(fit);rsi

# Residuales estudentizados externamente - rse
rse <- rstudent(fit);rse

names(fit)

gl.grafico<-fit$df.residual-1
tinferior<-qt(0.025,df=gl.grafico)
tsuperior<-qt(0.975,df=gl.grafico)

plot(fit$fitted.values, rse, 
     ylab="Residuales jackknife", xlab="Valores ajustados", 
     main="Residuales jackknife") 
abline(0, 0)     
abline(h=tinferior, col="red")
abline(h=tsuperior, col="red")
###abline(h=-2, col="red"); abline(h=2, col="red")

###################################################
# Verificaci?n de los supuestos sobre los errores #
###################################################

#Prueba de normalidad, H0: Los errores siguen una distribuci?n normal
shapiro.test(rstudent(fit))
library(car)
qqPlot(fit)
qqPlot(fit, distribution="norm")

#Prueba de varianza constante, H0: Los errores tienen varianza homogenea
library(car)
ncvTest(fit)

library(lmtest)
# Prueba Breush-Pagan
bptest(fit)

#Pueba para autocorrelaci?n, H0: No hay autocorrelaci?n, es decir los errores son independientes
library(lmtest)
dwtest(fit)

###Linealidad: Partial residual plots
crPlots(fit, data=Autopartes) 


###Outlier in X-space
plot(hatvalues(fit), pch=23, bg='orange', cex=2, ylab='Valores hii')
hii_cotainferior<-(2*length(fit$coefficients))/(nrow(Autopartes))
###abline(h=hii_cotainferior, col="red")
Autopartes[which(hatvalues(fit) > hii_cotainferior),]
###Desventaja de los leverage 
###Herramienta diagn?stica que trata cada variable independiente de la misma forma
###No importa como afecta cada una la variable respuesta
###Variables eficaces e ineficaces

###Outlier in Y-Space
###outlierTest(lm(prestige ~ income + education, data=Duncan))
###H0: El dato NO es outlier
library(car)
outlierTest(fit)

###Influential points
###
###DFBetas 
###Si |dfbeta|>2/(sqrt(n)) posible influyente
###n=length(Autopartes$Y)
###2/(sqrt(n))
dfbetas(fit)

###DFFits
###Si |Dffitsi|>1 Posible influyente (conjuntos de datos peque?os y medianos)
###Si |Dffitsi|>2*raiz((p+1)/n) (conjuntos de datos grandes)
###Si |Dffitsi|>2*sqrt((length(fit$coefficients))/(nrow(Autopartes)))
###2*sqrt((length(fit$coefficients))/(nrow(Autopartes)))
dffits(fit)


###Distancia de Cook
###Di >1 indicador influyente
cooks.distance(fit)

 
###Todas las estad?sticas
influence.measures(fit)
Meddiag<-influence.measures(fit);Meddiag
summary(Meddiag)


###An?lisis gr?fico del paquete OLSRR
###install.packages("olsrr")
library(olsrr)
ols_plot_dfbetas(fit)
ols_plot_dffits(fit)
ols_plot_cooksd_chart(fit)

### Multicolinealidad
###https://cran.r-project.org/web/packages/olsrr/olsrr.pdf
ols_vif_tol(fit)
library(car)
vif(fit)