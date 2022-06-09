rm(list=ls(all=TRUE))
#2.8
rm(list=ls(all=TRUE))
datos <- c(9.37, 13.04, 11.69, 8.21, 11.18, 10.41, 13.15, 11.51, 13.21, 7.75)
qqnorm((datos))
qqline(datos)
n=length((datos))
qt(0.95,n-1)
t.test(datos, mu=10)



#2.20
rm(list=ls(all=TRUE))
days=c(108, 124, 124, 106, 115, 138, 163, 159, 134, 139)
t.test(days,mu=120,alternative=c("greater"), conf.level=0.99)
n=length((days))
qt(0.99,n-1)


#2.22
rm(list=ls(all=TRUE))
Hours=c(159, 224, 222, 149, 280, 379, 362, 260, 101, 179, 168, 485, 212, 264, 250, 170)
n=length((Hours))
t.test(Hours, mu=225,alternative=c("greater") )
qt(0.95, n-1)


#2.26
rm(list=ls(all=TRUE))
TypeI=c(65, 81, 57, 66, 82, 82, 67, 59, 75, 70)
TypeII=c(64, 71, 83, 59, 65, 56, 69, 74, 82, 79)
qnorm(TypeI)
var.test(TypeI, TypeII)
n=length((TypeI))
qf(0.975,n-1,n-1)
qf(0.025,n-1,n-1)

t.test(TypeI, TypeII, var.equal=TRUE)
qt(0.975, n-1)


#2.30
rm(list=ls(all=TRUE))
ten_seconds=c(1,2,1,3,5,1,5,2,3,5,3,6,5,3,2,1,6,8,2,3)
twenty_seconds=c(7,8,5,9,5,8,6,4,6,7,6,9,5,7,4,6,8,5,8,7)
n=length((ten_seconds))
var.test(ten_seconds, twenty_seconds)
qf(0.975,n-1,n-1)
qf(0.025,n-1,n-1)

t.test(ten_seconds, twenty_seconds, var.equal=TRUE)
qt(0.975, n-1)


#2.31
rm(list=ls(all=TRUE))
Etch=c(5,34, 6,00, 5,97, 5,25, 6,65, 7,55, 7,35, 6,35, 4,76, 5,54, 5,44, 4,61, 5,98, 5,62, 4,39, 6,00, 7,25, 6,21, 4,98, 5,32)
library(EnvStats)
gl<-length(Etch)-1
PHV<-varTest(Etch,alternative = "two.sided",sigma.squared=1)
# Valores cr?ticos
qchisq(0.025,gl)
qchisq(0.975,gl)
PHV$statistic
qqnorm(Etch)
qqline(Etch)


#2.32
rm(list=ls(all=TRUE))

caliper1=c(0.265, 0.265, 0.266, 0.267, 0.267, 0.265, 0.267, 0.267, 0.265, 0.268, 0.268, 0.265)
caliper2=c(0.264, 0.265, 0.264, 0.266, 0.267, 0.268, 0.264, 0.265, 0.265, 0.267, 0.268, 0.269)
var.test(caliper1, caliper2)
n=length((caliper2))
qf(0.975,n-1,n-1)
qf(0.025,n-1,n-1)

t.test(caliper1, caliper2, var.equal=TRUE, paired=TRUE)
qt(0.975, n-1)


#2.33
rm(list=ls(all=TRUE))

b1<-c(6.08, 6.22, 7.99, 7.44, 6.48, 7.99, 6.32, 7.60, 6.03, 7.52)
b2<-c(5.73, 5.80, 8.42, 6.84, 6.43, 8.76, 6.32, 7.62, 6.59, 7.67)
var.test(b1, b2)
n=length((b1))
qf(0.975,n-1,n-1)
qf(0.025,n-1,n-1)

t.test(b1, b2, var.equal=TRUE)
qt(0.975, n-1)


#2.35
rm(list=ls(all=TRUE))
Formulation_1=c(206, 188, 205, 187, 193, 207, 185, 189, 192, 210, 194, 178)
Formulation_2=c(177, 197, 206, 201, 176, 185, 200, 197, 198, 188, 189, 203)
qqnorm(Formulation_1)
qqline(Formulation_1)
qqnorm(Formulation_2)
qqline(Formulation_2)
var.test(Formulation_1, Formulation_2)
n=length((Formulation_1))
qf(0.975,n-1,n-1)
qf(0.025,n-1,n-1)

t.test(Formulation_1,Formulation_2, var.equal=TRUE, c("greater"))
qt(0.975, n-1)



#2.5

qnorm(0.975)
pvalue <- 2*(1-pnorm(qnorm(0.975)))
pvalue
