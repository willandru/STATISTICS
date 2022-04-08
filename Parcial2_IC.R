rm(list=ls(all=TRUE))

base.datos <- read.table(file.choose(), header=T, sep=',')

#1
roman <- subset(base.datos , select = Nasal_height, subset =Period == "Roman_period", drop=TRUE )
early <- subset(base.datos , select = Nasal_height, subset =Period == "Early_predynastic", drop=TRUE )

var.test(roman, early ,  conf.level = 0.9 )
# IC 90% = [0.9729657 ,  3.3690097]

#2
late <- subset(base.datos , select = Basialveolar_length, subset =Period == "Late_predynastic", drop=TRUE )
ptoleomaico <- subset(base.datos , select = Basialveolar_length, subset =Period == "Ptolemaic_period", drop=TRUE )

var.test(late,ptoleomaico)
# [0.4264586 1.8824666] , COmo el IC para el cociente de varianzas incluye al 1, se asumen varianzas iguales.
t.test(late,ptoleomaico, var.equal = TRUE, conf.level = 0.93)
# IC 93% = [2.402388 ,  6.664279]

#3

din_12_13 <- subset(base.datos , select = Basibregmatic_height, subset =Period == "12th_and_13th_Dynasty", drop=TRUE )

lim_inf <- (length(din_12_13)-1)*var(din_12_13)/qchisq(0.925,length(din_12_13)-1)
lim_sup <- (length(din_12_13)-1)*var(din_12_13)/qchisq(0.075,length(din_12_13)-1)
cbind(lim_inf,lim_sup)
# IC 85% = [17.71635  ,  38.12661]


#4 
max.brea <- base.datos$Maximum_breadth

t.test(max.brea, conf.level=0.88)
# IC 88% = [133.3489 , 134.5978]

